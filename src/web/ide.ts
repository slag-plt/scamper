import { EditorView } from "@codemirror/view"

import OPFSFileSystem from './fs.js'
import Split from 'split.js'
import { Scamper } from '../scamper.js'
import { renderToOutput } from '../lpm/html-display.js'
import * as Lock from './lockfile.js'
import { mkFreshEditorState, mkNoFileEditorState } from "./codemirror.js"
import { initializeLibs } from "../lib/index.js"

const editorPane = document.getElementById('editor')!
const outputPane = document.getElementById('output')!

const configFilename = '.scamper.config'

type Config = {
  lastOpenedFilename: string | null
  lastVersionAccessed: string
}

const defaultConfig: Config = {
  lastOpenedFilename: null,
  lastVersionAccessed: '0.0.0'
}

class IDE {
  fs: OPFSFileSystem
  editor: EditorView
  currentFile: string | null
  autosaveId: number
  scamper?: Scamper
  isDirty: boolean
  config: Config
  isLoadingFile = false
  // N.B., showDotFiles should normally be false except when are debugging
  showDotFiles = false

  ///// Initialization /////////////////////////////////////////////////////////

  private constructor(fs: OPFSFileSystem) {
    this.fs = fs
    this.config = defaultConfig    // N.B., loaded asynchronously from disk in IDE.create()
    this.currentFile = null
    this.autosaveId = -1
    this.isDirty = false

    Split(['#editor', '#results'], { sizes: [65, 35] })
    this.editor = new EditorView({
      state: mkNoFileEditorState(outputPane),
      parent: editorPane!
    })

    document.getElementById('version')!.innerText = `(${APP_VERSION})`

    // N.B., this section is wonky. I'm trying to set up hooks to save on exit
    // but nothing is consistent! I think we eventually want to use just
    // visibilitychange and beforeunload. It seems like beforeunload is the
    // most consistent at the cost of killing bfcache... whicih I don't mind
    // for this particular application.
    document.addEventListener('visibilitychange', async () => {
      if (document.visibilityState === 'hidden') {
        await this.saveCurrentFile()
        await this.saveConfig()
        await Lock.releaseLockFile(this.fs)
      } else if (document.visibilityState === 'visible') {
        await Lock.acquireLockFile(this.fs)
      }
    })

    document.addEventListener('pagehide', async () => {
      await this.saveCurrentFile()
      await this.saveConfig()
      await Lock.releaseLockFile(this.fs)
    })

    window.addEventListener('beforeunload', async (e) => {
      // N.B., ensure the "are you sure" dialog pops
      await this.saveCurrentFile()
      await this.saveConfig()
      await Lock.releaseLockFile(this.fs)
      if (this.isDirty) {
        e.preventDefault()
        return true
      } else {
        return false
      }
    })

    this.initFileDropZone()
    this.initButtons()
  }

  /** Initializes the sidebar's dropzone */
  private initFileDropZone() {
    const sidebar = document.getElementById('sidebar')!

    sidebar.addEventListener('dragover', (event) => {
      event.preventDefault()
      event.dataTransfer!.dropEffect = 'copy'
      sidebar.classList.add('drag-over')
    })

    sidebar.addEventListener('dragleave', (event) => {
      event.preventDefault()
      sidebar.classList.remove('drag-over')
    })

    sidebar.addEventListener('drop', async (event) => {
      event.preventDefault()
      sidebar.classList.remove('drag-over')
      this.stopAutosaving()
      const files = event.dataTransfer?.files
      if (!files || files.length === 0) { return }

      for (let i = 0; i < files.length; i++) {
        const file = files[i]
        try {
          const content = await file.text()
          const filename = file.name

          // Check if file already exists
          if (await this.fs.fileExists(filename)) {
            const shouldOverwrite = confirm(`File "${filename}" already exists. Do you want to overwrite it?`)
            if (!shouldOverwrite) {
              continue
            }
            // Delete existing file
            this.stopAutosaving()
            await this.fs.deleteFile(filename)
          }

          await this.fs.saveFile(filename, content)
          this.currentFile = null
          await this.switchToFile(filename)
        } catch (e) {
          if (e instanceof Error) {
            this.displayError(`Failed to upload file "${file.name}": ${e.message}`)
          }
        }
      }
    })
  }

  private initButtons() {
    document.getElementById('toggle-sidebar')!.addEventListener('click', () => {
      const sidebar = document.getElementById('sidebar')!
      const isVisible = sidebar.style.display !== 'none'
      sidebar.style.display = isVisible ? 'none' : 'block'
    })

    ///// File Buttons ////////////////////////////////////////////////////

    document.getElementById('create-file')!.addEventListener('click', async () => {
      if (!this.fs) return
      const filename = prompt('Enter a file name for your new program.')
      if (filename !== null) {
        if (await this.fs.fileExists(filename)) {
          alert(`File ${filename} already exists!`)
        } else {
          await this.fs.saveFile(filename, `; ${filename}`)
          await this.switchToFile(filename)
        }
      }
    })

    document.getElementById('upload-file')!.addEventListener('click', async () => {
      const fileInput = document.getElementById('upload-file-input')! as HTMLInputElement
      fileInput.click()
    })

    document.getElementById('upload-file-input')!.addEventListener('change', async (event) => {
      if (!this.fs) { return }
      const target = event.target as HTMLInputElement
      const file = target.files?.[0]
      if (!file) { return }

      try {
        const content = await file.text()
        const filename = file.name

        // Check if file already exists
        if (await this.fs.fileExists(filename)) {
          const shouldOverwrite = confirm(`File "${filename}" already exists. Do you want to overwrite it?`)
          if (!shouldOverwrite) {
            target.value = '' // Reset the input
            return
          }
          // Delete existing file
          this.stopAutosaving()
          await this.fs.deleteFile(filename)
        }

        await this.fs.saveFile(filename, content)
        this.currentFile = null
        await this.switchToFile(filename)

        // Reset the file input for future uploads
        target.value = ''
      } catch (e) {
        if (e instanceof Error) {
          this.displayError(`Failed to upload file: ${e.message}`)
        }
        target.value = '' // Reset the input on error
      }
    })

    document.getElementById('download-archive')!.addEventListener('click', async () => {
      // TODO: implement logic for zipping all the files in the
      // file system as a backup
    })

    document.getElementById('rename-file')!.addEventListener('click', async () => {
      if (!this.fs) { return }
      if (!this.currentFile) { return }
      const newName = prompt(`Enter a new filename for ${this.currentFile}`)
      if (newName !== null && newName !== this.currentFile) {
        if (await this.fs.fileExists(newName)) {
          alert(`File ${newName} already exists!`)
        } else {
          try {
            this.stopAutosaving()
            // N.B., in renaming the file, the this.fs webworker will close
            // its handle to the current file, so we should be able to
            // just load it as if no file was open.
            await this.fs.renameFile(this.currentFile, newName)
            this.currentFile = null
            await this.switchToFile(newName)
          } catch (e) {
            if (e instanceof Error) {
              this.displayError(e.message)
            }
          }
        }
      }
    })

    document.getElementById('delete-file')!.addEventListener('click', async () => {
      if (!this.fs) return
      if (!this.currentFile) { return }
      const shouldDelete = confirm(`Are you sure you want to delete ${this.currentFile}?`)
      if (shouldDelete) {
        this.stopAutosaving()
        await this.fs.deleteFile(this.currentFile)

        // Remove the file from output in the IDE
        this.currentFile = null
        this.initializeDummyDoc()
        this.config.lastOpenedFilename = null
        outputPane!.innerHTML = ''

        this.populateFileDrawer()
        this.startAutosaving()
      }
    })

    document.getElementById('download-file')!.addEventListener('click', async () => {
      if (!this.fs) return
      if (!this.currentFile) { return }
      const contents = await this.fs.loadFile(this.currentFile)
      const hidden = document.createElement('a')
      hidden.href = 'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents)
      hidden.target = '_blank'
      hidden.download = this.currentFile
      hidden.click()
    })

    ///// Execution Buttons ////////////////////////////////////////////////////

    document.getElementById('run')!.addEventListener('click', () => {
      this.startScamper(false)
      this.scamper!.runProgram()
    })

    document.getElementById('run-window')!.addEventListener('click', async () => {
      if (!this.currentFile) { return }
      await this.saveCurrentFile()
      const params = new URLSearchParams({ filename: this.currentFile, isTree: "false" })
      window.open(`runner.html?${params.toString()}`)
    })

    document.getElementById('step')!.addEventListener('click', () => this.startScamper(true))

    document.getElementById('step-once')!.addEventListener('click', () => {
      this.scamper!.stepProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })

    document.getElementById('step-stmt')!.addEventListener('click', () => {
      this.scamper!.stepStmtProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })

    document.getElementById('step-all')!.addEventListener('click', () => {
      this.scamper!.runProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })

    document.getElementById('ast-text')!.addEventListener('click', () => {
      this.showASTText()
    })

    document.getElementById('ast-window')!.addEventListener('click', () => {
      if (!this.currentFile) { return }
      this.saveCurrentFile()
      const params = new URLSearchParams({ filename: this.currentFile, isTree: "true" })
      window.open(`runner.html?${params.toString()}`)
    })
    this.toggleStepButtons(false)
  }

  /** Toggles the step buttons on (true) or off (false). */
  private toggleStepButtons (enabled: boolean) {
    const stepOnce = document.getElementById('step-once')! as HTMLButtonElement
    const stepStmt = document.getElementById('step-stmt')! as HTMLButtonElement
    const stepAll = document.getElementById('step-all')! as HTMLButtonElement
    stepOnce.disabled = !enabled
    stepStmt.disabled = !enabled
    stepAll.disabled = !enabled
  }

  /** Populates the file drawer with entries. */
  private async populateFileDrawer() {
    if (!this.fs) {
      throw new Error('FileChooser: must call init() before usage')
    }

    const fileDrawer = document.getElementById('file-drawer')!
    // N.B., empty the container and repopulate from scratch
    fileDrawer.innerHTML = ''
    const files = await this.fs.getFileList()
    let tabIndex = 0
    for (const file of files) {
      if (!file.isDirectory && (this.showDotFiles || !file.name.startsWith('.'))) {
        const ret = document.createElement('div')
        ret.setAttribute('role', 'button')
        ret.setAttribute('aria-label', `Open ${file.name}`)
        ret.setAttribute('tabindex', (tabIndex++).toString())
        ret.classList.add('file')
        if (file.name === this.currentFile) {
          ret.classList.add('selected')
        }
        ret.textContent = file.name
        ret.addEventListener('click', async () => {
          // N.B., try to avoid double-clicking causing multiple file loads to occur at once
          if (!this.isLoadingFile) {
            await this.switchToFile(file.name)
          }
        })
        fileDrawer.appendChild(ret)
      }
    }
  }

  /** Saves the current configuration to disk */
  async saveConfig() {
    await this.fs.saveFile(configFilename, JSON.stringify(this.config))
  }

  /** Loads the configuration from disk */
  async loadConfig() {
    if (await this.fs.fileExists(configFilename)) {
      this.config = JSON.parse(await this.fs.loadFile(configFilename))
    } else {
      this.config = defaultConfig
      await this.saveConfig()
    }
  }

  static async create() {
    const fs = await OPFSFileSystem.create()
    await initializeLibs()
    const obtainedLock = await Lock.acquireLockFile(fs)
    if (!obtainedLock) {
      document.getElementById("loading-content")!.innerText = 'Another instance of Scamper is open. Please close that instance and try again.'
      document.getElementById("loading")!.style.display = "block"
    } else {
      const ide = new IDE(fs)
      await ide.populateFileDrawer()
      await ide.loadConfig()
      if (ide.config.lastOpenedFilename !== null) {
        if (await fs.fileExists(ide.config.lastOpenedFilename)) {
          await ide.switchToFile(ide.config.lastOpenedFilename)
        } else {
          ide.config.lastOpenedFilename = null
        }
      }
    }
  }

  ///// IDE utility functions //////////////////////////////////////////////////

  startAutosaving() {
    if (this.autosaveId === -1) {
      this.autosaveId = window.setInterval(async () => {
        await this.saveCurrentFile()
      }, 3000)
    }
  }

  stopAutosaving() {
    window.clearInterval(this.autosaveId)
    this.autosaveId = -1
  }

  getDoc(): string {
    return this.editor!.state.doc.toString()
  }

  initializeDoc (src: string) {
    this.editor!.setState(mkFreshEditorState(src, {
      output: outputPane,
      dirtyAction: () => this.makeDirty(),
      isReadOnly: false
    }))
  }

  initializeDummyDoc () {
    this.editor!.setState(mkNoFileEditorState(outputPane))
  }

  makeDirty() {
    if (this.scamper !== undefined && !this.isDirty) {
      this.isDirty = true
      // const status = document.getElementById('results-status')!
      const msg = document.createElement('em')
      msg.innerText = '(Warning: results out of sync with updated code)'
      document.getElementById('results-status')!.appendChild(msg)
    }
  }

  makeClean() {
    document.getElementById('results-status')!.innerHTML = ''
    this.isDirty = false
  }

  ///// IDE actions ////////////////////////////////////////////////////////////

  startScamper(isTracing: boolean): void {
    outputPane!.innerHTML = ''
    try {
      this.scamper = new Scamper(outputPane, this.getDoc())
      // TODO: reimplement once tracing is back in!
      isTracing = false
      if (isTracing) {
        this.toggleStepButtons(true)
      } else {
        this.toggleStepButtons(false)
      }
    } catch (e) {
      renderToOutput(outputPane, e)
    }
    this.makeClean()
  }

  showASTText(): void {
    const existing = document.getElementById('ast-output')
    const label = document.getElementById('ast-label')
    const desc = document.getElementById('ast-desc')
    if (existing) {
      existing.remove()
      if (label) {
        label.remove()
      }
      if (desc) {
        desc.remove()
      }
      return
    } try {
      // TODO: reimplement with new backend!
      // const parsed = Parser.parseProgram(this.getDoc())
      // const labelEl = document.createElement('h2')
      // labelEl.setAttribute('id', 'ast-label')
      // labelEl.innerText = "Abstract Syntax Tree"
      // labelEl.setAttribute('tabindex', '0')
      // labelEl.setAttribute('aria-label', 'Abstract Syntax Tree... Navigation instructions: use tab to traverse the tree in the order of node position on the code, or use "left/right" arrows for visiting neighbors, "down arrow" to visit children, and "up arrow" to go to parent')
      // outputPane!.appendChild(labelEl)
      // parsed.ast.render(outputPane, this.editor)
      // const descriptionEl = document.createElement('div')
      // descriptionEl.setAttribute('id', 'ast-desc')
      // descriptionEl.innerText = parsed.ast.describe()
      // descriptionEl.setAttribute('tabindex', '0')
      // descriptionEl.setAttribute('role', 'region')
      // outputPane!.appendChild(descriptionEl)
      // this.makeClean()
    } catch (e) {
      renderToOutput(outputPane, e)
    }
  }

  async saveCurrentFile() {
    if (!this.currentFile) { return }
    try {
      await this.fs.saveFile(this.currentFile, this.getDoc())
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  async switchToFile(filename: string): Promise<void> {
    this.isLoadingFile = true
    if (!this.fs) return
    // Stop autosaving to make the transaction atomic
    this.stopAutosaving()

    // Save the current file and close our handle to it, if open
    if (this.currentFile !== null) {
      await this.saveCurrentFile()
    }

    // Load the file!
    this.currentFile = filename
    try {
      const src = await this.fs.loadFile(this.currentFile)
      this.initializeDoc(src)
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(`${e.message}\n\n${e.stack}`)
      }
    }

    // Reset the UI: output panel and file drawer, also restart saving
    outputPane!.innerHTML = ''
    this.populateFileDrawer()
    this.config.lastOpenedFilename = this.currentFile
    this.startAutosaving()
    this.isLoadingFile = false
  }

  displayError(error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }
}

export const ide = await IDE.create()
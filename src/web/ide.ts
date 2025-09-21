import { basicSetup } from 'codemirror'
import { indentWithTab } from '@codemirror/commands'
import { EditorState } from "@codemirror/state"
import { EditorView, keymap } from "@codemirror/view"

import FS from './fs.js'
import Split from 'split.js'
import { Scamper } from '../scamper.js'
import { renderToOutput } from '../display.js'
import { ScamperSupport } from '../codemirror/language.js'
import makeScamperLinter from '../codemirror/linter.js'
import { indentSelection } from "@codemirror/commands"
import * as Lock from './lockfile.js'

const editorPane      = document.getElementById('editor')!
const outputPane      = document.getElementById('output')!
const runButton       = document.getElementById('run')!
const runWindowButton = document.getElementById('run-window')!
const astWindowButton = document.getElementById('ast-window')!
const stepButton      = document.getElementById('step')!

const createFileButton = document.getElementById('create-file')! as HTMLButtonElement
const uploadFileButton = document.getElementById('upload-file')! as HTMLButtonElement
const downloadArchiveButton = document.getElementById('download-archive')! as HTMLButtonElement
const renameFileButton = document.getElementById('rename-file')! as HTMLButtonElement
const deleteFileButton = document.getElementById('delete-file')! as HTMLButtonElement
const downloadFileButton = document.getElementById('download-file')! as HTMLButtonElement

const stepOnceButton = document.getElementById('step-once')! as HTMLButtonElement
const stepStmtButton = document.getElementById('step-stmt')! as HTMLButtonElement
const stepAllButton  = document.getElementById('step-all')! as HTMLButtonElement
const astTextButton = document.getElementById('ast-text')! as HTMLButtonElement

const noLoadedFileText = '; Create and/or load a file from the left-hand sidebar!'
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
  fs: FS
  editor: EditorView
  currentFile: string | null
  autosaveId: number
  scamper?: Scamper
  isDirty: boolean
  config: Config
  isLoadingFile: boolean = false

  ///// Initialization /////////////////////////////////////////////////////////

  mkEditorState (doc: string, isReadOnly: boolean = false): EditorState {
    return EditorState.create({
      doc,
      extensions: [
        basicSetup,
        EditorState.readOnly.of(isReadOnly),
        keymap.of([
          indentWithTab,
          {
            key: "Ctrl-Shift-i",
            run: (view) => {
              const doc = view.state.doc
              const sel = view.state.selection.main
              const line = doc.lineAt(sel.head)
              const visualColumn = sel.head - line.from
              view.dispatch({
                selection: { anchor: 0, head: doc.length }
              })     
              const success = indentSelection(view)
              const updatedLine = view.state.doc.line(line.number)
              const nonWhitespacePrefix = updatedLine.text.match(/^\s*/)?.[0].length || 0         
              const newHead = Math.min(updatedLine.from + nonWhitespacePrefix + visualColumn, updatedLine.to)
              view.dispatch({
                selection: { anchor: newHead },
                scrollIntoView: true
              })
              return success
            }
          },
          {
            key: "'",
            run: (view) => {
              const { from, to } = view.state.selection.main
              view.dispatch({
                changes: { from, to, insert: "'" },
                selection: { anchor: from + 1 }
              })
              return true
            }
          }
          ]),
        ScamperSupport(),
        makeScamperLinter(outputPane),
        EditorView.updateListener.of((update) => {
          if (update.docChanged) {
            this.makeDirty()
          }
        })
      ]
    })
  }

  private constructor (fs: FS) {
    this.fs = fs
    this.config = defaultConfig    // N.B., loaded asynchronously from disk in IDE.create()
    this.currentFile = null
    this.autosaveId = -1
    this.isDirty = false

    Split(['#editor', '#results'], { sizes: [65, 35] })
    this.editor = new EditorView({
      state: this.mkEditorState(noLoadedFileText, true),
      parent: editorPane!
    })

    document.getElementById('version')!.innerText = `(${APP_VERSION})`
    window.addEventListener('beforeunload', async (_e) => {
      await this.saveCurrentFile()
      await this.saveConfig()
      await Lock.releaseLockFile(this.fs)
    })

    this.initButtons()
    this.initFileDropZone()

    this.loadConfig()
  }

  private initFileDropZone () {
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
      
      if (!this.fs) { return }
      
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

  private initButtons () {
    document.getElementById('toggle-sidebar')!.addEventListener('click', () => {
      const sidebar = document.getElementById('sidebar')!
      const isVisible = sidebar.style.display !== 'none'
      sidebar.style.display = isVisible ? 'none' : 'block'
    })
    createFileButton.addEventListener('click', async () => {
      if (!this.fs) return
      const filename = prompt('Enter a file name for your new program.')
      if (filename !== null) {
        if (await this.fs.fileExists(filename)) {
          alert(`File ${filename} already exists!`)
        } else {
          await this.fs.saveFile(filename, `; ${filename}`)
          this.switchToFile(filename)
        }
      }
    })
    uploadFileButton.addEventListener('click', async () => {
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
    downloadArchiveButton.addEventListener('click', async () => {
      // TODO: implement logic for zipping all the files in the
      // file system as a backup
    })
    renameFileButton.addEventListener('click', async () => {
      if (!this.fs) { return }
      if (!this.currentFile) { return }
      const newName = prompt(`Enter a new filename for ${this.currentFile}`)
      if (newName !== null && newName !== this.currentFile) {
        if (await this.fs.fileExists(newName)) {
          alert(`File ${newName} already exists!`)
        } else {
          try {
            this.stopAutosaving()
            // N.B., in renaming the file, the FS webworker will close
            // its handle to the current file, so we should be able to
            // just load it as if no file was open.
            await this.fs.renameFile(this.currentFile, newName)
            this.currentFile = null
            this.switchToFile(newName)
          } catch (e) {
            if (e instanceof Error) {
              this.displayError(e.message)
            }
          }
        }
      }
    })
    deleteFileButton.addEventListener('click', async () => {
      if (!this.fs) return
      if (!this.currentFile) { return }
      const shouldDelete = confirm(`Are you sure you want to delete ${this.currentFile}?`)
      if (shouldDelete) {
        this.stopAutosaving()
        await this.fs.deleteFile(this.currentFile)

        // Remove the file from output in the IDE
        this.currentFile = null
        this.initializeDoc(noLoadedFileText, true)
        this.config.lastOpenedFilename = null
        outputPane!.innerHTML = ''
        
        this.populateFileDrawer()
        this.startAutosaving()
      }
    })
    downloadFileButton.addEventListener('click', async () => {
      if (!this.fs) return
      if (!this.currentFile) { return }
      const contents = await this.fs.loadFile(this.currentFile)
      const hidden = document.createElement('a')
      hidden.href = 'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents)
      hidden.target = '_blank'
      hidden.download = this.currentFile
      hidden.click()
    })
    runButton.addEventListener('click', () => {
      this.startScamper(false)
      this.scamper!.runProgram()
    })
    runWindowButton.addEventListener('click', async () => {
      if (!this.currentFile) { return }
      await this.saveCurrentFile()
      const params = new URLSearchParams({ filename: this.currentFile, isTree: "false" })
      window.open(`runner.html?${params.toString()}`)
    })
    stepButton.addEventListener('click', () => this.startScamper(true))
    stepOnceButton.addEventListener('click', () => {
      this.scamper!.stepProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })
    stepStmtButton.addEventListener('click', () => {
      this.scamper!.stepStmtProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })
    stepAllButton.addEventListener('click', () => {
      this.scamper!.runProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })
    astTextButton.addEventListener('click', () => {
      this.showASTText()
    })
    astWindowButton.addEventListener('click', () => {
      if (!this.currentFile) { return }
      this.saveCurrentFile()
      const params = new URLSearchParams({filename: this.currentFile, isTree: "true"})
      window.open(`runner.html?${params.toString()}`)
    })
    
    stepOnceButton.disabled = true
    stepStmtButton.disabled = true
    stepAllButton.disabled = true
  }

  private async populateFileDrawer () {
    if (!this.fs) {
      throw new Error('FileChooser: must call init() before usage')
    }

    const fileDrawer = document.getElementById('file-drawer')!
    // N.B., empty the container and repopulate from scratch
    fileDrawer.innerHTML = ''
    const files = await this.fs.getFileList()
    let tabIndex = 0
    for (const file of files) {
      if (!file.isDirectory) {
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

  async saveConfig () {
    await this.fs.saveFile(configFilename, JSON.stringify(this.config)/*, true*/)
  }

  async loadConfig () {
    if (await this.fs.fileExists(configFilename)) {
      this.config = JSON.parse(await this.fs.loadFile(configFilename/*, true*/))
    } else {
      this.config = defaultConfig 
      await this.saveConfig()
    }
    if (this.config.lastOpenedFilename !== null) {
      if (await this.fs.fileExists(this.config.lastOpenedFilename)) {
        this.switchToFile(this.config.lastOpenedFilename)
      } else {
        this.config.lastOpenedFilename = null
      }
    }
  }

  static async create () {
    const fs = await FS.create()
    const obtainedLock = await Lock.acquireLockFile(fs)
    if (!obtainedLock) {
      document.getElementById("loading-content")!.innerText = 'Another instance of Scamper is open. Please close that instance and try again.'
      document.getElementById("loading")!.style.display = "block"
    } else {
      const ide = new IDE(fs)
      await ide.populateFileDrawer()
      await ide.loadConfig()
    }
  }

  ///// IDE utility functions //////////////////////////////////////////////////

  startAutosaving () {
    if (this.autosaveId === -1) {
      this.autosaveId = window.setInterval(async () => await this.saveCurrentFile(), 3000)
    }
  }

  stopAutosaving () {
    window.clearInterval(this.autosaveId)
    this.autosaveId = -1
  }

  getDoc (): string {
    return this.editor!.state.doc.toString()
  }

  initializeDoc (src: string, isReadOnly: boolean = false) {
    this.editor!.setState(this.mkEditorState(src ,isReadOnly))
  }

  makeDirty () {
    if (this.scamper !== undefined && !this.isDirty) {
      this.isDirty = true
      // const status = document.getElementById('results-status')!
      const msg = document.createElement('em')
      msg.innerText = '(Warning: results out of sync with updated code)'
      document.getElementById('results-status')!.appendChild(msg)
    }
  }

  makeClean () {
    document.getElementById('results-status')!.innerHTML = ''
    this.isDirty = false
  }

  ///// IDE actions ////////////////////////////////////////////////////////////

  startScamper (isTracing: boolean): void {
    outputPane!.innerHTML = ''
    try {
      this.scamper = new Scamper(outputPane, this.getDoc())
      // TODO: reimplement once tracing is back in!
      isTracing = false
      if (isTracing) {
        stepOnceButton.disabled = false
        stepStmtButton.disabled = false
        stepAllButton.disabled = false
      } else {
        stepOnceButton.disabled = true
        stepStmtButton.disabled = true
        stepAllButton.disabled = true
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

  async saveCurrentFile () {
    if (!this.fs || !this.currentFile) return
    try {
      await this.fs.saveFile(this.currentFile, this.getDoc())
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  async switchToFile (filename: string): Promise<void> {
    this.isLoadingFile = true
    if (!this.fs) return
    // Stop autosaving to make the transaction atomic
    this.stopAutosaving()

    // Save the current file and close our handle to it, if open
    if (this.currentFile) {
      await this.saveCurrentFile()
    }

    // Load the file!
    this.currentFile = filename
    try {
      const src = await this.fs.loadFile(this.currentFile)
      console.log(`Loading file ${filename}: ${src.slice(0, 10)}...`)
      this.initializeDoc(src)
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
   
    // Reset the UI: output panel and file drawer, also restart saving
    outputPane!.innerHTML = ''
    this.populateFileDrawer()
    this.config.lastOpenedFilename = this.currentFile
    this.startAutosaving()
    this.isLoadingFile = false
  }

  displayError (error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }
}

export const ide = await IDE.create()
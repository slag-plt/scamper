import { basicSetup } from 'codemirror'
import { indentWithTab } from '@codemirror/commands'
import { EditorView, keymap } from "@codemirror/view"

import FS from './fs/fs.js'
import Split from 'split.js'
import { Scamper } from '../scamper.js'
import { renderToOutput } from '../display.js'
import { ScamperSupport } from '../codemirror/language.js'
import makeScamperLinter from '../codemirror/linter.js'
import { indentSelection } from "@codemirror/commands"

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


class IDE {
  fs?: FS
  editor: EditorView
  currentFile: string
  autosaveId: number
  scamper?: Scamper
  isDirty: boolean

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

  constructor () {
    this.editor = new EditorView({
      doc: '',
      extensions: [
        basicSetup,
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
      ], parent: editorPane!
    })
    this.autosaveId = -1
    this.isDirty = false

    const params = new URLSearchParams(window.location.search)
    if (params.has('filename')) {
      this.currentFile = params.get('filename')!
    } else {
      // TODO: probably can delete this at some point---we'll (probably)
      // never invoke the IDE without a filename parameter.
      this.currentFile = new FS().getLastOpened()
    }

    createFileButton.addEventListener('click', async () => {
      if (!this.fs) return
      const filename = prompt('Enter a file name for your new program.')
      if (filename !== null) {
        if (await this.fs.fileExists(filename)) {
          alert(`File ${filename} already exists!`)
        } else {
          await this.fs.saveFile(filename, `; ${filename}`)
          this.populateFileDrawer()
        }
      }
    })
    uploadFileButton.addEventListener('click', async () => {
      // TODO: maybe make this a file input button?
    })
    downloadArchiveButton.addEventListener('click', async () => {
      // TODO: implement logic for zipping all the files in the
      // file system as a backup
    })
    renameFileButton.addEventListener('click', async () => {
      if (!this.fs) return
      const newName = prompt(`Enter a new filename for ${this.currentFile}`)
      if (newName !== null && newName !== this.currentFile) {
        if (await this.fs.fileExists(newName)) {
          alert(`File ${newName} already exists!`)
        } else {
          try {
            // N.B., in renaming the file, the FS webworker will close
            // its handle to the current file, so we should be able to
            // just load it as if no file was open.
            await this.fs.renameFile(this.currentFile, newName)
            this.currentFile = newName
            this.loadCurrentFile()
            this.populateFileDrawer()
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
      const shouldDelete = confirm(`Are you sure you want to delete ${this.currentFile}?`)
      if (shouldDelete) {
        await this.fs.closeFile(this.currentFile)
        await this.fs.deleteFile(this.currentFile)
        // this.currentFile = undefined
        this.populateFileDrawer()
      }
    })
    downloadFileButton.addEventListener('click', async () => {
      if (!this.fs) return
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
      this.saveCurrentFile()
      const params = new URLSearchParams({filename: this.currentFile, isTree: "true"})
      window.open(`runner.html?${params.toString()}`)
    })
    
    stepOnceButton.disabled = true
    stepStmtButton.disabled = true
    stepAllButton.disabled = true

    Split(['#editor', '#results'], {
      sizes: [65, 35]
    })

    document.getElementById('version')!.innerText = `(${APP_VERSION})`

    window.addEventListener('beforeunload', async (_e) => {
      await this.saveCurrentFile()
    })
  }

  async init (): Promise<void> {
    this.fs = await FS.create()
    await this.loadCurrentFile()
    await this.populateFileDrawer()
    this.startAutosaving()
  }

  static async create (): Promise<IDE> {
    const ide = new IDE()
    await ide.init()
    return ide
  }

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

  setDoc (src: string) {
    this.editor.dispatch(this.editor.state.update({
      changes: { from: 0, to: this.editor.state.doc.length, insert: src }
    }))
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

  async saveCurrentFile () {
    if (!this.fs) return
    try {
      await this.fs.saveFile(this.currentFile, this.getDoc(), true)
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  async loadCurrentFile () {
    if (!this.fs) return
    const currentFileLabel = document.getElementById('current-file')!
    currentFileLabel.innerText = this.currentFile
    try {
      const src = await this.fs.loadFile(this.currentFile, true)
      this.setDoc(src)
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  async switchToFile (filename: string): Promise<void> {
    if (!this.fs) return
    // Stop autosaving to make this transaction atomic
    this.stopAutosaving()

    // Save the current file and close our handle to it
    await this.saveCurrentFile()
    await this.fs.closeFile(this.currentFile)

    // Do the load!
    this.currentFile = filename
    await this.loadCurrentFile()
   
    // Clear the output pane and restart autosaving.
    outputPane!.innerHTML = ''
    this.startAutosaving()
  }

  displayError (error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }

  async populateFileDrawer () {
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
          await this.switchToFile(file.name)
          // N.B., maybe we should only redraw what is necessary versus
          // re-rendering the entire file drawer...
          await this.populateFileDrawer()
        })
        fileDrawer.appendChild(ret)
      }
    }
  }
}

await IDE.create()
import { basicSetup } from 'codemirror'
import { indentWithTab } from '@codemirror/commands'
import { EditorView, keymap } from "@codemirror/view"

import FS from './fs/fs.js'
import Split from 'split.js'
import { Scamper, mkOptions } from '../scamper.js'
import { renderToOutput } from '../display.js'
import { ScamperSupport } from '../codemirror/language.js'
import makeScamperLinter from '../codemirror/linter.js'

const editorPane      = document.getElementById('editor')!
const outputPane      = document.getElementById('output')!
const runButton       = document.getElementById('run')!
const runWindowButton = document.getElementById('run-window')!
const stepButton      = document.getElementById('step')!

const stepOnceButton = document.getElementById('step-once')! as HTMLButtonElement
const stepStmtButton = document.getElementById('step-stmt')! as HTMLButtonElement
const stepAllButton  = document.getElementById('step-all')! as HTMLButtonElement

class IDE {
  fs?: FS
  editor: EditorView
  currentFile: string
  autosaveId: number
  scamper?: Scamper
  isDirty: boolean

  startScamper (isTracing: boolean): void {
    outputPane!.innerHTML = ''
    const opts = mkOptions()
    opts.isTracing = isTracing
    try {
      this.scamper = new Scamper(outputPane, this.getDoc(), opts)
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

  constructor () {
    this.editor = new EditorView({
      doc: '',
      extensions: [
        basicSetup,
        keymap.of([indentWithTab]),
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

    runButton.addEventListener('click', () => {
      this.startScamper(false)
      this.scamper!.runProgram()
    })
    runWindowButton.addEventListener('click', async () => {
      await this.saveCurrentFile()
      const params = new URLSearchParams({ filename: this.currentFile })
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

  // stopAutosaving () {
  //   window.clearInterval(this.autosaveId)
  //   this.autosaveId = -1
  // }

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
      this.setDoc(await this.fs.loadFile(this.currentFile, true))
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  displayError (error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }
}

await IDE.create()
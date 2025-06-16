import { basicSetup } from 'codemirror'
import { indentWithTab } from '@codemirror/commands'
import { EditorView, keymap } from "@codemirror/view"

import FS from './fs.js'
import Split from 'split.js'
import { Scamper, mkOptions } from '../scamper.js'
import { renderToOutput } from '../display.js'
import { ScamperSupport } from '../codemirror/language.js'
import makeScamperLinter from '../codemirror/linter.js'
import { indentSelection } from "@codemirror/commands"

const editorPane      = document.getElementById('editor')!
const outputPane      = document.getElementById('output')!
const runButton       = document.getElementById('run')!
const runWindowButton = document.getElementById('run-window')!
const stepButton      = document.getElementById('step')!

const stepOnceButton = document.getElementById('step-once')! as HTMLButtonElement
const stepStmtButton = document.getElementById('step-stmt')! as HTMLButtonElement
const stepAllButton  = document.getElementById('step-all')! as HTMLButtonElement
const astTextButton = document.getElementById('ast-text')! as HTMLButtonElement


class IDE {
  fs: FS
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

  // Converts the editor source to an AST and renders it as ASCII.
  showASTText(): void {
     ///
  }

  constructor () {
    this.fs = new FS()
    this.editor = new EditorView({
      doc: '',
      extensions: [
        basicSetup,
        keymap.of([indentWithTab,
          {
            key: "Ctrl-i",
            run: (view) => {
              const allPage = view.state.doc
              view.dispatch({
                selection: {
                  anchor: 0,
                  head: allPage.length
                }
              })
              return indentSelection(view)
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

    const params = new URLSearchParams(window.location.search);
    if (params.has('filename')) {
      this.currentFile = params.get('filename')!
    } else {
      // TODO: probably can delete this at some point---we'll (probably)
      // never invoke the IDE without a filename parameter.
      this.currentFile = this.fs.getLastOpened()
    }
    this.loadCurrentFile()

    runButton.addEventListener('click', () => {
      this.startScamper(false)
      this.scamper!.runProgram()
    })
    runWindowButton.addEventListener('click', () => {
      this.saveCurrentFile()
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
    astTextButton.addEventListener('click', () => {
      this.showASTText()
    })
    
    stepOnceButton.disabled = true
    stepStmtButton.disabled = true
    stepAllButton.disabled = true

    Split(['#editor', '#results'], {
      sizes: [65, 35]
    })

    document.getElementById('version')!.innerText = `(${APP_VERSION})`

    window.addEventListener('beforeunload', (_e) => {
      this.saveCurrentFile()
    })

    this.startAutosaving()
  }

  startAutosaving () {
    if (this.autosaveId === -1) {
      this.autosaveId = window.setInterval(() => this.saveCurrentFile(), 3000)
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

  saveCurrentFile () {
    this.fs.saveFile(this.currentFile, this.getDoc())
  }

  loadCurrentFile () {
    const currentFileLabel = document.getElementById('current-file')!
    currentFileLabel.innerText = this.currentFile
    this.setDoc(this.fs.loadFile(this.currentFile))
  }
}

new IDE()

// ├── and └── 
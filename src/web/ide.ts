import { minimalSetup, EditorView } from 'codemirror'
import * as Scamper from '../scamper.js'

let editor: EditorView | null = null
const editorPane = document.getElementById('editor')
const outputPane = document.getElementById('output')
const runButton  = document.getElementById('run')

function initialize() {
  editor = new EditorView({
    doc: '(render "Hello World!")',
    extensions: [
      minimalSetup,
    ],
    parent: editorPane!
  })

  runButton!.addEventListener('click', () => {
    outputPane!.innerHTML = ''
    const program = editor!.state.doc.toString()
    Scamper.runProgram(Scamper.makeInitialEnv('output'), program)
  })
}

export default initialize
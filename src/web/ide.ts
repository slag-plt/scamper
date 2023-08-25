import { basicSetup, EditorView } from 'codemirror'
import Scamper from '../scamper.js'

let editor: EditorView | null = null
const editorPane = document.getElementById('editor')
const outputPane = document.getElementById('output')
const runButton  = document.getElementById('run')

function initialize() {
  editor = new EditorView({
    doc: `
(import image)

(define triforce
  (lambda (n sz)
    (if (= n 0)
        (overlay (triangle sz "outline" "black")
                 (triangle sz "solid" "yellow"))
        (let ([sub (triforce (- n 1) (/ sz 2))])
             (above sub (beside sub sub))))))

(display (triforce 5 500))
    `.trim(),
    extensions: [
      basicSetup,
    ],
    parent: editorPane!
  })

  runButton!.addEventListener('click', () => {
    const scamper = new Scamper('output')
    outputPane!.innerHTML = ''
    const program = editor!.state.doc.toString()
    scamper.runProgram(program)
  })
}

export default initialize
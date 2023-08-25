import { basicSetup, EditorView } from 'codemirror'
import Split from 'split.js'
import Scamper from '../scamper.js'

let editor: EditorView | null = null
const editorPane = document.getElementById('editor')
const outputPane = document.getElementById('output')
const runButton  = document.getElementById('run')

// https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
function storageAvailable(type: any) {
  let storage;
  try {
    storage = window[type]
    const x = '__storage_test__';
    (storage as any).setItem(x, x)
    (storage as any).removeItem(x)
    return true
  } catch (e) {
    return (
      e instanceof DOMException &&
      // everything except Firefox
      (e.code === 22 ||
        // Firefox
        e.code === 1014 ||
        // test name field too, because code might not be present
        // everything except Firefox
        e.name === "QuotaExceededError" ||
        // Firefox
        e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    )
  }
}

// N.B., local files are a simple object mapping filenames to (string) contents
let files: object = {}
let isStorageAvailable = storageAvailable('localStorage')
let currentFile: string | null = null

function initializeStorage() {
  if (isStorageAvailable) {
    if (localStorage.getItem('scamper-files') !== null) {
      files = JSON.parse(localStorage.getItem('scamper-files')!) as object
    }
    if (localStorage.getItem('scamper-last-opened-file') !== null) {
      currentFile = localStorage.getItem('scamper-last-current-file')!
    }
  }
}

function commitStorage() {
  if (isStorageAvailable) {
    localStorage.setItem('scamper-files', JSON.stringify(files))
    localStorage.setItem('scamper-last-opened-file', currentFile!)
  }
}

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

  Split(['#editor', '#results'], {
    sizes: [50, 50]
  })
  Split(['#output', '#interactions'], {
    sizes: [80, 20],
    direction: 'vertical',
  })
}

export default initialize
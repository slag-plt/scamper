import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as Display from '../../display.js'
import FS from '../../web/fs.js'

export const lib: L.Library = new L.Library()
export default lib

///// Reactive file ////////////////////////////////////////////////////////////

interface ReactiveFile extends L.Struct {
  [L.structKind]: 'reactive-file',
  filename: string,
  callback: L.ScamperFn
}

function withFile (filename: string, callback: L.ScamperFn): ReactiveFile {
  checkContract(arguments, contract('with-file', [C.string, C.func]))  
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file',
    filename,
    callback
  }
}
lib.registerValue('with-file', withFile)

function renderReactiveFile (v: any): HTMLElement {
  const rf = v as ReactiveFile
  const ret = document.createElement('code')
  FS.fileExists(rf.filename).then((exists) => {
    if (!exists) {
      const err = new L.ScamperError('Runtime', `File not found: ${rf.filename}`)
      ret.appendChild(Display.renderToHTML(err))
    } else {
      FS.loadFile(rf.filename).then((data) => {
        ret.innerHTML = ''
        try {
          const v = L.callScamperFn(rf.callback, data)
          ret.appendChild(Display.renderToHTML(v))
        } catch (e) {
          ret.appendChild(Display.renderToHTML(e as L.ScamperError))
        }
      })
    }
  })
  return ret
}

Display.addCustomWebRenderer(
  (v) => L.isStructKind(v, 'reactive-file'), renderReactiveFile)

///// Reactive file chooser ////////////////////////////////////////////////////

interface ReactiveFileChooser extends L.Struct {
  [L.structKind]: 'reactive-file-chooser',
  callback: L.ScamperFn
}

function withFileChooser (callback: L.ScamperFn): ReactiveFileChooser {
  checkContract(arguments, contract('with-file-chooser', [C.func]))  
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file-chooser',
    callback
  }
}
lib.registerValue('with-file-chooser', withFileChooser)

function renderReactiveFileChooser (v: any): HTMLElement {
  const rf = v as ReactiveFileChooser
  const ret = document.createElement('code')
  const inp = document.createElement('input')
  const outp = document.createElement('code')
  inp.type = 'file'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
        outp.innerHTML = ''
        try {
          const v = L.callScamperFn(rf.callback, e.target.result as string)
          outp.appendChild(Display.renderToHTML(v))
        } catch (e) {
          outp.appendChild(Display.renderToHTML(e as L.ScamperError))
        }
      } else {
        outp.innerText = ''
      }
    }
    if (inp.files !== null && inp.files.length > 0) {
      outp.innerText = 'Loading...'
      reader.readAsText(inp.files[0])
    }
  }, false)

  ret.appendChild(inp)
  ret.appendChild(document.createElement('br'))
  ret.appendChild(outp)
  return ret
}

Display.addCustomWebRenderer(
  (v) => L.isStructKind(v, 'reactive-file-chooser'), renderReactiveFileChooser)
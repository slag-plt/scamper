import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import HTMLRenderer from '../../lpm/renderers/html-renderer'
import OPFSFileSystem from '../../web/fs.js'

let fs: OPFSFileSystem | null = null

export const lib: L.Library = new L.Library(async () => {
  if (navigator.storage !== undefined) {
    fs = await OPFSFileSystem.create()
  }
})
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
  const ret = document.createElement('div')
  if (!fs) {
    ret.innerText = 'OPFS not supported'
    return ret
  }
  fs.fileExists(rf.filename).then((exists: boolean) => {
    if (!exists) {
      const err = new L.ScamperError('Runtime', `File not found: ${rf.filename}`)
      ret.appendChild(HTMLRenderer.render(err))
    } else {
      fs!.loadFile(rf.filename).then((data: string) => {
        ret.innerHTML = ''
        try {
          const v = L.callScamperFn(rf.callback, data)
          ret.appendChild(HTMLRenderer.render(v))
        } catch (e) {
          ret.appendChild(HTMLRenderer.render(e as L.ScamperError))
        }
      })
    }
  })
  return ret
}

HTMLRenderer.registerCustomRenderer(
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
  const ret = document.createElement('div')
  const inp = document.createElement('input')
  const outp = document.createElement('div')
  inp.type = 'file'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
        outp.innerHTML = ''
        try {
          const v = L.callScamperFn(rf.callback, e.target.result as string)
          outp.appendChild(HTMLRenderer.render(v))
        } catch (e) {
          outp.appendChild(HTMLRenderer.render(e as L.ScamperError))
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

HTMLRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file-chooser'), renderReactiveFileChooser)
import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import HTMLRenderer from '../../lpm/renderers/html'
import OPFSFileSystem from '../../web/fs.js'
import { SubthreadErrors } from "../../lpm";

let fs: OPFSFileSystem | null = null

export const lib: L.Library = new L.Library(async () => {
  fs = await OPFSFileSystem.create()
})
export default lib

///// Reactive file ////////////////////////////////////////////////////////////

interface ReactiveFile extends L.Struct {
  [L.structKind]: 'reactive-file',
  filename: string,
  callback: L.ScamperFn
}

function withFile (...args: [string, L.ScamperFn]): ReactiveFile {
  checkContract(args, contract('with-file', [C.string, C.func]))
  const [filename, callback] = args
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file',
    filename,
    callback
  }
}
lib.registerValue('with-file', withFile)

function renderReactiveFile (v: L.Value): HTMLElement {
  const rf = v as ReactiveFile
  const ret = document.createElement('div')
  const currentFs = fs

  if (currentFs === null) {
    ret.innerText = 'OPFS not supported'
    return ret
  }

  const loadReactiveFile = async (): Promise<void> => {
    const exists = await currentFs.fileExists(rf.filename)
    if (!exists) {
      const err = new L.ScamperError('Runtime', `File not found: ${rf.filename}`)
      ret.appendChild(HTMLRenderer.render(err))
      return
    }

    const data = await currentFs.loadFile(rf.filename)
    ret.innerHTML = ''
    try {
      const value = L.callScamperFn(rf.callback, data)
      ret.appendChild(HTMLRenderer.render(value))
    } catch (e) {
      if (e instanceof SubthreadErrors) {
        for (const err of e.errors) {
          ret.appendChild(HTMLRenderer.render(err))
        }
      } else {
        ret.appendChild(HTMLRenderer.render(e as L.ScamperError))
      }
    }
  }

  void loadReactiveFile()
  return ret
}

HTMLRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file'), renderReactiveFile)

///// Reactive file chooser ////////////////////////////////////////////////////

interface ReactiveFileChooser extends L.Struct {
  [L.structKind]: 'reactive-file-chooser',
  callback: L.ScamperFn
}

function withFileChooser (...args: [L.ScamperFn]): ReactiveFileChooser {
  checkContract(args, contract('with-file-chooser', [C.func]))
  const [callback] = args
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file-chooser',
    callback
  }
}
lib.registerValue('with-file-chooser', withFileChooser)

function renderReactiveFileChooser (v: L.Value): HTMLElement {
  const rf = v as ReactiveFileChooser
  const ret = document.createElement('div')
  const inp = document.createElement('input')
  const outp = document.createElement('div')
  inp.type = 'file'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e.target !== null) {
        outp.innerHTML = ''
        try {
          const value = L.callScamperFn(rf.callback, e.target.result as string)
          outp.appendChild(HTMLRenderer.render(value))
        } catch (err) {
          outp.appendChild(HTMLRenderer.render(err as L.ScamperError))
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
import * as L from '../../../lpm'
import { getFS } from '../../../fs'
import HTMLRenderer from '../../../lpm/renderers/html.js'
import { SubthreadErrors } from '../../../lpm'
import { ReactiveFile, ReactiveFileChooser } from '../files.js'

///// Reactive file ////////////////////////////////////////////////////////////

function renderReactiveFile (v: any): HTMLElement {
  const rf = v as ReactiveFile
  const ret = document.createElement('div')
  const fs = getFS()
  fs.fileExists(rf.filename).then((exists: boolean) => {
    if (!exists) {
      const err = new L.ScamperError('Runtime', `File not found: ${rf.filename}`)
      ret.appendChild(HTMLRenderer.render(err))
    } else {
      fs.loadFile(rf.filename).then((data: string) => {
        ret.innerHTML = ''
        try {
          const v = L.callScamperFn(rf.callback, data)
          ret.appendChild(HTMLRenderer.render(v))
        } catch (e) {
          if (e instanceof SubthreadErrors) {
            for (const err of e.errors) {
              ret.appendChild(HTMLRenderer.render(err))
            }
          } else {
            ret.appendChild(HTMLRenderer.render(e as L.ScamperError))
          }
        }
      },
      (_err: unknown) => {
        ret.innerText = `Unknown error while loading file ${rf.filename}`
      })
    }
  },
  (_err: unknown) => {
    ret.innerText = `Unknown error while loading file ${rf.filename}`
  })
  return ret
}

HTMLRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file'), renderReactiveFile)

///// Reactive file chooser ////////////////////////////////////////////////////

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

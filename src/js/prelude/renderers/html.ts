import * as L from '../../../lpm'
import HTMLRenderer from '../../../lpm/renderers/html.js'
import { ReactiveFileChooser } from '../files.js'

///// Reactive file chooser ////////////////////////////////////////////////////

function renderReactiveFileChooser (v: any): HTMLElement {
  // Captured while the program is stepping (rendering happens as its output is
  // emitted); the callback below fires from a FileReader afterwards (#375).
  const run = L.currentRun()
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
        // Run the callback as a fiber (JS can no longer call the closure) and
        // render its result; a callback error surfaces in the output pane.
        run.spawn(rf.callback, [e.target.result as string], (r) => {
          if (r !== null) {
            outp.appendChild(HTMLRenderer.render(r))
          }
        })
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

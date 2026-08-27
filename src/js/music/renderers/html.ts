import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Composition, music_compositionQ, music_playComposition } from '../index.js'
import { requireWaf } from '../webaudiofont/webaudiofont.js'

function render (v: L.Value): HTMLElement {
  const composition: Composition = v as Composition
  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    if (requireWaf().audioContext.state === 'suspended') {
      requireWaf().audioContext.resume().catch(console.error)
    }
    timer = music_playComposition(composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
      requireWaf().player.cancelQueue(requireWaf().audioContext)
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}

HtmlRenderer.registerCustomRenderer(music_compositionQ, render)

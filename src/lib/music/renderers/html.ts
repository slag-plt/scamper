import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Composition, compositionQ, playComposition } from '../index.js'
import { waf } from '../webaudiofont/webaudiofont.js'

function render (v: any): HTMLElement {
  const composition: Composition = v as Composition
  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    if (waf()!.audioContext.state === 'suspended') {
      waf()!.audioContext.resume().catch(console.error)
    }
    timer = playComposition(composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
      waf()!.player.cancelQueue(waf()!.audioContext)
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}

HtmlRenderer.registerCustomRenderer(compositionQ, render)

import HTMLRenderer from '../../../lpm/renderers/html.js'
import { Plot, plotQ } from '../viz.js'

import Chart from 'chart.js/auto'

HTMLRenderer.registerCustomRenderer(plotQ, (v: any): HTMLElement => {
  const canvas = document.createElement('canvas')
  canvas.width = 800
  const plot = v as Plot
  canvas.ariaLabel = "Plot"
  canvas.role = 'img'
  canvas.innerText = "Plot"
  new Chart(canvas, plot.opts as any)
  return canvas
})

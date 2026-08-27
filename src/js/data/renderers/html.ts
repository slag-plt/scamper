import type { ChartConfiguration } from 'chart.js'
import * as L from '../../../lpm'
import HTMLRenderer from '../../../lpm/renderers/html.js'
import { Plot, data_plotQ } from '../viz.js'

import Chart from 'chart.js/auto'

HTMLRenderer.registerCustomRenderer(data_plotQ, (v: L.Value): HTMLElement => {
  const canvas = document.createElement('canvas')
  canvas.width = 800
  const plot = v as Plot
  canvas.ariaLabel = 'Plot'
  canvas.role = 'img'
  canvas.innerText = 'Plot'
  // The config is assembled from whatever `with-plot-options` was given, so
  // there is nothing static to check it against; Chart.js validates it itself.
  new Chart(canvas, plot.opts as unknown as ChartConfiguration)
  return canvas
})

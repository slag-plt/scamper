import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { addCustomWebRenderer } from '../../display.js'

import Chart from 'chart.js/auto'
import { title } from 'process'

const viz: L.Library = new L.Library()

interface Plot extends L.Struct {
  [L.structKind]: 'plot'
  title: string
  opts: object
}

export function plotQ (v: L.Value): boolean {
  return L.isStructKind(v, 'plot')
}
viz.registerValue('plot?', plotQ)

export function plotLine (title: string, data: L.List): Plot {
  checkContract(arguments, contract('plot-line', [
    C.string,
    C.listof(C.pairof(C.or(C.number, C.string), C.number))
  ]))
  const points: {x: number | string, y: number}[] = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number | string, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'plot-line requires at least one data point')
  }
  const hasCategoryLabels = typeof points[0].x === 'string'
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'plot',
    title,
    opts: {
      type: 'line',
      data: {
        datasets: [{
          label: title,
          data: points
        }]
      },
      options: {
        scales: {
          x: { type: hasCategoryLabels ? 'category' : 'linear', }
        }
      }
    }
  }
}
viz.registerValue('plot-line', plotLine)

export function plotBar (title: string, data: L.List): Plot {
  checkContract(arguments, contract('plot-bar', [
    C.string,
    C.listof(C.pairof(C.or(C.string), C.number))
  ]))
  const points = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as string, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'plot-bar requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'plot',
    title,
    opts: {
      type: 'bar',
      data: {
        datasets: [{
          label: title,
          data: points
        }]
      },
      options: {
        scales: {
          x: { type: 'linear' }
        }
      }
    }
  }
}
viz.registerValue('plot-bar', plotBar)

export function plotScatter (title: string, data: L.List): Plot {
  checkContract(arguments, contract('plot-scatter', [C.string, C.listof(C.pairof(C.number, C.number))]))
  const points = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'plot-scatter requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'plot',
    title,
    opts: {
      type: 'scatter',
      data: {
        datasets: [{
          label: title,
          data: points
        }]
      },
      options: {
        scales: {
          x: { type: 'linear' }
        }
      }
    }
  }
}
viz.registerValue('plot-scatter', plotScatter)

addCustomWebRenderer(plotQ, (v: L.Value): HTMLElement => { const canvas = document.createElement('canvas')
  canvas.width = 800
  const plot = v as Plot
  canvas.ariaLabel = plot.title
  canvas.role = 'img'
  canvas.innerText = plot.title
  new Chart(canvas, plot.opts as any)
  return canvas
})

export default viz
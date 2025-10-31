import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { addCustomWebRenderer } from '../../display.js'

import Chart from 'chart.js/auto'

const viz: L.Library = new L.Library()

interface Dataset extends L.Struct {
  [L.structKind]: 'dataset'
  scale: 'linear' | 'category'
  opts: object
}

export function datasetQ (v: L.Value): boolean {
  return L.isStructKind(v, 'dataset')
}
viz.registerValue('dataset?', datasetQ)

interface Plot extends L.Struct {
  [L.structKind]: 'plot'
  opts: object
}

export function plotQ (v: L.Value): boolean {
  return L.isStructKind(v, 'plot')
}
viz.registerValue('plot?', plotQ)

export function plot (...args: L.Value[]): Plot {
  // TODO: hand-check arguments: datasets with an optional opts record
  const datasets: Dataset[] = args as Dataset[]
  // TODO: check to ensure dataasets all have consistent x-axis scales
  const scale = datasets[0].scale
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'plot',
    opts: {
      data: {
        datasets: datasets.map(ds => ds.opts)
      },
      options: {
        scales: {
          x: { type: scale }
        }
      }
    }
  }
}
viz.registerValue('plot', plot)

export function datasetLine (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-line', [
    C.string,
    C.listof(C.pairof(C.or(C.number, C.string), C.number))
  ]))
  const points: {x: number | string, y: number}[] = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number | string, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-line requires at least one data point')
  }
  const scale = typeof points[0].x === 'string' ? 'category' : 'linear'
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'dataset',
    scale,
    opts: {
      type: 'line',
      label: title,
      data: points,
    }
  }
}
viz.registerValue('dataset-line', datasetLine)

export function datasetBar (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('plot-bar', [
    C.string,
    C.listof(C.pairof(C.or(C.string), C.number))
  ]))
  const points: {x: number | string, y: number}[] = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number | string, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-bar requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'dataset',
    scale: 'category',
    opts: {
      type: 'bar',
      label: title,
      data: points,
    }
  }
}
viz.registerValue('dataset-bar', datasetBar)

export function datasetScatter (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-scatter', [C.string, C.listof(C.pairof(C.number, C.number))]))
  const points: {x: number | string, y: number}[] = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-scatter requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct', 
    [L.structKind]: 'dataset',
    scale: 'linear',
    opts: {
      type: 'scatter',
      label: title,
      data: points,
    }
  }
}
viz.registerValue('dataset-scatter', datasetScatter)

export function datasetBubble (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-bubble', [C.string, C.listof(C.listof(C.number))]))
  const points: {x: number, y: number, r: number}[] = L.listToVector(data).map(v => {
    const l = L.listToVector(v as L.List)
    if (l.length !== 3) {
      throw new L.ScamperError('Runtime', 'Data for dataset-bubble must be a list of three numbers')
    }
    return { x: l[0] as number, y: l[1] as number, r: l[2] as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-bubble requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'dataset',
    scale: 'linear',
    opts: {
      type: 'bubble',
      label: title,
      data: points,
    }
  }
}
viz.registerValue('dataset-bubble', datasetBubble)

export function datasetPie (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-pie', [C.string, C.listof(C.pairof(C.string, C.number))]))
  const labels: string[] = []
  const values: number[] = []
  L.listToVector(data).forEach(v => {
    const p = v as L.Pair
    labels.push(p.fst as string)
    values.push(p.snd as number)
  })
  if (values.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-pie requires at least one data point')
  }
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'dataset',
    scale: 'category',
    opts: {
      type: 'pie',
      label: title,
      data: values,
      labels
    }
  }
}
viz.registerValue('dataset-pie', datasetPie)

export function dataset


addCustomWebRenderer(plotQ, (v: L.Value): HTMLElement => {
  const canvas = document.createElement('canvas')
  canvas.width = 800
  const plot = v as Plot
  canvas.ariaLabel = plot.title
  canvas.role = 'img'
  canvas.innerText = plot.title
  new Chart(canvas, plot.opts as any)
  return canvas
})

export default viz
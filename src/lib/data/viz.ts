import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { addCustomWebRenderer } from '../../display.js'

import Chart from 'chart.js/auto'

const viz: L.Library = new L.Library()

interface Dataset extends L.Struct {
  [L.structKind]: 'dataset'
  opts: object
}

export function datasetQ (v: L.Value): boolean {
  return L.isStructKind(v, 'dataset')
}

interface Plot extends L.Struct {
  [L.structKind]: 'plot'
  opts: object
}

export function plotQ (v: L.Value): boolean {
  return L.isStructKind(v, 'plot')
}

export function plotLinear (...datasets: Dataset[]): Plot {
  checkContract(arguments, contract('plot-linear', [], C.struct('dataset')))
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'plot',
    opts: {
      data: {
        datasets: datasets.map(ds => ds.opts)
      },
      options: {
        scales: {
          x: { type: 'linear' }
        }
      }
    }
  }
}

function plotCategory (labels: L.List, ...datasets: Dataset[]): Plot {
  checkContract(arguments, contract('plot-category', [C.listof(C.string)], C.struct('dataset')))
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'plot',
    opts: {
      data: {
        labels: L.listToVector(labels) as string[],
        datasets: datasets.map(ds => ds.opts)
      },
      options: {
        scales: {
          x: { type: 'category' }
        }
      }
    }
  }
}

function plotRadial (labels: L.List, ...datasets: Dataset[]): Plot {
  checkContract(arguments, contract('plot-radial', [C.listof(C.string)], C.struct('dataset')))
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'plot',
    opts: {
      data: {
        labels: L.listToVector(labels) as string[],
        datasets: datasets.map(ds => ds.opts)
      }
    }
  }
}

function makeDataset (type: string, label: string, data: any[]): Dataset {
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'dataset',
    opts: { type, label, data, }
  }
}

export function datasetLine (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-line', [
    C.string,
    C.listof(C.or(C.number, C.pairof(C.number, C.number)))
  ]))
  const points: (number | {x: number, y: number})[] = L.listToVector(data).map(v => {
    if (L.isPair(v)) {
      return { x: v.fst as number, y: v.snd as number }
    } else {
      return v as number
    }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-line requires at least one data point')
  }
  return makeDataset('line', title, points)
}

export function datasetBar (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('plot-bar', [
    C.string,
    C.listof(C.number)
  ]))
  const points: number[] = L.listToVector(data) as number[]
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-bar requires at least one data point')
  }
  return makeDataset('bar', title, points)
}

export function datasetScatter (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-scatter', [
    C.string, C.listof(C.pairof(C.number, C.number))
  ]))
  const points: {x: number | string, y: number}[] = L.listToVector(data).map(v => {
    const p = v as L.Pair
    return { x: p.fst as number, y: p.snd as number }
  })
  if (points.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-scatter requires at least one data point')
  }
  return makeDataset('scatter', title, points)
}

export function datasetBubble (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-bubble', [
    C.string,
    C.listof(C.listof(C.number))
  ]))
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
  return makeDataset('bubble', title, points)
}

export function datasetPie (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-pie', [
    C.string,
    C.listof(C.number)
  ]))
  const values = L.listToVector(data) as number[]
  if (values.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-pie requires at least one data point')
  }
  return makeDataset('pie', title, values)
}

export function datasetPolar (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-polar', [
    C.string,
    C.listof(C.number)
  ]))
  const values = L.listToVector(data) as number[]
  if (values.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-polar requires at least one data point')
  }
  return makeDataset('polarArea', title, values )
}

export function datasetRadar (title: string, data: L.List): Dataset {
  checkContract(arguments, contract('dataset-radar', [
    C.string,
    C.listof(C.number)
  ]))
  const values = L.listToVector(data) as number[]
  if (values.length === 0) {
    throw new L.ScamperError('Runtime', 'dataset-radar requires at least one data point')
  }
  return makeDataset('radar', title, values)
}

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

viz.registerValue('dataset?', datasetQ)
viz.registerValue('plot?', plotQ)
viz.registerValue('plot-linear', plotLinear)
viz.registerValue('plot-category', plotCategory)
viz.registerValue('plot-radial', plotRadial)
viz.registerValue('dataset-line', datasetLine)
viz.registerValue('dataset-bar', datasetBar)
viz.registerValue('dataset-scatter', datasetScatter)
viz.registerValue('dataset-bubble', datasetBubble)
viz.registerValue('dataset-pie', datasetPie)
viz.registerValue('dataset-polar', datasetPolar)
viz.registerValue('dataset-radar', datasetRadar)

export default viz

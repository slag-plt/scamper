import { ICE, ScamperError } from './lang.js'
import * as V from './value.js'

function mkOutputDiv (body: HTMLElement) {
  const ret = document.createElement('div')
  ret.classList.add('scamper-output')
  ret.appendChild(body)
  return ret
}

function mkCodeElement (text: string): HTMLElement {
  const ret = document.createElement('code')
  ret.appendChild(document.createTextNode(text))
  return ret
}

type ConsoleRenderer = (v: any) => string
type WebRenderer = (v: any) => HTMLElement
type TypeTest = (v: any) => boolean

const customConsoleRenderers: [TypeTest, ConsoleRenderer][] = []
const customWebRenderers: [TypeTest, WebRenderer][] = []

export function addCustomConsoleRenderer (test: TypeTest, renderer: ConsoleRenderer): void {
  customConsoleRenderers.push([test, renderer])
}

export function addCustomWebRenderer (test: TypeTest, renderer: WebRenderer): void {
  const entry: [TypeTest, WebRenderer] = [test, renderer]
  customWebRenderers.push(entry)
}

function getRenderer<T> (v: any, renderers: [TypeTest, T][]): T | undefined {
  for (const [test, renderer] of renderers) {
    if (test(v)) { return renderer }
  }
  return undefined
}

function renderToString (v: any): string {
  switch (typeof v) {
    case 'boolean':
      return v ? '#t' : '#f'
    case 'number':
      return v.toString()
    case 'string':
      return `"${v}"`
    case 'undefined':
      return 'void'
    default:
      if (v === null) {
        return 'null'
      } else if (Array.isArray(v)) {
        return `(vector ...)`
      } else if (V.isClosure(v)) {
        return `[Function (closure)]`
      } else if (V.isJsFunction(v)) {
        return `[Function (JS)]`
      } else if (V.isChar(v)) {
        throw new Error(`renderToString: char unimplemented ${v.toString()}`)
      } else if (V.isPair(v)) {
        throw new Error(`renderToString: pair unimplemented ${v.toString()}`)
      } else {
        const customRenderer = getRenderer(v, customConsoleRenderers)
        if (customRenderer !== undefined) {
          return customRenderer(v)
        } else if (V.isStruct(v)) {
          const s = v as V.Struct
          return s.fields.length === 0 ?
            `(${s.kind})` :
            `(${s.kind} ${s.fields.map(renderToString).join(' ')})`
        } else if (v instanceof ScamperError) {
          return v.toString()
        } else if (v instanceof ICE) {
          return v.toString()
        } else {
          return `[Object]`
        }
      }
  }
}

function renderToHTML (v: any): HTMLElement {
  switch (typeof v) {
    case 'boolean':
      return mkCodeElement(v ? '#t' : '#f')
    case 'number':
      return mkCodeElement(v.toString())
    case 'string':
      return mkCodeElement(`"${v}"`)
    case 'undefined':
      return mkCodeElement('void')
    default:
      if (v === null) {
        return mkCodeElement('null')
      } else if (Array.isArray(v)) {
        return mkCodeElement(`(vector ...)`)
      } else if (V.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (V.isJsFunction(v)) {
        return mkCodeElement(`[Function (JS)]`)
      } else if (V.isChar(v)) {
        throw new Error(`renderToString: char unimplemented ${v.toString()}`)
      } else if (V.isPair(v)) {
        throw new Error(`renderToString: pair unimplemented ${v.toString()}`)
      } else {
        const customRenderer = getRenderer(v, customWebRenderers)
        if (customRenderer !== undefined) {
          return customRenderer(v)
        } else if (V.isStruct(v)) {
          const s = v as V.Struct
          return mkCodeElement(s.fields.length === 0 ?
            `(${s.kind})` :
            `(${s.kind} ${s.fields.map(renderToString).join(' ')})`)
        } else if (v instanceof ScamperError) {
          return mkCodeElement(v.toString())
        } else if (v instanceof ICE) {
          return mkCodeElement(v.toString())
        } else {
          return mkCodeElement(`[Object]`)
        }
      }
  }
}

function makeRenderer (id?: string): (v: any) => void {
  if (id === undefined) {
    return (v: any) => { console.log(renderToString(v)) }
  } else {
    return (v: any) => { document.getElementById(id)!.appendChild(mkOutputDiv(renderToHTML(v))) }
  }
}

export function makeRenderLib (id?: string): [string, V.Value][] {
  return [
    ['render', V.mkJsFunction(makeRenderer(id), 1)],
  ]
}
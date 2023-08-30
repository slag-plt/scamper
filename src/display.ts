import { charToName, ICE, ScamperError } from './lang.js'
import * as V from './value.js'

// TODO: probably need to sandbox this as an object held by the the Scamper
// instance at some point...

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

export function renderToString (v: any): string {
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
        const arr = v as any[]
        return arr.length === 0 ? '(vector)' : `(vector ${arr.map(renderToString).join(' ')})`
      } else if (V.isClosure(v)) {
        return `[Function (closure)]`
      } else if (V.isJsFunction(v)) {
        return `[Function (JS)]`
      } else if (V.isChar(v)) {
        return `#\\${charToName((v as V.Char).value)}`
      } else if (V.isList(v)) {
        const arr = V.listToArray(v)
        return arr.length === 0 ? '(list)' : `(list ${arr.map(renderToString).join(' ')})`
      } else if (V.isPair(v)) {
        return `(pair ${renderToString(v.fst)} ${renderToString(v.snd)})`
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

export function renderToHTML (v: any): HTMLElement {
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
        const vec = v as V.Value[]
        if (vec.length === 0) {
          return mkCodeElement('(vector)')
        }
        const ret = mkCodeElement('(vector ')
        ret.appendChild(renderToHTML(vec[0]))
        vec.slice(1).forEach((e) => {
          ret.appendChild(renderToHTML(e))
          ret.appendChild(mkCodeElement(' '))
        })
        ret.append(mkCodeElement(')'))
        return ret
      } else if (V.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (V.isJsFunction(v)) {
        return mkCodeElement(`[Function (JS)]`)
      } else if (V.isChar(v)) {
        return mkCodeElement(`#\\${charToName((v as V.Char).value)}`)
      } else if (V.isList(v)) {
        const ret = mkCodeElement('(list ')
        let lst = v
        ret.appendChild(renderToHTML(lst.fst))
        lst = lst.snd
        while (lst !== null) {
          ret.appendChild(mkCodeElement(' '))
          ret.appendChild(renderToHTML(lst.fst))
          lst = lst.snd
        }
        ret.append(mkCodeElement(')'))
        return ret
      } else if (V.isPair(v)) {
        const ret = mkCodeElement('(pair ')
        ret.appendChild(renderToHTML(v.fst))
        ret.appendChild(mkCodeElement(' '))
        ret.appendChild(renderToHTML(v.snd))
        ret.append(mkCodeElement(')'))
        return ret
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
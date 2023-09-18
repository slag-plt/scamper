import { charToName, ICE, ScamperError, Value } from './lang.js'

function mkOutputDiv (body: HTMLElement) {
  const ret = document.createElement('div')
  ret.classList.add('scamper-output')
  ret.appendChild(body)
  return ret
}

export function mkCodeElement (text: string): HTMLElement {
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
        const vec = v as Value.T[]
        if (vec.length === 0) {
          return mkCodeElement('(vector)')
        }
        const ret = mkCodeElement('(vector ')
        ret.appendChild(renderToHTML(vec[0]))
        vec.slice(1).forEach((e) => {
          ret.appendChild(mkCodeElement(' '))
          ret.appendChild(renderToHTML(e))
        })
        ret.append(mkCodeElement(')'))
        return ret
      } else if (Value.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (Value.isJsFunction(v)) {
        return mkCodeElement(`[Function (JS)]`)
      } else if (Value.isChar(v)) {
        return mkCodeElement(`#\\${charToName((v as Value.Char).value)}`)
      } else if (Value.isList(v)) {
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
      } else if (Value.isPair(v)) {
        const ret = mkCodeElement('(pair ')
        ret.appendChild(renderToHTML(v.fst))
        ret.appendChild(mkCodeElement(' '))
        ret.appendChild(renderToHTML(v.snd))
        ret.append(mkCodeElement(')'))
        return ret
      } else if (v instanceof HTMLElement) {
        return v
      } else {
        const customRenderer = getRenderer(v, customWebRenderers)
        if (customRenderer !== undefined) {
          return customRenderer(v)
        } else if (Value.isStruct(v)) {
          const s = v as Value.Struct
          const fields = Value.getFieldsOfStruct(s)
          if (fields.length === 0) {
            return mkCodeElement(`(${s.kind})`)
          } else {
            const ret = mkCodeElement(`(${s.kind} `)
            ret.appendChild(renderToHTML(s[fields[0]]))
            for (let i = 1; i < fields.length; i++) {
              ret.appendChild(mkCodeElement(' '))
              ret.appendChild(renderToHTML(s[fields[i]]))
            }
            ret.append(mkCodeElement(')'))
            return ret
          }
        } else if (v instanceof ScamperError) {
          return mkCodeElement(v.toString())
        } else if (v instanceof ICE) {
          return mkCodeElement(v.toString())
        } else if (v instanceof Error) {
          return mkCodeElement(v.toString())
        } else {
          return mkCodeElement(`[Blob: ${JSON.stringify(v)}]`)
        }
      }
  }
}

export function renderToOutput(output: HTMLElement, v: any) {
  const div = document.createElement('div')
  div.classList.add('scamper-output')
  div.appendChild(renderToHTML(v))
  output!.appendChild(div)
}
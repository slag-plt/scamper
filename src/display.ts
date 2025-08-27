import * as L from './lpm'
import hljs from 'highlight.js'

export function mkCodeElement (text: string): HTMLElement {
  const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
  const ret = document.createElement('code')
  ret.classList.add('hljs')
  ret.innerHTML = elt.value
  ret.tabIndex = 0;
  return ret
}

export function mkSourceBlock (text: string): HTMLElement {
  const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
  const ret = document.createElement('pre')
  ret.classList.add('hljs')
  ret.innerHTML = elt.value
  ret.tabIndex = 0;
  return ret
}

type WebRenderer = (v: any) => HTMLElement
type TypeTest = (v: any) => boolean

const customWebRenderers: [TypeTest, WebRenderer][] = []

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

export function renderToHTML (v: L.Value): HTMLElement {
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
        return mkCodeElement('()')
      } else if (L.isSym(v)) {
        return mkCodeElement((v as L.Sym).value)
      } else if (L.isArray(v)) {
        const vec = v as L.Value[]
        if (vec.length === 0) {
          return mkCodeElement('[]')
        }
        const ret = mkCodeElement('[')
        ret.appendChild(renderToHTML(vec[0]))
        vec.slice(1).forEach((e) => {
          ret.appendChild(mkCodeElement(' '))
          ret.appendChild(renderToHTML(e))
        })
        ret.append(mkCodeElement(']'))
        return ret
      } else if (L.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (L.isJsFunction(v)) {
        return mkCodeElement(`[Function (${(v as Function).name})]`)
      } else if (L.isChar(v)) {
        return mkCodeElement(`#\\${L.charToName((v as L.Char).value)}`)
      } else if (L.isList(v)) {
        const ret = mkCodeElement('(')
        let lst: any = v
        // N.B., we know the list is non-empty because we cover the null case already
        ret.appendChild(renderToHTML(lst.fst))
        lst = lst.snd
        while (lst !== null) {
          ret.appendChild(mkCodeElement(' '))
          ret.appendChild(renderToHTML(lst.fst))
          lst = lst.snd
        }
        ret.append(mkCodeElement(')'))
        return ret
      } else if (L.isPair(v)) {
        // TODO: do we introduce `( . `)` for pairs again?
        const ret = mkCodeElement('(pair ')
        ret.appendChild(renderToHTML((v as L.Pair).fst))
        ret.appendChild(mkCodeElement(' '))
        ret.appendChild(renderToHTML((v as L.Pair).snd))
        ret.append(mkCodeElement(')'))
        return ret
      } else if (v instanceof HTMLElement) {
        return v
      } else {
        // TODO: note: we never cycle back to expToHTML if the value is an
        // embedded expression. This shouldn't happy for now, so we'll
        // defer fixing this bug until we refactor our AST to be more concise.
        const customRenderer = getRenderer(v, customWebRenderers)
        if (customRenderer !== undefined) {
          return customRenderer(v)
        } else if (L.isStruct(v)) {
          const s = v as L.Struct
          const fields = L.getFieldsOfStruct(s)
          if (fields.length === 0) {
            return mkCodeElement(`(${s[L.structKind]})`)
          } else {
            const ret = mkCodeElement(`(${s[L.structKind]} `)
            ret.appendChild(renderToHTML(s[fields[0]]))
            for (let i = 1; i < fields.length; i++) {
              ret.appendChild(mkCodeElement(' '))
              ret.appendChild(renderToHTML(s[fields[i]]))
            }
            ret.append(mkCodeElement(')'))
            return ret
          }
        } else if (v instanceof L.ScamperError) {
          return mkCodeElement(v.toString())
        } else if (v instanceof L.ICE) {
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
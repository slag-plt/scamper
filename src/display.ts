import * as LPM from './lpm'
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

export function renderToHTML (v: LPM.Value): HTMLElement {
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
      } else if (LPM.isSym(v)) {
        return mkCodeElement((v as LPM.Sym).value)
      } else if (LPM.isArray(v)) {
        const vec = v as LPM.Value[]
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
      } else if (LPM.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (LPM.isJsFunction(v)) {
        return mkCodeElement(`[Function (${(v as Function).name})]`)
      } else if (LPM.isChar(v)) {
        return mkCodeElement(`#\\${LPM.charToName((v as LPM.Char).value)}`)
      } else if (LPM.isList(v)) {
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
      } else if (LPM.isPair(v)) {
        // TODO: do we introduce `( . `)` for pairs again?
        const ret = mkCodeElement('(pair ')
        ret.appendChild(renderToHTML((v as LPM.Pair).fst))
        ret.appendChild(mkCodeElement(' '))
        ret.appendChild(renderToHTML((v as LPM.Pair).snd))
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
        } else if (LPM.isStruct(v)) {
          const s = v as LPM.Struct
          const fields = LPM.getFieldsOfStruct(s)
          if (fields.length === 0) {
            return mkCodeElement(`(${s[LPM.structKind]})`)
          } else {
            const ret = mkCodeElement(`(${s[LPM.structKind]} `)
            ret.appendChild(renderToHTML(s[fields[0]]))
            for (let i = 1; i < fields.length; i++) {
              ret.appendChild(mkCodeElement(' '))
              ret.appendChild(renderToHTML(s[fields[i]]))
            }
            ret.append(mkCodeElement(')'))
            return ret
          }
        } else if (v instanceof LPM.ScamperError) {
          return mkCodeElement(v.toString())
        } else if (v instanceof LPM.ICE) {
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

export class HTMLDisplay implements LPM.OutputChannel, LPM.ErrorChannel {
  display: HTMLElement

  constructor (display: HTMLElement) {
    this.display = display
  }

  send (v: LPM.Value): void {
    renderToOutput(this.display, v)
  }

  report (err: LPM.ScamperError): void {
    renderToOutput(this.display, err)
  }
}
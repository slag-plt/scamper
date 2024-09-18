import { charToName, ICE, ScamperError, Value } from './lang.js'

export function mkCodeElement (text: string): HTMLElement {
  const ret = document.createElement('code')
  ret.appendChild(document.createTextNode(text))
  // TODO: here is where I want to syntax highlight, I think!
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

export function renderToHTML (v: Value.T): HTMLElement {
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
      } else if (Value.isSym(v)) {
        return mkCodeElement((v as Value.Sym).value)
      } else if (Value.isArray(v)) {
        const vec = v as Value.T[]
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
      } else if (Value.isClosure(v)) {
        return mkCodeElement(`[Function (closure)]`)
      } else if (Value.isJsFunction(v)) {
        return mkCodeElement(`[Function (${(v as Function).name})]`)
      } else if (Value.isChar(v)) {
        return mkCodeElement(`#\\${charToName((v as Value.Char).value)}`)
      } else if (Value.isList(v)) {
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
      } else if (Value.isPair(v)) {
        // TODO: do we introduce `( . `)` for pairs again?
        const ret = mkCodeElement('(pair ')
        ret.appendChild(renderToHTML((v as Value.Pair).fst))
        ret.appendChild(mkCodeElement(' '))
        ret.appendChild(renderToHTML((v as Value.Pair).snd))
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
        } else if (Value.isStruct(v)) {
          const s = v as Value.Struct
          const fields = Value.getFieldsOfStruct(s)
          if (fields.length === 0) {
            return mkCodeElement(`(${s[Value.structKind]})`)
          } else {
            const ret = mkCodeElement(`(${s[Value.structKind]} `)
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
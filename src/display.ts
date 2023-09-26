import { Bracket, closeBracket, charToName, ICE, ScamperError, Exp, Pat, Value } from './lang.js'

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

function bracketHTMLElements (bracket: Bracket, es: HTMLElement[]): HTMLElement {
  const ret = mkCodeElement(bracket)
  ret.append(es[0])
  for (let i = 1; i < es.length; i++) {
    ret.appendChild(mkCodeElement(' '))
    ret.appendChild(es[i])
  }
  ret.append(mkCodeElement(closeBracket(bracket)))
  return ret
}

export function patToHTML (p: Pat.T): HTMLElement {
  switch (p.kind) {
    case 'var':
      return mkCodeElement(p.name)

    case 'wild':
      return mkCodeElement('_')

    case 'num':
      return mkCodeElement(p.value.toString())

    case 'bool':
      return mkCodeElement(p.value ? '#t' : '#f')

    case 'char':
      return mkCodeElement(`#\\${charToName(p.value)}`)

    // TODO: need to unescape string values!
    case 'str':
      return mkCodeElement(`"${p.value}"`)

    case 'null':
      return mkCodeElement('null')

    case 'ctor':
      return bracketHTMLElements('(', [
        mkCodeElement(p.ctor),
        ...p.args.map(patToHTML)
      ])
  }
}

function letToHTML (soFar: Exp.Binding[], e: Exp.Let): HTMLElement {
  if (e.bindings.length === 1 && e.body.kind === 'let') {
    soFar.push(e.bindings[0])
    return letToHTML(soFar, e.body)
  } else if (soFar.length === 0) {
    return bracketHTMLElements('(', [
      mkCodeElement('let'),
      bracketHTMLElements('(', e.bindings.map(({name, body}) => {
        return bracketHTMLElements('[', [
          mkCodeElement(name),
          expToHTML(body)
        ])
      })),
      expToHTML(e.body)
    ])
  } else if (e.bindings.length > 1) {
    const ret = bracketHTMLElements('(', [
      mkCodeElement('let'),
      bracketHTMLElements('(', e.bindings.map(({name, body}) => {
        return bracketHTMLElements('[', [
          mkCodeElement(name),
          expToHTML(body)
        ])
      })),
      expToHTML(e.body)
    ])
    return bracketHTMLElements('(', [
      mkCodeElement('let*'),
      bracketHTMLElements('(', soFar.map(({name, body}) => {
        return bracketHTMLElements('[', [
          mkCodeElement(name),
          expToHTML(body)
        ])
      })),
      ret
    ])
  } else {
    soFar.push(e.bindings[0])
    return bracketHTMLElements('(', [
      mkCodeElement('let*'),
      bracketHTMLElements('(', soFar.map(({name, body}) => {
        return bracketHTMLElements('[', [
          mkCodeElement(name),
          expToHTML(body)
        ])
      })),
      expToHTML(e.body)
    ])
  }
}

export function expToHTML (e: Exp.T): HTMLElement {
  switch (e.kind) {
    case 'var':
      return mkCodeElement(e.name)

    case 'val':
      return renderToHTML(e.value)

    case 'lam':
      return bracketHTMLElements('(', [
        mkCodeElement('lambda'),
        bracketHTMLElements('(', e.args.map(mkCodeElement)),
        expToHTML(e.body)
      ])

    case 'let':
      return letToHTML([], e)

    case 'app':
      return bracketHTMLElements('(', [expToHTML(e.head), ...e.args.map(expToHTML)])

    case 'and':
      return bracketHTMLElements('(', [
        mkCodeElement('and'),
        ...e.exps.map(expToHTML)
      ])

    case 'or':
      return bracketHTMLElements('(', [
        mkCodeElement('or'),
        ...e.exps.map(expToHTML)
      ])

    case 'if':
      return bracketHTMLElements('(', [
        mkCodeElement('if'),
        expToHTML(e.guard),
        expToHTML(e.ifb),
        expToHTML(e.elseb)
      ])

    case 'begin':
      return bracketHTMLElements('(', [
        mkCodeElement('begin'),
        ...e.exps.map(expToHTML)
      ])

    case 'cond':
      return bracketHTMLElements('(', [
        mkCodeElement('cond'),
        ...e.branches.map(({guard, body}) => {
          return bracketHTMLElements('[', [
            expToHTML(guard),
            expToHTML(body)
          ])
        })
      ])

    case 'match':
      return bracketHTMLElements('(', [
        mkCodeElement('match'),
        expToHTML(e.scrutinee),
        ...e.branches.map(({pattern, body}) => {
          return bracketHTMLElements('[', [
            patToHTML(pattern),
            expToHTML(body)
          ])
        })
      ])
  }
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
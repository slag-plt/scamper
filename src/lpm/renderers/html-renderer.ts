import { structKind, List, Value } from '../lang.js'
import { ICE, ScamperError } from '../error.js'
import * as R from './index.js'
import * as U from '../util.js'

function mkCodeElement (text: string): HTMLElement {
  const ret = document.createElement('code')
  ret.textContent = text
  ret.tabIndex = 0;
  return ret
}

export class Renderer extends R.Renderer<HTMLElement> {
  render(v: Value): HTMLElement {
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
        } else if (U.isSym(v)) {
          return mkCodeElement(v.value)
        } else if (U.isArray(v)) {
          if (v.length === 0) {
            return mkCodeElement('(vector)')
          }
          const ret = mkCodeElement('(vector ')
          ret.appendChild(this.render(v[0]))
          v.slice(1).forEach((e) => {
            ret.appendChild(mkCodeElement(' '))
            ret.appendChild(this.render(e))
          })
          ret.append(mkCodeElement(')'))
          return ret
        } else if (U.isClosure(v)) {
          return mkCodeElement(`[Function (closure)]`)
        } else if (U.isJsFunction(v)) {
          return mkCodeElement(`[Function (${(v as Function).name})]`)
        } else if (U.isChar(v)) {
          return mkCodeElement(`#\\${U.charToName(v.value)}`)
        } else if (U.isList(v)) {
          const ret = mkCodeElement('(list ')
          let cur: List = v
          // N.B., we know the list is non-empty because we cover the null case already
          ret.appendChild(this.render(cur.head))
          cur = cur.tail
          while (cur !== null) {
            ret.appendChild(mkCodeElement(' '))
            ret.appendChild(this.render(cur.head))
            cur = cur.tail
          }
          ret.append(mkCodeElement(')'))
          return ret
        } else if (U.isPair(v)) {
          // TODO: do we introduce `( . `)` for pairs again?
          const ret = mkCodeElement('(pair ')
          ret.appendChild(this.render(v.fst))
          ret.appendChild(mkCodeElement(' '))
          ret.appendChild(this.render(v.snd))
          ret.append(mkCodeElement(')'))
          return ret
        } else if (v instanceof HTMLElement) {
          return v
        } else {
          // TODO: note: we never cycle back to expToHTML if the value is an
          // embedded expression. This shouldn't happy for now, so we'll
          // defer fixing this bug until we refactor our AST to be more concise.
          const customRenderer = this.getCustomRendererFor(v)
          if (customRenderer !== null) {
            return customRenderer(v)
          } else if (U.isStruct(v)) {
            const fields = U.getFieldsOfStruct(v)
            if (fields.length === 0) {
              return mkCodeElement(`(${v[structKind]})`)
            } else {
              const ret = mkCodeElement(`(${v[structKind]} `)
              ret.appendChild(this.render(v[fields[0]]))
              for (let i = 1; i < fields.length; i++) {
                ret.appendChild(mkCodeElement(' '))
                ret.appendChild(this.render(v[fields[i]]))
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
}

const HTMLRenderer = new Renderer()
export default HTMLRenderer
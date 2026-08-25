import { structKind, Value } from '../lang.js'
import { ICE, ScamperError } from '../error.js'
import * as R from './index.js'
import * as U from '../util.js'

class Renderer extends R.Renderer<string> {
  /**
   * @param col the column this rendering starts at, so a custom renderer that
   *   breaks lines can indent the continuation ones to sit under it. Callers
   *   writing a prefix first -- the trace's "--> ", say -- pass its width.
   */
  public render(v: Value, col = 0): string {
    const customRenderer = this.getCustomRendererFor(v)
    if (customRenderer) {
      return customRenderer(v, col)
    } else {
      switch (typeof v) {
        case 'boolean': return v ? '#t' : '#f'
        case 'number': return v.toString()
        case 'string': return `"${U.escapeStringLiteral(v)}"`
        case 'undefined': return 'void'
        default:
          if (v === null) {
            return 'null'
          } else if (U.isArray(v)) {
            return v.length === 0 ? '(vector)' :
              `(vector ${v.map((v) => this.render(v)).join(' ')})`
          } else if (U.isClosure(v)) {
            return `[Function: ${v.name ?? '##anonymous##'}]`
          } else if (U.isFunction(v)) {
            return `[Function: ${v.name ?? '##anonymous##'}]`
          } else if (U.isChar(v)) {
            return `#\\${U.charToName(v.value)}`
          } else if (U.isList(v)) {
            return `(list ${U.listToVector(v).map((v) => this.render(v)).join(' ')})`
          } else if (U.isPair(v)) {
            return `(pair ${this.render(v.fst)} ${this.render(v.snd)})`
          } else if (U.isStruct(v)) {
            const name = v[structKind]
            const fields = U.getFieldsOfStruct(v)
            if (fields.length === 0) {
              return `(${name})`
            } else {
              const args = fields.map((f) => this.render(v[f])).join(' ')
              return `(${name} ${args})`
            }
          } else if (v instanceof ScamperError) {
            return v.toString()
          } else if (v instanceof ICE) {
            return v.toString()
          } else if (v instanceof Error) {
            return v.toString()
          } else if (U.isObj(v)) {
            return U.objToString(v, (x) => this.render(x))
          } else {
            return `[Blob: ${JSON.stringify(v)}]`
          }
      }
    }
  }
}

const TextRenderer = new Renderer()
export default TextRenderer
import { Value } from '../lang.js'
import { OutputChannel, ErrorChannel } from '../../lpm/output'
import { ScamperError } from '../error.js'
import TextRenderer from '../renderers/text'

export class ConsoleOutput implements OutputChannel, ErrorChannel {
  seenError = false
  private _totalSends = 0

  send(v: Value): void {
    console.log(TextRenderer.render(v))
    this._totalSends++
  }

  report(e: ScamperError): void {
    this.seenError = true
    console.error(TextRenderer.render(e))
  }

  pushLevel(..._attrs: string[]) {
    /* nothing to do! */
  }

  popLevel() {
    /* nothing to do! */
  }

  get totalSends() {
    return this._totalSends
  }
}

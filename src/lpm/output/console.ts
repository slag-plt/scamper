import { Value } from '../lang.js'
import { OutputChannel, ErrorChannel } from '../../lpm/output'
import { ScamperError } from '../error.js'
import TextRenderer from '../renderers/text'

export class ConsoleOutput implements OutputChannel, ErrorChannel {
  seenError = false
  send (v: Value): void {
    console.log(TextRenderer.render(v))
  }

  report (e: ScamperError): void {
    this.seenError = true
    console.error(TextRenderer.render(e))
  }

  pushLevel (..._attrs: string[]) { /* nothing to do! */ }
  popLevel () { /* nothing to do! */}
}

import { Value } from '../lang.js'
import { ScamperError } from '../error.js'
import { OutputChannel, ErrorChannel } from './channel.js'
import TextRenderer from '../renderers/text'

/** A unified output and error channel that logs by line */
export class LoggingChannel implements OutputChannel, ErrorChannel {
  log: Value[]
  errLog: string[]
  renderOutput: boolean
  combineLogs: boolean

  constructor (renderOutput = true, combineLogs = true) {
    this.log = []
    this.errLog = []
    this.renderOutput = renderOutput
    this.combineLogs = combineLogs
  }

  send (v: Value): void {
    this.log.push(this.renderOutput ? TextRenderer.render(v): v)
  }

  report (e: ScamperError): void {
    if (this.combineLogs) {
      this.log.push(e.toString())
    } else {
      this.errLog.push(e.toString())
    }
  }

  pushLevel (..._attrs: string[]) { /* nothing to do! */ }
  popLevel () { /* nothing to do! */ }
}

import { Value } from '../lang.js'
import { ScamperError } from '../error.js'
import { toString } from '../util.js'
import { OutputChannel, ErrorChannel } from './channel.js'

/** A channel that simply logs any output that it is fed. */
export class LoggingOutputChannel implements OutputChannel {
  log: Value[]

  constructor () {
    this.log = []
  }
  pushLevel (_label: string, _attrs: string[]) { /* nothing to do! */ }
  popLevel () { /* nothing to do! */ }

  send (v: Value): void {
    this.log.push(v)
  }
}


/** An error channel that simply logs any errors that arise. */
export class LoggingErrorChannel implements ErrorChannel {
  log: ScamperError[]

  constructor () {
    this.log = []
  }

  report (err: ScamperError): void {
    this.log.push(err)
  }
}

/** A unified output and error channel that logs by line */
export class LoggingChannel implements OutputChannel, ErrorChannel {
  log: string[]

  constructor () {
    this.log = []
  }

  send (v: Value): void {
    this.log.push(toString(v))
  }

  report (e: ScamperError): void {
    this.log.push(e.toString())
  }

  pushLevel (_label: string, _attrs: string[]) { /* nothing to do! */ }
  popLevel () { /* nothing to do! */ }
}

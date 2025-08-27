import { Value } from './lang.js'
import { ScamperError } from './error.js'
import { toString } from './util.js'

/** A "console" that can receive output from LPM. */
export interface OutputChannel {
  send: (v: Value) => void
}

/** A channel that simply logs any output that it is fed. */
export class LoggingOutputChannel implements OutputChannel {
  log: Value[]

  constructor () {
    this.log = []
  }

  send (v: Value): void {
    this.log.push(v)
  }
}

/** A "console" that can receive errors from LPM. */
export interface ErrorChannel {
  report: (e: ScamperError) => void
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
}

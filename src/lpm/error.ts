import { Range } from './range.js'
import { Value } from './lang'
import { toString } from './util'

/** Phases of scamper execution, used for the purposes of error reporting. */
type Phase = 'Parser' | 'Runtime' | 'Docstring'

/** Errors that arise during Scamper compilation and execution. */
export class ScamperError extends Error {
  phase: Phase
  modName?: string
  range?: Range
  source?: string

  constructor(
    phase: Phase,
    msg: string,
    modName?: string,
    range?: Range,
    source?: string,
  ) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
    this.source = source
  }

  toString(): string {
    const detail = `${this.modName ?? ''}${this.range && this.range !== Range.none ? this.range.toString() : ''}`
    const src = this.source ? `(${this.source}) ` : ''
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${src}${this.message}`
  }
}

/** Internal compiler errors arise due to bugs in Scamper. */
export class ICE extends Error {
  funcName: string

  constructor(funcName: string, msg: string) {
    super(msg)
    this.funcName = funcName
  }

  toString(): string {
    return `ICE (${this.funcName}): ${this.message}\n${this.stack ?? ''}`
  }
}

// TODO: likely deprecated after fiber change
export class SubthreadErrors extends Error {
  errors: ScamperError[]
  constructor(errors: ScamperError[]) {
    const msg = errors.map((e) => e.toString()).join(' ')
    super(msg)
    this.errors = [...errors]
  }
}

export class ReportError extends ScamperError {
  constructor(
    public value: Value,
    public range: Range,
  ) {
    super('Runtime', `Reported value: ${toString(value)}`)
  }
}

/**
 * Thrown by a blocking library primitive to suspend the currently-running fiber
 * while the scheduler performs an asynchronous action; the fiber then resumes
 * with the action's resolved value pushed as the primitive call's return value
 * (see Scheduler's `block-on` handling and Fiber.resumeWithValue). This is
 * control flow, NOT an error -- it is caught by the scheduler before any error
 * handling and is never surfaced to the user.
 */
export class SuspendSignal {
  constructor(public action: () => Promise<Value>) {}
}

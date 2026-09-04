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

  /**
   * @returns a copy of this error with its source range removed, so its
   * rendered form omits the `[line:col-...]` location. Lets tests assert on
   * error messages without coupling to (fragile) source line numbers.
   */
  stripRange(): ScamperError {
    return new ScamperError(this.phase, this.message, this.modName, undefined, this.source)
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
export class SuspendSignal extends Error {
  /**
   * Where the suspending call was written. A primitive throws this signal with
   * no range -- it has no idea -- and `applyFn` fills it in on the way out,
   * with the same call site it would have given a synchronously-thrown error.
   * The scheduler then attaches it to whatever the action rejects with, which
   * is otherwise raised far from the call and arrives unlocated (#342).
   */
  range?: Range

  constructor(public action: () => Promise<Value>) {
    // Extends Error only so that `throw` of it is a throw of an error, which is
    // what every linter and every reader expects; it is still control flow, and
    // both catch sites (Scheduler.stepTask, applyFn) test for it before they
    // test for anything else, so nothing that handles errors ever sees one.
    super('a blocking primitive suspended the fiber')
  }
}

/**
 * Thrown by `set-maximum-recursion-depth!` to raise or lower the running
 * fiber's call stack limit. The limit belongs to the Fiber, which a library
 * primitive has no handle on, so the request travels out to `applyFn` -- the
 * runtime's single native-invocation site, which does hold the fiber -- and is
 * fully handled there. Like SuspendSignal this is control flow, NOT an error:
 * it is tested for before any error handling and never surfaces to the user.
 *
 * It carries the depth rather than a callback so that error.ts need not import
 * Fiber; there is exactly one primitive of this kind, so a general effect
 * mechanism would be a cycle bought for nothing.
 */
export class SetRecursionDepthSignal extends Error {
  constructor(public depth: number) {
    // Extends Error only so that `throw` of it is a throw of an error, which is
    // what every linter and every reader expects.
    super('a primitive set the fiber\'s maximum recursion depth')
  }
}

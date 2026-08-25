import { Value } from './lang'
import { ScamperError } from './error'

/**
 * A handle on one program run, held so a callback can fire against the run that
 * registered it (#375).
 *
 * A page can hold several independent programs at once -- the reading widgets
 * are the reason -- and a DOM listener, timer, or animation loop fires long
 * after the step that registered it, when no fiber is running and there is
 * nothing ambient to consult. So a library function that registers one captures
 * its run *at registration* and uses the handle afterwards:
 *
 * ```ts
 * const run = currentRun()
 * element.addEventListener('click', () => run.spawn(fn, []), { signal: run.signal })
 * ```
 */
export interface RunHandle {
  /**
   * Runs `(fn ...args)` as a new fiber in this run's top-level environment, so
   * it sees the program's definitions and reports to the program's error
   * channel. `onComplete` receives the closure's result, or null.
   */
  spawn(fn: Value, args: Value[], onComplete?: (result: Value | null) => void): void
  /**
   * This run's AbortSignal, aborted when the program is re-run or stopped.
   * Pass it as `addEventListener`'s `{ signal }`, clear an interval on its
   * 'abort', or stop an animation loop once `aborted`, so a handler tears
   * itself down with the run that made it rather than leaking into the next.
   */
  readonly signal: AbortSignal | undefined
}

/**
 * Resolves the run a call belongs to. Registered by the Scamper singleton at
 * startup; this indirection lets library event handlers (src/js/*) reach a run
 * WITHOUT importing the heavy Scamper singleton -- an eager import of
 * scamper.ts from those modules would run at test-setup time and grab real,
 * unmocked transitive deps (see test/setup.ts).
 */
export type RunResolver = () => RunHandle | undefined

let resolver: RunResolver | undefined

/** Registered by the Scamper singleton at startup. See {@link currentRun}. */
export function setRunResolver(fn: RunResolver): void {
  resolver = fn
}

/** A handle that drops what it is given, for when no program is running. */
const NO_RUN: RunHandle = {
  spawn: () => {
    /* nothing to run the callback in */
  },
  signal: undefined,
}

/**
 * @returns a handle on the run this call belongs to -- the one whose fiber is
 *          stepping, or the foreground program if called from outside a step.
 *
 * Capture this at registration and keep the handle; calling it later, from
 * inside the callback, is what ties a handler to the wrong program.
 */
export function currentRun(): RunHandle {
  return resolver?.() ?? NO_RUN
}

/**
 * Runs `(fn ...args)` as a new fiber in the active program's top-level
 * environment, resolving the run at the moment of the call.
 *
 * Prefer {@link currentRun} wherever the call is deferred: a callback firing
 * later resolves to the foreground program, which is the wrong one as soon as a
 * page holds more than one (#375).
 */
export function spawn(
  fn: Value,
  args: Value[],
  onComplete?: (result: Value | null) => void,
): void {
  if (resolver === undefined) {
    throw new ScamperError(
      'Runtime',
      'Cannot run a callback: no Scamper program is active',
    )
  }
  currentRun().spawn(fn, args, onComplete)
}

/**
 * The current program run's AbortSignal. See {@link RunHandle.signal}.
 *
 * Prefer `currentRun().signal`, which is the same value but keeps the handle
 * that the matching `spawn` should go through.
 */
export function currentRunSignal(): AbortSignal | undefined {
  return currentRun().signal
}

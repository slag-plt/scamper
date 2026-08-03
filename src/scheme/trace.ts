import { Fiber } from '../lpm/fiber.js'
import { expToString, mkLit } from './ast.js'
import { sugarExpr } from './sugar.js'
import { raiseFiber } from './raise.js'

/**
 * Lazily yields the reduction trace of `fiber`: the program rendered as a
 * sugared expression after each top-level reduction step, ending at the final
 * value. Composes the tracing pieces -- raise (fiber -> expression), sugar
 * (recover derived forms), and expToString.
 *
 * This is a *generator*, so stepping is pull-based: each `.next()` advances the
 * fiber to the next user-visible step and yields its rendering, pausing in
 * between. A batch driver (the CLI) consumes it with `for...of`; an interactive
 * one (a future step-by-step GUI) can instead call `.next()` on demand -- one
 * reduction per key-press or button-click -- with no change here.
 *
 * Granularity policy for the readable, classic reduction trace:
 *   - snapshot only at the top frame (`frames.length === 1`), so a called
 *     function's internal steps are atomic rather than exposed;
 *   - skip any snapshot mentioning an internal `##...##` name (e.g. a contract
 *     wrapper), which is never user-visible;
 *   - end with the fiber's final value, since a trailing builtin application
 *     produces its value inside its own frame, off the top level.
 *
 * N.B. steps the fiber synchronously, so -- unlike the scheduler-driven
 * execution path -- file imports and async (`block-on`) operations are not
 * handled; a program that blocks will not complete here.
 *
 * @param fiber a freshly-constructed fiber for the program to trace
 */
export function* traceReductions(fiber: Fiber): Generator<string> {
  let last: string | undefined
  // The current program state as a user-visible reduction, or null to skip: a
  // non-top-frame step, an internal `##...##` state, or a repeat of the last.
  const render = (): string | null => {
    if (fiber.frames.length !== 1) return null
    const s = expToString(sugarExpr(raiseFiber(fiber)))
    return !s.includes('##') && s !== last ? s : null
  }

  let s = render()
  if (s !== null) {
    last = s
    yield s
  }
  while (!fiber.isDone()) {
    if (fiber.step().tag === 'trace') {
      s = render()
      if (s !== null) {
        last = s
        yield s
      }
    }
  }
  if (fiber.lastResult !== null) {
    const value = expToString(mkLit(fiber.lastResult))
    if (value !== last) yield value
  }
}

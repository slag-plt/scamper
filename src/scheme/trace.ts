import { Fiber } from '../lpm/fiber.js'
import { Value } from '../lpm/lang.js'
import { Exp, expToString, mkLit } from './ast.js'
import { sugarExpr } from './sugar.js'
import { raiseFiber } from './raise.js'

// The fiber's current state as a user-visible reduction expression (with its
// rendering), or undefined to skip: only the top frame is shown (a called
// function's internals stay atomic), and internal `##...##` states are never
// surfaced.
function visibleReduction(fiber: Fiber): { exp: Exp; str: string } | undefined {
  if (fiber.frames.length !== 1) return undefined
  const exp = sugarExpr(raiseFiber(fiber))
  const str = expToString(exp)
  return str.includes('##') ? undefined : { exp, str }
}

/**
 * Builds a stateful reduction stepper -- the shared source of truth for the
 * step-by-step trace. `render` raises + sugars the fiber's current state to the
 * next user-visible reduction (deduping consecutive repeats); `final` renders a
 * completed statement's value as its last step. Structurally a
 * `FiberTraceStepper` (see src/lpm/raiser.ts), so both the CLI trace
 * (`traceReductions`) and the scheduler's step mode drive identical logic.
 */
export function makeTraceStepper(): {
  render: (fiber: Fiber) => Exp | undefined
  final: (value: Value) => Exp | undefined
} {
  let last: string | undefined
  return {
    render(fiber) {
      const r = visibleReduction(fiber)
      if (r === undefined || r.str === last) return undefined
      last = r.str
      return r.exp
    },
    final(value) {
      const exp = mkLit(value)
      const str = expToString(exp)
      if (str === last) return undefined
      last = str
      return exp
    },
  }
}

/**
 * Lazily yields the reduction trace of `fiber`: the program rendered as a
 * sugared expression after each top-level reduction step, ending at the final
 * value. See `makeTraceStepper` for the granularity policy.
 *
 * This is a *generator*, so stepping is pull-based: each `.next()` advances the
 * fiber to the next user-visible step and yields its rendering, pausing in
 * between. A batch driver (the CLI) consumes it with `for...of`; the web IDE's
 * step mode drives the same policy through the scheduler instead.
 *
 * N.B. steps the fiber synchronously, so -- unlike the scheduler-driven
 * execution path -- file imports and async (`block-on`) operations are not
 * handled; a program that blocks will not complete here.
 *
 * @param fiber a freshly-constructed fiber for the program to trace
 */
export function* traceReductions(fiber: Fiber): Generator<string> {
  const stepper = makeTraceStepper()
  const first = stepper.render(fiber)
  if (first !== undefined) yield expToString(first)
  while (!fiber.isDone()) {
    if (fiber.step().tag === 'trace') {
      const e = stepper.render(fiber)
      if (e !== undefined) yield expToString(e)
    }
  }
  if (fiber.lastResult !== null) {
    const e = stepper.final(fiber.lastResult)
    if (e !== undefined) yield expToString(e)
  }
}

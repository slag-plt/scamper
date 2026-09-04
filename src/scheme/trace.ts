import { Fiber } from '../lpm/fiber.js'
import { Value } from '../lpm/lang.js'
import { Exp, expToString, mkLit } from './ast.js'
import { sugarExpr } from './sugar.js'
import { raiseFiber } from './raise.js'

// The fiber's current state as a user-visible reduction expression (with its
// rendering), or undefined to skip. A step is hidden while any hidden frame is
// on the stack -- a call into a library/import function reduces atomically, so
// its internals (and any user callback it drives) never surface -- while the
// user's own module/local functions, including recursive calls, are stepped
// into. The shield covers the call's whole dynamic extent, tail calls included:
// a replacement frame inherits it (see Frame.hidden). Internal `##...##` states
// are never surfaced either.
function visibleReduction(fiber: Fiber): { exp: Exp; str: string } | undefined {
  // No frames means we're at a statement boundary with nothing to raise.
  if (fiber.frames.length === 0 || fiber.frames.some((f) => f.hidden)) {
    return undefined
  }
  const exp = sugarExpr(raiseFiber(fiber))
  const str = expToString(exp)
  return str.includes('##') ? undefined : { exp, str }
}

/**
 * Builds a stateful reduction stepper -- the shared source of truth for the
 * step-by-step trace. `render` raises + sugars the fiber's current state to the
 * next user-visible reduction (deduping consecutive repeats); `final` renders a
 * completed statement's value as its last step. Structurally a
 * `FiberTraceStepper` (see src/lpm/raiser.ts), so the scheduler can drive this
 * policy -- for a batch trace (`scamper --trace`) or the IDE's step mode --
 * without importing src/scheme.
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

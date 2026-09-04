import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import { Loc } from '../../src/lpm/range'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
import { DEFAULT_TRACE_STEP_LIMIT } from '../../src/lpm/output/trace-collector'

beforeAll(async () => {
  await initialize()
})

// Regression for #369: stepping a statement that never finishes hung the IDE
// with no way out. A trace is gathered in full before it is shown, and each
// step raises the whole fiber to an expression, so a runaway recursion costs
// O(steps x depth) -- the ceiling was 10,000 steps, by which point one step
// alone renders to megabytes and collecting them takes the best part of a
// minute. Collection must stop at a step limit low enough to come back at once,
// and `traceStatement` must apply that limit when the caller names no other.

// `(factorial -1)` never reaches the base case, and each call nests inside the
// `*` of the one before it, so the state grows without bound (the issue's own
// example).
const RUNAWAY =
  '(define factorial\n' +
  '  (lambda (n)\n' +
  '    (if (zero? n) 1 (* n (factorial (- n 1))))))\n' +
  '(factorial -1)\n'

describe('tracing a statement that never finishes (#369)', () => {
  test('stops at the default step limit instead of running on', async () => {
    const result = await Scamper.getInstance().traceStatement({
      src: RUNAWAY,
      cursorLoc: new Loc(1, 1, RUNAWAY.indexOf('(factorial -1)') + 1),
      err: new SimpleErrorChannel(),
    })
    expect(result).not.toBeNull()
    // Cut off rather than run to an end it does not have, and said to be so:
    // the window tells the reader the rest was dropped.
    expect(result?.truncated).toBe(true)
    // Exactly the limit, which is what says collection stopped *because* of it
    // rather than wandering off somewhere else. Deliberately not a wall-clock
    // assertion: how long 2,500 steps take is a property of the machine, and a
    // bound tight enough to mean anything here is one a loaded CI runner
    // trips. What guards against a regression to the old ~50s ceiling is this
    // test's own timeout -- collecting 10,000 steps cannot finish inside it.
    expect(result?.steps).toHaveLength(DEFAULT_TRACE_STEP_LIMIT)
  }, 20_000)
})

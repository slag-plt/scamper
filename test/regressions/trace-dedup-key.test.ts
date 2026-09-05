import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import { Loc } from '../../src/lpm/range'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
import { isStructKind } from '../../src/lpm/util'
import type { Value } from '../../src/lpm/lang'
import type { TraceOutput, TraceStart } from '../../src/lpm/trace'
import { type Exp, expToLayout, expToString } from '../../src/scheme/ast'
import { renderToString } from '../../src/scheme/pretty'

beforeAll(async () => {
  await initialize()
})

// Regression for #494: collecting a trace is quadratic, because every step
// rebuilds the whole frame stack and flattens it to a string purely to
// deduplicate against the step before it (src/scheme/trace.ts). Any fix for
// that cost is a change to how two steps are *compared*, and the comparison is
// what decides which steps a student sees -- a coarser key merges reductions
// away, a finer one splits one reduction into two.
//
// So what this pins is not the cost but the content: the exact reductions of a
// few programs, and the equality the dedup key rests on. It is deliberately
// *not* a timing assertion. #369 already tried one and withdrew it -- how long
// a trace takes is a property of the machine, and a bound tight enough to catch
// a regression is one a loaded CI runner trips. A faster key that changed a
// trace by one step would pass a stopwatch and fail here, which is the way
// round that matters.

/** The reduction a collected step carries, whichever kind of step it is. */
function stepExp(v: Value): Exp {
  if (
    isStructKind<TraceStart>(v, 'trace-start') ||
    isStructKind<TraceOutput>(v, 'trace-output')
  ) {
    const output = v.output
    if (output === undefined) throw new Error('a trace step with no reduction')
    return output as Exp
  }
  throw new Error('a collected step that is not a reduction')
}

/**
 * The reductions the IDE would show for `statement` in `src`.
 *
 * Through `traceStatement` rather than the harness's `reductionTrace` because
 * that is the path #494 is about: the IDE gathers a whole trace before showing
 * any of it, so it pays the per-step cost n times over.
 */
async function traceSteps(src: string, statement: string): Promise<Exp[]> {
  const result = await Scamper.getInstance().traceStatement({
    src,
    cursorLoc: new Loc(1, 1, src.indexOf(statement) + 1),
    err: new SimpleErrorChannel(),
  })
  if (result === null) throw new Error('no statement under the cursor')
  expect(result.truncated).toBe(false)
  return result.steps.map(stepExp)
}

const FACTORIAL =
  '(define factorial\n' +
  '  (lambda (n)\n' +
  '    (if (zero? n) 1 (* n (factorial (- n 1))))))\n'

describe('the trace dedup key decides trace content (#494)', () => {
  test('a recursive call reduces through exactly these steps', async () => {
    // A trace long enough, and nested deeply enough, that a changed key shows
    // up: this is the shape whose growing state makes collection quadratic in
    // the first place, only stopping at a base case.
    const steps = await traceSteps(
      FACTORIAL + '(factorial 4)\n',
      '(factorial 4)',
    )
    expect(steps.map(expToString)).toEqual([
      '(factorial 4)',
      '(if (zero? 4) 1 (* 4 (factorial (- 4 1))))',
      '(if #f 1 (* 4 (factorial (- 4 1))))',
      '(* 4 (factorial (- 4 1)))',
      '(* 4 (factorial 3))',
      '(* 4 (if (zero? 3) 1 (* 3 (factorial (- 3 1)))))',
      '(* 4 (if #f 1 (* 3 (factorial (- 3 1)))))',
      '(* 4 (* 3 (factorial (- 3 1))))',
      '(* 4 (* 3 (factorial 2)))',
      '(* 4 (* 3 (if (zero? 2) 1 (* 2 (factorial (- 2 1))))))',
      '(* 4 (* 3 (if #f 1 (* 2 (factorial (- 2 1))))))',
      '(* 4 (* 3 (* 2 (factorial (- 2 1)))))',
      '(* 4 (* 3 (* 2 (factorial 1))))',
      '(* 4 (* 3 (* 2 (if (zero? 1) 1 (* 1 (factorial (- 1 1)))))))',
      '(* 4 (* 3 (* 2 (if #f 1 (* 1 (factorial (- 1 1)))))))',
      '(* 4 (* 3 (* 2 (* 1 (factorial (- 1 1))))))',
      '(* 4 (* 3 (* 2 (* 1 (factorial 0)))))',
      '(* 4 (* 3 (* 2 (* 1 (if (zero? 0) 1 (* 0 (factorial (- 0 1))))))))',
      '(* 4 (* 3 (* 2 (* 1 (if #t 1 (* 0 (factorial (- 0 1))))))))',
      '(* 4 (* 3 (* 2 (* 1 1))))',
      '(* 4 (* 3 (* 2 1)))',
      '(* 4 (* 3 2))',
      '(* 4 6)',
      '24',
    ])
  })

  test('a let, a cond, and a library call reduce through exactly these steps', async () => {
    // Three shapes the recursive trace does not cover: a binder filled in
    // place, a form whose clauses are peeled off one at a time, and a call into
    // the library, which reduces atomically however many steps it takes.
    expect(
      (await traceSteps('(let ([x (+ 1 2)]) (* x x))\n', '(let')).map(
        expToString,
      ),
    ).toEqual([
      '(let ([x (+ 1 2)]) (* x x))',
      '(let ([x 3]) (* x x))',
      '(* 3 3)',
      '9',
    ])
    expect(
      (await traceSteps('(cond [(< 5 1) 1] [(< 1 5) 2])\n', '(cond')).map(
        expToString,
      ),
    ).toEqual([
      '(cond [(< 5 1) 1] [(< 1 5) 2])',
      '(cond [#f 1] [(< 1 5) 2])',
      '(cond [(< 1 5) 2])',
      '(cond [#t 2])',
      '2',
    ])
    expect(
      (
        await traceSteps('(map (lambda (x) (* x x)) (list 1 2 3))\n', '(map')
      ).map(expToString),
    ).toEqual(['(list 1 4 9)'])
  })

  test('a step that renders as the one before it is dropped', async () => {
    // What the key is *for*. `(define x 5)` reaches its value and then reports
    // it, two states that render alike, and the trace shows one step. A key
    // that told them apart would double every such pair.
    const steps = await traceSteps('(define x 5)\n', '(define')
    expect(steps.map(expToString)).toEqual(['5'])
  })

  test('the key is the flat text the printer would produce', async () => {
    // The invariant a cheaper key has to keep. `expToString` is the dedup key
    // *and* the larger of the two costs a step pays, so it is the natural thing
    // to make faster -- by skipping the layout planner, memoising, or hashing.
    // Whatever it does, it has to answer what the printer answers, since that
    // text is what "the same step" means here.
    const steps = await traceSteps(
      FACTORIAL + '(factorial 4)\n',
      '(factorial 4)',
    )
    for (const step of steps) {
      expect(expToString(step)).toBe(
        renderToString(expToLayout(step), Infinity, 'flat'),
      )
    }
  })
})

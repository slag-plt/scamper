import { beforeEach, describe, expect, test, vi } from 'vitest'
import * as Scheme from '../../src/scheme'
import { LoggingChannel } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { Frame } from '../../src/lpm/frame'
import { runFiberOnScheduler } from '../../src/lpm/run'
import { expToString } from '../../src/scheme/ast'
import {
  type Spine,
  plug,
  raiseFiber,
  raiseFiberSpine,
  raiseFrameContext,
} from '../../src/scheme/raise'
import { sugarExpr } from '../../src/scheme/sugar'
import { makeTraceStepper } from '../../src/scheme/trace'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'

// The differential harness for #494.
//
// src/scheme/raise.ts now reconstructs a paused machine two ways: `raiseFiber`
// rebuilds the whole frame stack into one expression (what the tracer uses
// today), and `raiseSpine` keeps the stack's shape -- each outer frame as a
// context with a hole in it -- which `plug` folds back together. The second is
// what stages 4 and 5 will build on; this file is the evidence that it is the
// same reconstruction.
//
// It runs every program below on a real scheduler and, after *every* fiber
// step, asserts three things:
//
//   1. THE DIFFERENTIAL. `expToString(plug(spine, inner))` is
//      `expToString(raiseFiber(fiber))` -- the decomposition loses nothing.
//   2. SUGARING IS COMPOSITIONAL over the hole:
//      `sugar(plug(ctx, e)) === plug(sugar(ctx), sugar(e))`. This was the
//      design's biggest unknown: `collectAnd`/`collectOr`/`collectCond`/
//      `flattenBegin` (src/scheme/sugar.ts) walk *else*/*body* positions, and
//      a hole only ever sits at a *value* position, so sugaring a context on
//      its own should recover exactly what sugaring the whole does. If that is
//      false anywhere, the trace shows an un-sugared `if` chain where a `cond`
//      belongs -- and stage 5, which sugars a context once instead of once per
//      step, is not safe to write.
//   3. THE PROMISE `Frame.version` MAKES: a frame that still has the same
//      identity and the same version reconstructs the same way. That is what
//      lets stage 4 keep an outer frame's context across steps instead of
//      rebuilding it, and it is the only thing checking the four bump sites in
//      src/lpm/fiber.ts. Checked over *every* frame on the stack, as its
//      context (`raiseFrameContext`, which reads exactly the `values`, `ops`
//      and `env` a version claims to cover) -- including the top one, which is
//      where the bumps in stepFrame and resumeWithValue show up.
//
// Nothing here changes what a student sees: the tracer still calls
// `raiseFiber`, and this file is the only caller of the new shape.

/**
 * `spine` with every context sugared in place.
 *
 * The hole survives: it is an identifier, and `sugarExpr` returns an
 * identifier unchanged (by identity, which is what `plug` matches on).
 */
function sugarSpine(spine: Spine): Spine {
  return spine === null
    ? null
    : { ...spine, ctx: sugarExpr(spine.ctx), rest: sugarSpine(spine.rest) }
}

/**
 * Remembers how each frame rendered at each version it was seen at, and
 * complains if the same (frame, version) ever renders two different ways.
 */
class VersionWatch {
  private readonly seen = new Map<Frame, { version: number; ctx: string }>()

  /** How many times a remembered rendering was actually re-checked. */
  hits = 0

  check(frame: Frame, where: string): void {
    const ctx = expToString(raiseFrameContext(frame, null).ctx)
    const rec = this.seen.get(frame)
    if (rec?.version !== frame.version) {
      this.seen.set(frame, { version: frame.version, ctx })
      return
    }
    this.hits++
    expect(
      ctx,
      `${where}: frame ${frame.name} is unchanged (version ` +
        `${frame.version.toString()}) but reconstructs differently`,
    ).toBe(rec.ctx)
  }
}

/** What one program's run observed. */
interface Tally {
  steps: number
  maxDepth: number
}

const watch = new VersionWatch()
const totals: Tally = { steps: 0, maxDepth: 0 }

/** Called at each observation, for the tests that want a state to look at. */
type Observer = (fiber: Fiber) => void

function compareStep(
  fiber: Fiber,
  src: string,
  tally: Tally,
  observe?: Observer,
): void {
  // No frames means a statement boundary, with nothing to reconstruct.
  if (fiber.frames.length === 0) {
    return
  }
  tally.steps++
  tally.maxDepth = Math.max(tally.maxDepth, fiber.frames.length)
  const where = `${JSON.stringify(src)} @ step ${tally.steps.toString()}`

  const { spine, inner } = raiseFiberSpine(fiber)
  const whole = raiseFiber(fiber)

  // (1) the differential
  expect(expToString(plug(spine, inner)), where).toBe(expToString(whole))

  // (2) sugaring is compositional over the hole
  expect(
    expToString(plug(sugarSpine(spine), sugarExpr(inner))),
    `${where} (sugared)`,
  ).toBe(expToString(sugarExpr(whole)))

  // (3) the promise Frame.version makes
  for (const frame of fiber.frames) {
    watch.check(frame, where)
  }

  observe?.(fiber)
}

/**
 * Compiles and traces `src` on a real scheduler -- so imports, blocking
 * primitives and error recovery all take their usual route -- comparing the two
 * reconstructions after every step of the fiber.
 */
async function differential(src: string, observe?: Observer): Promise<Tally> {
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  expect(diagnostics.map((d) => d.message), src).toEqual([])
  if (prog === undefined) {
    throw new Error(`compile produced no program for ${src}`)
  }
  const fiber = new Fiber(prog, Scheme.mkInitialEnv())
  const tally: Tally = { steps: 0, maxDepth: 0 }
  // The escape hatch for observing the machine *between* steps; the scheduler
  // exposes only the trace policy's own (deduplicated, shielded) view.
  const realStep = fiber.step.bind(fiber)
  vi.spyOn(fiber, 'step').mockImplementation(() => {
    const result = realStep()
    compareStep(fiber, src, tally, observe)
    return result
  })
  // A blocking primitive suspends the fiber *inside* a step, which throws out
  // past the spy above, and the scheduler delivers the result later through
  // resumeWithValue. Observing on both sides of it is the only way to see that
  // state at all -- and the only thing that checks its version bump.
  const realResume = fiber.resumeWithValue.bind(fiber)
  vi.spyOn(fiber, 'resumeWithValue').mockImplementation((value) => {
    compareStep(fiber, src, tally, observe)
    realResume(value)
    compareStep(fiber, src, tally, observe)
  })
  const out = new LoggingChannel(false, false)
  await runFiberOnScheduler(fiber, {
    out,
    err: out,
    isTracing: true,
    stepper: makeTraceStepper(),
  })
  totals.steps += tally.steps
  totals.maxDepth = Math.max(totals.maxDepth, tally.maxDepth)
  return tally
}

// The programs the trace suite is built out of, gathered from where they live
// rather than invented: test/scheme/tracing.test.ts, test/lpm/trace-statement
// .test.ts, test/scheme/step-mode.test.ts, test/regressions/trace-dedup-key
// .test.ts, tail-call-trace-shield, step-into-recursive-calls,
// anonymous-lambda-trace-steps, traced-define-value, test/scheme/hole.test.ts,
// and the `--trace` fixtures of test/apps/cli/cli.test.ts.
const PROGRAMS: Record<string, string[]> = {
  'the core forms, one per shape': [
    '42',
    '(+ 1 2)',
    '(+ 1 (* 2 3))',
    '(+ (* 2 3) (- 10 6))',
    '(if (< 1 2) 10 20)',
    '(let ([x 2] [y 3]) (+ x y))',
    '(let ([x (+ 1 2)]) (* x x))',
    '(match (list 1 2) [(cons a b) a] [_ 0])',
    '(define x 5)\n(display "hi")\n(+ x 1)',
    '(+ 1 2)\n(* 3 4)',
    '(define z (+ 1 2))',
    '(define sq (lambda (x) (* x x)))\n(sq 4)',
    '(define sqr (lambda (x) (* x x)))\n(define y (sqr (+ 2 3)))',
    '(define h (lambda (x & rest) x))\n(h 1 2 3)',
    '(struct point (x y))\n(point-x (point 1 (+ 1 1)))',
  ],
  // Every derived form, which is what makes (2) worth asserting: each of these
  // is an `if`/`let` chain underneath, recovered only from its provenance.
  'derived forms, whose sugaring is the property under test': [
    '(cond [(< 5 1) 1] [(< 1 5) 2])',
    '(cond [(< 5 1) 1] [(< 1 5) (cond [#t (and #t (or #f #t))])])',
    '(and (< 1 2) (< 3 4))',
    '(or (< 5 1) (< 3 4))',
    '(and (or #f (< 1 2)) (begin 1 (< 3 4)))',
    '(begin (+ 1 1) (+ 2 2))',
    '(begin (+ 1 1) (begin (+ 2 2) (+ 1 2)))',
    '[1 (+ 1 1)]',
    '[(+ 1 1) [2 (* 2 2)]]',
    '{"a" (+ 1 1)}',
    '{"a" (+ 1 1) "b" {"c" (* 2 2)}}',
    '(#(* % 2) (+ 1 2))',
    '#(+ %1 %2)',
    // A derived form *inside a call that is still running*: the `cond` sits in
    // the caller's context while the callee reduces, so the context is what has
    // to sugar on its own.
    '(define f (lambda (n) (* n 2)))\n(cond [#f 0] [#t (+ 1 (f 3))])',
    '(define f (lambda (n) (* n 2)))\n(and #t (+ 1 (f 3)))',
    '(define f (lambda (n) (* n 2)))\n(begin 1 (+ 1 (f 3)))',
    '(define f (lambda (n) (* n 2)))\n[1 (f 3)]',
    '(define f (lambda (n) (* n 2)))\n{"k" (f 3)}',
  ],
  // Where the property is tested hardest. A context's hole only ever lands at
  // a position `raiseFrame` fills from the value stack -- an `if` guard, an
  // `ap` head or operand, a `match` scrutinee, a `let` binding in flight -- and
  // the claim is that those are never the *else*/*body* positions that
  // `collectAnd`/`collectOr`/`collectCond`/`flattenBegin` recurse through. So
  // each program below parks a call in one of them and leaves the rest of the
  // derived form still to run, which is the shape that would sugar wrong.
  'a hole at each position a derived form is collected through': [
    // the head of a cond chain, with clauses still behind it
    '(define p (lambda (n) (> n 1)))\n(cond [(p 0) "a"] [(p 3) "b"] [else "c"])',
    // ...and of an and/or chain
    '(define p (lambda (n) (> n 1)))\n(and (p 2) (p 3) #t)',
    '(define p (lambda (n) (> n 1)))\n(or (p 0) (p 3))',
    // a derived form nested in the branch of one that is still reducing
    '(define p (lambda (n) (> n 1)))\n(cond [(p 0) 1] [else (and (p 2) (or (p 0) #t))])',
    // a begin whose first expression is the one in flight, so the hole sits in
    // the wildcard binding flattenBegin walks
    '(define f (lambda (n) (* n 2)))\n(begin (+ 1 (f 3)) 2)',
    '(define f (lambda (n) (* n 2)))\n(begin (f 1) (begin (f 2) (f 3)))',
    // a plain if guard, a match scrutinee, a let binding, and an application
    // head -- the other four stack-filled positions
    '(define f (lambda (n) (* n 2)))\n(if (zero? (f 0)) 1 2)',
    '(define f (lambda (n) (* n 2)))\n(match (f 3) [0 "zero"] [k k])',
    '(define f (lambda (n) (* n 2)))\n(let ([x (f 3)] [y 2]) (+ x y))',
    '(define f (lambda (n) (lambda (x) (* n x))))\n((f 3) 4)',
    // and the same hole inside a vector/map literal that is itself inside a cond
    '(define f (lambda (n) (* n 2)))\n(cond [#t [1 (f 3)]])',
    '(define f (lambda (n) (* n 2)))\n(and #t {"k" (f 3)})',
  ],
  // The quadratic case #494 is about: a non-tail recursion whose stack grows a
  // frame per call, so the spine is deep and rebuilt on every step.
  'deep non-tail recursion': [
    '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n(fact 9)',
    '(define factorial\n  (lambda (n)\n    (if (zero? n) 1 (* n (factorial (- n 1))))))\n(factorial 4)',
    '(define len (lambda (l) (if (null? l) 0 (+ 1 (len (cdr l))))))\n(len (list 1 2 3 4 5 6 7 8))',
    '(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))\n(fib 5)',
    '(define f (lambda (n) (match n [0 "zero"] [k (* k (f (- k 1)))])))\n(f 4)',
    '(define f (lambda (n) (cond [(zero? n) 1] [else (* n (f (- n 1)))])))\n(f 4)',
  ],
  // Tail recursion, which replaces the caller's frame instead of pushing one,
  // so the stack stays one deep however many calls it makes.
  'tail recursion (depth 1)': [
    '(define go (lambda (i acc) (if (> i 5) acc (go (+ i 1) (+ acc i)))))\n(go 1 0)',
    // The binder is filled in place while the *outer* frame still holds it,
    // which is why `env` has to be versioned alongside `values` and `ops`.
    '(let ([go (lambda (i acc) (if (> i 4) acc (go (+ i 1) (+ acc i))))])\n  (go 1 0))',
    '(let ([go (lambda (i) (if (> i 3) 0 (+ 1 (go (+ i 1)))))])\n  (go 0))',
  ],
  // A lambda with no name a student wrote -- new since #569, which is what put
  // the lambda itself (rather than `##anonymous##`) into a step.
  'anonymous lambdas': [
    '((lambda (a b) (+ (* 2 a) (* 3 b))) (+ 1 2) (+ 3 (* 4 5)))',
    '(define f (lambda (x) x))',
    '(define mk (lambda (n) (lambda (x) (+ x n))))\n(mk 5)',
    '(define k 10)\n(define mk (lambda () (lambda () k)))\n(mk)',
    '(define n 3)\n(map (lambda (x) (+ x n)) (list 1 2))',
    '((lambda (f) (f (f 2))) (lambda (x) (* x x)))',
  ],
  // A `??` in the reconstruction, which must stay distinguishable from the
  // hole a context carries.
  'holes': [
    '(if #t 1 ??)',
    '(+ (* 2 3) ??)',
    '(define f (lambda (x) ??))\n(f 1)',
    '(define f (lambda (x) (+ x ??)))\n(+ 1 (f 2))',
    '(match 1 [_ ??])',
    '(and #f ??)',
    '[1 ??]',
    '(with-handler (lambda (msg) msg) (lambda () ??))',
  ],
  // Calls into library code, which a trace steps over but a reconstruction
  // still has to rebuild, plus the error path (handleError unwinds the stack).
  'library calls and error recovery': [
    '(map (lambda (x) (* x x)) (list 1 2 3))',
    '(fold + 0 (list 1 2 3))',
    '(define double (lambda (x) (* x 2)))\n(map double (list 1 2 3))',
    '(define sum-doubled\n  (lambda (xs) (fold + 0 (map (lambda (x) (* x 2)) xs))))\n(sum-doubled (list 1 2 3))',
    '(define double (lambda (x) (* x 2)))\n(define f (lambda (xs) (map double xs)))\n(f (list 1 2 3))',
    '(with-handler (lambda (msg) msg) (lambda () (car 5)))',
    '(define boom (lambda (n) (+ n (car 5))))\n(with-handler (lambda (m) m) (lambda () (+ 1 (boom 2))))',
    '(display 1)\n(car 5)\n(display 3)',
  ],
}

describe('the spine reconstruction agrees with raiseFiber (#494)', () => {
  for (const [group, programs] of Object.entries(PROGRAMS)) {
    describe(group, () => {
      for (const src of programs) {
        test(JSON.stringify(src), async () => {
          const tally = await differential(src)
          // A program that took no step compared nothing, which would make its
          // case vacuous.
          expect(tally.steps).toBeGreaterThan(0)
        })
      }
    })
  }
})

// Two programs that need a file system: an import (whose frames are hidden, and
// whose tail call replaces one) and `with-file`, the only shipped primitive
// that suspends the fiber mid-expression and comes back through
// Fiber.resumeWithValue -- one of the four places a frame's version is bumped.
describe('programs that reach the scheduler (#494)', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = await MockFileSystem.create()
    setBackend(localBackend(fs))
  })

  test('an imported file, tail-calling a worker it built', async () => {
    await fs.saveFile(
      'lib.scm',
      '(define-export sum-to\n' +
        '  (lambda (n)\n' +
        '    (let ([go (lambda (i acc) (if (> i n) acc (go (+ i 1) (+ acc i))))])\n' +
        '      (go 1 0))))\n',
    )
    const tally = await differential('(import "lib.scm")\n(sum-to 3)')
    expect(tally.steps).toBeGreaterThan(0)
  })

  test('with-file, which suspends the fiber and resumes it', async () => {
    await fs.saveFile('data.txt', 'hello')
    const tally = await differential(
      '(define twice-length (lambda (s) (* (string-length s) 2)))\n' +
        '(with-file "data.txt" twice-length)',
    )
    expect(tally.steps).toBeGreaterThan(0)
  })
})

// The choice `contextHole` makes, pinned. `plug` matches its hole by object
// identity, so it would *work* with any node -- but everything downstream reads
// the rendering, and a context that leaked into a trace step has to be
// recognisable as an internal rather than as code the student wrote. (It is:
// `visibleReduction` in src/scheme/trace.ts drops any step whose text contains
// `##`, so a leak is an invisible step rather than a bogus `??`.)
describe("a context's hole is not the student's ?? (#494)", () => {
  test('both appear in one context, spelled differently', async () => {
    // `(f 3)` is running, so the statement frame's context has a hole where its
    // value goes -- and a `??` of the program's own, still unevaluated, beside
    // it.
    const seen: string[] = []
    await differential('(define f (lambda (x) (* x 2)))\n(+ (f 3) ??)', (fiber) => {
      const { spine } = raiseFiberSpine(fiber)
      if (spine !== null) {
        seen.push(expToString(spine.ctx))
      }
    })
    const both = seen.filter((t) => t.includes('??'))
    expect(both.length).toBeGreaterThan(0)
    for (const text of both) {
      // The student's hole is still `??`...
      expect(text).toContain('??')
      // ...and the context's is an internal name, which no program may write.
      expect(text).toContain('##')
    }
  })
})

// A guard on the harness itself. Every assertion above lives inside a loop over
// the machine's states, so a harness that quietly stopped observing -- a spy
// that never fired, a spine that was always empty -- would pass every test in
// this file while checking nothing.
describe('the harness observed enough to mean something', () => {
  test('it compared thousands of states, at real stack depth', () => {
    expect(totals.steps).toBeGreaterThan(2000)
    expect(totals.maxDepth).toBeGreaterThan(9)
  })

  test('and re-checked frames it had already rendered', () => {
    // Without this the version promise (3) is never actually tested: it only
    // says anything when a (frame, version) pair comes round a second time.
    expect(watch.hits).toBeGreaterThan(1000)
  })
})

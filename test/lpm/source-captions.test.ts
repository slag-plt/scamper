import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'
import { makeTraceStepper } from '../../src/scheme/trace'
import { ScamperError } from '../../src/lpm/error'
import type { OutputChannel, ErrorChannel, Value } from '../../src/lpm'

/**
 * A channel that keeps output and source captions in the order they arrived,
 * so a test can see which statement each value was attributed to.
 */
class CaptioningChannel implements OutputChannel, ErrorChannel {
  readonly log: ({ kind: 'value' } | { kind: 'source'; text: string })[] = []
  totalSends = 0

  send(_v: Value) {
    this.log.push({ kind: 'value' })
    this.totalSends++
  }

  pushLevel() {
    /* sections are not what these tests are about */
  }

  popLevel() {
    /* as above */
  }

  beginStatement(source: string) {
    this.log.push({ kind: 'source', text: source })
  }

  report(_e: ScamperError) {
    this.log.push({ kind: 'value' })
  }
}

/** Runs `src` and returns the captions and values it produced, in order. */
async function runCaptioned(src: string, isTracing = false) {
  const out = new CaptioningChannel()
  const { prog, diagnostics } = await Scheme.compile(src)
  expect(diagnostics).toEqual([])
  if (prog === undefined) throw new Error('no program compiled')
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
    src,
    isTracing,
    // A traced run emits nothing without one, so the many-blocks case below
    // would otherwise look identical to an untraced run.
    stepper: isTracing ? makeTraceStepper() : undefined,
  })
  return out.log
}

/** Just the caption texts, in order. */
function captions(log: Awaited<ReturnType<typeof runCaptioned>>): string[] {
  return log.flatMap((e) => (e.kind === 'source' ? [e.text] : []))
}

// Output can be captioned with the statement that produced it. The captions are
// always emitted; whether they are shown is the output pane's business.
describe('source captions', () => {
  test('each displayed value is preceded by its own statement', async () => {
    const log = await runCaptioned('(display 1)\n(display (+ 2 3))')
    expect(captions(log)).toEqual(['(display 1)', '(display (+ 2 3))'])
    // Caption first, then the value it describes.
    expect(log.map((e) => e.kind)).toEqual(['source', 'value', 'source', 'value'])
  })

  test('a statement spanning several lines is captioned whole', async () => {
    const src = '(display\n  (+ 1\n     2))'
    expect(captions(await runCaptioned(src))).toEqual([src])
  })

  test('a statement that displays nothing is captioned all the same', async () => {
    // A define prints nothing, but it is still code the person wrote and still
    // belongs in the record of what ran.
    const log = await runCaptioned('(define x 5)\n(display x)')
    expect(captions(log)).toEqual(['(define x 5)', '(display x)'])
    expect(log.map((e) => e.kind)).toEqual(['source', 'source', 'value'])
  })

  test('consecutive silent statements each get their own caption', async () => {
    const log = await runCaptioned(
      '(import image)\n(define x 5)\n(define y 6)\n(display (+ x y))',
    )
    expect(captions(log)).toEqual([
      '(import image)',
      '(define x 5)',
      '(define y 6)',
      '(display (+ x y))',
    ])
  })

  test('silent statements at the end of a program are captioned too', async () => {
    // Nothing follows them to drag their captions out on the way past, so the
    // end of the run has to flush them.
    const log = await runCaptioned('(display 1)\n(define x 5)\n(define y 6)')
    expect(captions(log)).toEqual(['(display 1)', '(define x 5)', '(define y 6)'])
    expect(log.map((e) => e.kind)).toEqual(['source', 'value', 'source', 'source'])
  })

  test('a program that prints nothing at all is still captioned', async () => {
    expect(captions(await runCaptioned('(define x 5)'))).toEqual(['(define x 5)'])
  })

  test('a statement producing many blocks is captioned once', async () => {
    // A traced run emits a reduction per step; they share the one caption
    // rather than repeating it down the whole trace.
    const log = await runCaptioned('(display (+ 1 (* 2 3)))', true)
    expect(log.filter((e) => e.kind === 'value').length).toBeGreaterThan(1)
    expect(captions(log)).toEqual(['(display (+ 1 (* 2 3)))'])
  })

  test('a bare top-level expression is captioned', async () => {
    // The form most student code is made of, and the one that was silently
    // uncaptioned: expansion used to drop the range off a `stmtexp`, leaving
    // it with no source location for the caption to be cut from.
    expect(captions(await runCaptioned('(+ 1 2)'))).toEqual(['(+ 1 2)'])
  })

  test('a bare expression after an import is captioned', async () => {
    // What a real program looks like, and how the missing range was found.
    const src = '(import image)\n(+ 1 2)\n'
    expect(captions(await runCaptioned(src))).toEqual([
      '(import image)',
      '(+ 1 2)',
    ])
  })

  test('an error is captioned with the statement that raised it', async () => {
    const log = await runCaptioned('(display 1)\n(display (car 5))')
    expect(captions(log)).toEqual(['(display 1)', '(display (car 5))'])
  })

  test('a run given no source emits no captions', async () => {
    const out = new CaptioningChannel()
    const src = '(display 1)'
    const { prog } = await Scheme.compile(src)
    if (prog === undefined) throw new Error('no program compiled')
    await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
      out,
      err: out,
    })
    expect(captions(out.log)).toEqual([])
    expect(out.totalSends).toBe(1)
  })
})

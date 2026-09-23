import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme'
import { CodeOrigin, LoggingChannel } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/553
//
// The natives a library definition names at top level are reached *behind*
// their contract wrappers (see VarHandler, #476), so the docstring -- which is
// the only type check most natives have -- never runs on those calls. Each one
// now re-narrows its own arguments, in the style prelude_car has used since
// #476: a typed check raising a ScamperError that names the procedure.
//
// `js-var` is exported to user programs (src/lib/index.ts), so `((js-var
// "prelude_minus"))` is legal student code reaching the raw native with no
// contract at all. That is how every guard below is reached, and it is why
// none of this is unreachable code.

/** What the raw native does with `args`, contract and all bypassed. */
function callRaw(native: string, args = ''): Promise<string[]> {
  return runProgram(`((js-var "${native}") ${args})`, { stripRanges: true })
}

describe('#553: each live native re-narrows what its contract narrows', () => {
  test.for([
    // [native, arguments, message, what it did before the guard]
    ['canvas_canvasWidth', '5', 'canvas-width: expected a canvas', 'undefined'],
    ['canvas_canvasHeight', '5', 'canvas-height: expected a canvas', 'undefined'],
    ['canvas_canvasToPixels', '5', 'canvas->pixels: expected a canvas', 'a TypeError from getContext'],
    ['canvas_pixelsToCanvas', '(vector 5) 1 1', 'pixels->canvas: expected a vector of rgb values', 'an all-black, fully transparent canvas'],
    ['canvas_pixelsToCanvas', '(vector) "a" 1', 'pixels->canvas: expected an integer width and height', 'a 0x0 canvas'],
    ['prelude_lt', '"a" "b"', '<: expected numbers', '#t, from Javascript string comparison'],
    ['prelude_leq', '"a" "b"', '<=: expected numbers', '#t'],
    ['prelude_gt', '"b" "a"', '>: expected numbers', '#t'],
    ['prelude_plus', '"a" 1', '+: expected numbers', '"0a1", by string concatenation'],
    ['prelude_minus', '', '-: expected at least 1 argument', 'TypeError: Reduce of empty array'],
    ['prelude_minus', '"a"', '-: expected numbers', 'NaN'],
    ['prelude_div', '', '/: expected at least 1 argument', 'TypeError: Reduce of empty array'],
    ['prelude_length', '5', 'length: expected a list', 'a TypeError, after walking off a non-list'],
    ['prelude_reverse', '5', 'reverse: expected a list', 'a TypeError'],
    ['prelude_quotient', '"a" 2', 'quotient: expected numbers', 'NaN'],
    ['prelude_makeVector', '"3" 0', 'make-vector: expected an integer', 'the empty vector'],
    ['prelude_listTake', '5 1', 'list-take: expected a list', 'a list of voids'],
    ['prelude_listTake', 'null "a"', 'list-take: expected an integer', 'null'],
    // One native behind both list-tail and list-drop, so it names the former
    // whichever binding reached it (#649).
    ['prelude_listTail', '5 1', 'list-tail: expected a list', 'void'],
    ['prelude_listTail', 'null "a"', 'list-tail: expected an integer', 'null'],
    ['prelude_listToVector', '5', 'list->vector: expected a list', 'a TypeError'],
    ['prelude_vectorToList', '"abc"', 'vector->list: expected a vector', 'a list of one-character strings'],
    ['prelude_vectorLength', '"abc"', 'vector-length: expected a vector', '3 -- a string has a length too'],
    ['prelude_vectorRef', '"abc" 1', 'vector-ref: expected a vector', '"b" -- the bounds check passed'],
    ['prelude_vectorRef', '(vector 1 2) "1"', 'vector-ref: expected an integer', '2, indexed by the string'],
    ['prelude_vectorSet', '"abc" 1 5', 'vector-set!: expected a vector', 'void, having written nothing'],
    ['prelude_stringToList', '5', 'string->list: expected a string', 'null'],
    ['prelude_vectorLength', '', 'vector-length: expected a vector', 'a TypeError'],
    ['test_testResultOk', '5', 'test-result-ok: expected a string', 'a result whose name is a number'],
    ['test_testResultErrorExpected', '5 1 2', 'test-result-error-expected: expected a string', 'as above'],
    ['test_testResultErrorExn', '5 1', 'test-result-error-exn: expected a string', 'as above'],
    ['test_testResultErrorGeneric', '"d" 5', 'test-result-error-gen: expected strings', 'as above'],
  ])('%s %s', async ([native, args, message]) => {
    expect(await callRaw(native, args)).toEqual([
      `Runtime error: (${native}) ${message}`,
    ])
  })
})

describe('#553: the guards do not cost the calls that were always right', () => {
  test('the library still works through every guarded native', async () => {
    expect(
      await runProgram(`
(import image)
(import canvas)
(import test)
(sort (list 3 1 2) <)
(vector-map (lambda (x) (+ x 1)) (vector 1 2))
(vector-filter even? (vector 1 2 3 4))
(string-map char-upcase "hi")
(length (string->list "hi"))
(for-range (lambda (i) i) 0 3)
(test-result? (test-case "t" equal? 1 (lambda () 1)))
(canvas? (pixel-map (lambda (p) p) (make-canvas 2 2)))
`),
    ).toEqual([
      '(list 1 2 3)',
      '(vector 2 3)',
      '(vector 2 4)',
      '"HI"',
      '2',
      'void',
      '#t',
      '#t',
    ])
  })
})

/** Runs `src` in a fiber whose frames carry `origin`. See CodeOrigin. */
async function runAsOrigin(src: string, origin: CodeOrigin): Promise<string[]> {
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  expect(diagnostics).toEqual([])
  if (prog === undefined) {
    throw new Error('compile produced no program')
  }
  const out = new LoggingChannel(true)
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv(), origin), {
    out,
    err: out,
  })
  return out.log as string[]
}

describe('#553: the bypass mechanism itself', () => {
  // The guards above are about particular natives. This is about the reason
  // they were needed: a *builtin*-origin frame -- which is what every library
  // definition runs in -- reaches the contract target rather than the wrapper,
  // so the same source answers differently depending on who wrote it.
  test('a builtin-origin frame reaches the native, a user one the contract', async () => {
    // The student's own call: the `vector?` contract turns it away, naming the
    // argument's position. Unchanged by this work.
    expect(await runAsOrigin('(vector-length "abc")', 'user')).toEqual([
      'Runtime error [1:1-1:21]: (error) expected a vector as the first argument, received string',
    ])
    // The same call made from library code skips that check entirely. It
    // answered `3` before this fix -- a wrong value, silently. Now the native's
    // own guard catches it, and says so in its own name.
    expect(await runAsOrigin('(vector-length "abc")', 'builtin')).toEqual([
      'Runtime error: (prelude_vectorLength) vector-length: expected a vector',
    ])
  })

  test('a bypassed call that was always right is still right', async () => {
    expect(await runAsOrigin('(vector-length (vector 1 2 3))', 'builtin')).toEqual(['3'])
  })
})

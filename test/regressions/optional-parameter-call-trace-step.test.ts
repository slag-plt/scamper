import { describe, expect, test } from 'vitest'
import { reductionTrace, runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/737
//
// A trace dropped the step in which a call to a library function with optional
// parameters became its value. That function's contract wrapper binds each
// optional from its own rest parameter with a `let`, so its body ends in
// `pop-scope` ops -- and `pop-scope` is a minor step. The wrapper's frame is
// hidden, so the major `ap` step that computed the value is suppressed, and by
// the time the frame is gone (making the caller's new state visible) the step
// is a minor one the scheduler never renders.
//
// The assertions are whole step sequences: a fix that merely adds a step
// somewhere must still produce exactly these reductions.

describe('a call with optional parameters takes a trace step (#737)', () => {
  test("the issue's own program shows substring reducing", async () => {
    expect(
      await reductionTrace(
        '(define x "Hello")\n' +
          '(string-append "[" (string-upcase x) (make-string 3 #\\.) (substring x 1) "]")',
      ),
    ).toEqual([
      '"Hello"',
      '(string-append "[" (string-upcase "Hello") (make-string 3 #\\.) (substring "Hello" 1) "]")',
      '(string-append "[" "HELLO" (make-string 3 #\\.) (substring "Hello" 1) "]")',
      '(string-append "[" "HELLO" "..." (substring "Hello" 1) "]")',
      '(string-append "[" "HELLO" "..." "ello" "]")',
      '"[HELLO...ello]"',
    ])
  })

  test('the minimal case is one such call as an argument', async () => {
    expect(
      await reductionTrace('(string-append "a" (substring "Hello" 1))'),
    ).toEqual([
      '(string-append "a" (substring "Hello" 1))',
      '(string-append "a" "ello")',
      '"aello"',
    ])
  })

  test('the argument need not be the last one', async () => {
    // The skipped step in the issue happened to be the final argument's, but
    // the defect is in how the call ends, not where the call sits.
    expect(
      await reductionTrace('(string-append (substring "Hello" 1) "a")'),
    ).toEqual([
      '(string-append (substring "Hello" 1) "a")',
      '(string-append "ello" "a")',
      '"elloa"',
    ])
  })

  test('substring is not the only such function', async () => {
    expect(await reductionTrace('(+ 1 (string->number "2"))')).toEqual([
      '(+ 1 (string->number "2"))',
      '(+ 1 2)',
      '3',
    ])
  })

  test('a nested chain gains one step per call, not one in total', async () => {
    expect(
      await reductionTrace('(substring (substring (substring "abcdefgh" 1) 1) 1)'),
    ).toEqual([
      '(substring (substring (substring "abcdefgh" 1) 1) 1)',
      '(substring (substring "bcdefgh" 1) 1)',
      '(substring "cdefgh" 1)',
      '"defgh"',
    ])
  })

  test('and the call standing alone gains no duplicate at the end', async () => {
    // The frame that completes last is the statement's own, and the state is
    // only renderable while a frame remains -- so the promoted step must not
    // produce a second copy of the final value. This is the property most
    // likely to break if the promotion is ever moved into
    // `completeCurrentFrame`, which is where the blocking-primitive path
    // would want it.
    expect(await reductionTrace('(substring "Hello" 1)')).toEqual([
      '(substring "Hello" 1)',
      '"ello"',
    ])
  })

  test('an image shape reduces to the shape it built', async () => {
    // The patch note promises the image shapes, and 36 of the 40 procedures
    // with optional parameters are in image.scm, so this is the case a
    // multimedia course actually meets. The intermediate is the image value
    // printing as its own constructor, not a library internal leaking out:
    // `square` really does build a rectangle.
    expect(
      await reductionTrace(
        '(import image)\n(image-width (square 10 "solid" "red"))',
      ),
    ).toEqual([
      '(image-width (square 10 "solid" "red"))',
      '(image-width (rectangle 10 10 "solid" (rgba 255 0 0 255)))',
      '10',
    ])
  })
})

// The promotion has exactly one consumer, the tracer, so the scheduler lets a
// promoted step take the same per-op fast path a minor step does when nobody
// is tracing (#730's ~75ns of a ~300ns step). These pin that the shortcut did
// not cost the fix: the steps still appear when a trace is actually running.
describe('promoting the step does not depend on the fast path (#737)', () => {
  test('an untraced run produces the same value', async () => {
    expect(await runProgram('(string-append "a" (substring "Hello" 1))')).toEqual([
      '"aello"',
    ])
  })

  test('and a traced run still renders the step', async () => {
    expect(await reductionTrace('(string-append "a" (substring "Hello" 1))')).toEqual([
      '(string-append "a" (substring "Hello" 1))',
      '(string-append "a" "ello")',
      '"aello"',
    ])
  })
})

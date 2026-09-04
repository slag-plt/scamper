import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { reductionTrace } from '../harness.js'

// Regression for #478: a call into library code reduces atomically, and a tail
// call must not undo that. A trace hides a step only while a non-`user` frame
// is on the stack (src/scheme/trace.ts), and a tail call *replaces* the
// caller's frame (Fiber.replaceFrame) -- so replacing the one frame that was
// doing the hiding tears the shield off partway through the call, and the rest
// of it spills into the student's trace.
//
// The fixture is an imported file rather than a prelude function: an import is
// stepped over exactly as a builtin is (see CodeOrigin), and writing it here
// keeps the test about the mechanism rather than about how `map` happens to be
// written today.
describe("#478: a tail call keeps the caller's trace shield", () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = await MockFileSystem.create()
    setBackend(localBackend(fs))
  })

  test('tail-calling a worker built at call time stays atomic', async () => {
    await fs.saveFile(
      'lib.scm',
      '(define-export sum-to\n' +
        '  (lambda (n)\n' +
        '    (let ([go (lambda (i acc) (if (> i n) acc (go (+ i 1) (+ acc i))))])\n' +
        '      (go 1 0))))\n',
    )
    expect(await reductionTrace('(import "lib.scm")\n(sum-to 3)')).toEqual([
      '(sum-to 3)',
      '6',
    ])
  })

  test("tail-calling the caller's own callback stays atomic", async () => {
    // `twice` drives the student's `double` twice: once as an argument (the
    // library's frame is still on the stack, so it is hidden) and once in tail
    // position. Both must be hidden, or the trace shows a bare `(* 6 2)` with
    // nothing to say where the 6 came from.
    await fs.saveFile(
      'lib.scm',
      '(define-export twice (lambda (f x) (f (f x))))\n',
    )
    expect(
      await reductionTrace(
        '(import "lib.scm")\n' +
          '(define double (lambda (x) (* x 2)))\n' +
          '(twice double 3)',
      ),
    ).toEqual(['(twice double 3)', '12'])
  })

  test('a tail call *into* library code does not make it visible', async () => {
    // The other direction: a shield is only ever inherited toward hiding. A
    // student's function tail-calling `map` still steps over it -- and still
    // steps into itself.
    expect(
      await reductionTrace(
        '(define double (lambda (x) (* x 2)))\n' +
          '(define f (lambda (xs) (map double xs)))\n' +
          '(f (list 1 2 3))',
      ),
    ).toEqual(['(f (list 1 2 3))', '(map double (list 1 2 3))', '(list 2 4 6)'])
  })
})

import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/592
//
// A struct accessor given the wrong type blamed the *enclosing* call rather
// than the accessor call, and named the enclosing function rather than the
// accessor:
//
//   (struct point (x y))
//   (define f (lambda (p) (point-x p)))
//   (f 5)
//   -> Runtime error [3:1-3:5]: (f) Accessor function expects a point, ...
//
// `[3:1-3:5]` is `(f 5)`; the accessor call is on line 2. This is #239's
// coarseness arriving through the accessor path: applyFn decided whether the
// Ap op's own range could be trusted by asking whether the enclosing frame had
// a *synthetic name*, which is true of a contract wrapper but equally true of
// any lambda the student names. A struct accessor is a bare native with no
// contract wrapper, so it fell straight into that blind spot -- as do the
// constructor, and any native a student calls from inside their own function.

describe('#592: a struct accessor is blamed at its own call', () => {
  test("an accessor inside a user function reports that call, not the function's", async () => {
    expect(
      await runProgram(
        '(struct point (x y))\n(define f (lambda (p) (point-x p)))\n(f 5)',
      ),
    ).toEqual([
      'Runtime error [2:23-2:33]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('an accessor at the top level still reports its own call', async () => {
    expect(
      await runProgram('(struct point (x y))\n(point-x 5)'),
    ).toEqual([
      'Runtime error [2:1-2:11]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('an accessor inside an anonymous lambda reports its own call', async () => {
    expect(
      await runProgram(
        '(struct point (x y))\n((lambda (p) (point-x p)) 5)',
      ),
    ).toEqual([
      'Runtime error [2:14-2:24]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('the constructor is blamed at its own call too', async () => {
    expect(
      await runProgram(
        '(struct point (x y))\n(define f (lambda (a) (point a)))\n(f 5)',
      ),
    ).toEqual([
      'Runtime error [2:23-2:31]: (point) Constructor point expects 2 arguments, received 1',
    ])
  })

  test('an accessor a library function calls is still blamed on the student', async () => {
    // `map` is library code, so the call it makes has no site in the student's
    // program and the range stays their own `(map ...)`. The name reported is
    // the library frame's, which is how a contracted native gets its Scamper
    // spelling instead of the raw `prelude_*` identifier behind its wrapper --
    // `apply` here is prelude's own helper, which is its own (smaller) wart.
    expect(
      await runProgram('(struct point (x y))\n(map point-x (list 1 2))'),
    ).toEqual([
      'Runtime error [2:1-2:24]: (apply) Accessor function expects a point, received number',
    ])
  })
})

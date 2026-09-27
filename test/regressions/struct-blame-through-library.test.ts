import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/633
//
// Handing a struct accessor or constructor to a higher-order library function
// reported the library's *internal* helper as the culprit -- a name that
// appears nowhere in the student's program and that they cannot look up:
//
//   (struct point (x y))
//   (map point-x (list 1 2))
//   -> Runtime error [2:1-2:24]: (apply) Accessor function expects a point, ...
//
// The range was already right; only the name was wrong. applyFn names the
// enclosing library frame when the thrower has not named itself (which is how a
// contracted native gets its Scamper spelling rather than the raw `prelude_*`
// identifier behind its wrapper), and `apply` is prelude's own helper. A struct
// thrower has no wrapper to speak for it, so it now sets its own source and
// applyFn's `e.source ??=` leaves it alone -- through any depth of library
// frames.

describe('#633: a struct accessor called by a library function names itself', () => {
  test('map reports the accessor, not prelude `apply`', async () => {
    expect(
      await runProgram('(struct point (x y))\n(map point-x (list 1 2))'),
    ).toEqual([
      'Runtime error [2:1-2:24]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('filter reports the accessor, not `filter-onto`', async () => {
    expect(
      await runProgram('(struct point (x y))\n(filter point-x (list 1 2))'),
    ).toEqual([
      'Runtime error [2:1-2:27]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('sort reports the accessor, not `sort-merge`', async () => {
    expect(
      await runProgram('(struct point (x y))\n(sort (list 1 2) point-x)'),
    ).toEqual([
      'Runtime error [2:1-2:25]: (point-x) Accessor function expects a point, received number',
    ])
  })

  test('the constructor names itself through a library call too', async () => {
    expect(
      await runProgram('(struct point (x y))\n(map point (list 1 2))'),
    ).toEqual([
      'Runtime error [2:1-2:22]: (point) Constructor point expects 2 arguments, received 1',
    ])
  })

  test('a direct call is unaffected -- it already named the accessor', async () => {
    expect(
      await runProgram('(struct point (x y))\n(point-x 5)'),
    ).toEqual([
      'Runtime error [2:1-2:11]: (point-x) Accessor function expects a point, received number',
    ])
  })
})

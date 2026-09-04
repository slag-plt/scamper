import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/492
//
// `string` and `append` are documented with a rest parameter alone --
// `(string & c1)`, `(append & l1)` -- so the generated contract accepts a call
// with no arguments at all, but their natives each required a first one:
// `prelude_string` leaked a raw Javascript TypeError and `prelude_append`
// returned void. R7RS makes both total, so the natives are: `(string)` is the
// empty string and `(append)` is the empty list.

test('string and append accept no arguments (#492)', async () => {
  expect(await runProgram(`
  (string)
  (append)
  (null? (append))
  (string #\\a #\\b)
  (append (list 1) (list 2))
  `)).toEqual([
    '""',
    'null',
    '#t',
    '"ab"',
    '(list 1 2)',
  ])
})

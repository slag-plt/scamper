import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/251
//
// `color-name?`, `find-colors`, and `color-name->rgb` declared their string
// parameter's contract as bare `string` instead of the predicate `string?`.
// The auto-generated contract layer invokes the annotation as a predicate, so
// it called the real `string` (char ... -> string) constructor; a genuine
// string argument is never a char, so that constructor's own contract failed
// on every call and all three functions errored unconditionally.

test('color-name?/find-colors/color-name->rgb accept string arguments', async () => {
  expect(await runProgram(`
  (import image)
  (color-name? "red")
  (color-name? "not-a-real-color")
  (rgb-red (color-name->rgb "red"))
  (rgb-green (color-name->rgb "red"))
  (rgb-blue (color-name->rgb "red"))
  (list? (find-colors "red"))
  (>= (index-of (find-colors "red") "red") 0)
  `)).toEqual([
    '#t',
    '#f',
    '255',
    '0',
    '0',
    '#t',
    '#t',
  ])
})

// With the contract now correctly `string?`, passing a non-string is a
// contract violation (an error), not a `#f` result -- confirming the
// annotation is enforced as a predicate rather than ignored.
test('color-name? rejects a non-string argument via its contract', async () => {
  const out = await runProgram(`
  (import image)
  (color-name? 5)
  `)
  expect(out).toHaveLength(1)
  expect(out[0]).toContain('expected a string, received number')
})

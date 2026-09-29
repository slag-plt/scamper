import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { lookup } from '../../src/js/index.js'

// https://github.com/slag-plt/scamper/issues/641
//
// `eq?`'s docstring says "An alias for `(eqv? v1 v2)`", and under Scamper's
// value representation the two cannot differ: every atom is a Javascript
// primitive except `char`, which the shared helper unwraps. R7RS lets an
// implementation make `eq?` finer than `eqv?`; doing so here would only
// re-create the confusion this feature removes.
//
// So they are one native under two bindings -- the shape `list-tail`/`list-drop`
// and `procedure?`/`function?` already share -- which is what makes the
// docstring true by construction rather than by anyone keeping two natives in
// step. The native carries the R7RS-primary name, `eqv?`.

describe('eq? and eqv? are one procedure (#641)', () => {
  test('there is only one native behind the two names', () => {
    expect(() => lookup('prelude_eqQ')).toThrow(/not bound/)
    expect(typeof lookup('prelude_eqvQ')).toBe('function')
  })

  // Sharing the native means sharing its `name` property: Module.registerValue
  // stamps it as it binds each name, so the second binding overwrites the
  // first's stamp and the function itself can only carry one of the two. What a
  // student ever names is the contract wrapper in front of it, which is what
  // keeps each binding reporting itself -- the same thing `list-tail` and
  // `list-drop` rely on (#649).
  test('each name reports itself, not the one it shares a native with', async () => {
    expect(await runProgram(`
eqv?
eq?
`)).toEqual(['[Function: eqv?]', '[Function: eq?]'])
  })

  // Arity is checked by that wrapper, so both names have it -- and since #669
  // the message names the procedure it is about, which is the wrapper's own
  // Scamper spelling. So this is a second, independent witness that the shared
  // native does not leak one name into the other's errors.
  test('both names take exactly two arguments', async () => {
    expect(await runProgram(`
(eqv? 1)
(eq? 1)
`, { stripRanges: true })).toEqual([
      'Runtime error: (eqv?) Arity mismatch in function call: expected 2 arguments, got 1',
      'Runtime error: (eq?) Arity mismatch in function call: expected 2 arguments, got 1',
    ])
  })
})

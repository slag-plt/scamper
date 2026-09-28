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

  test('a student reaching either through its contract sees its own name', async () => {
    expect(await runProgram(`
(eqv? 1)
(eq? 1)
`, { stripRanges: true })).toEqual([
      'Runtime error: Arity mismatch in function call: expected 2 arguments, got 1',
      'Runtime error: Arity mismatch in function call: expected 2 arguments, got 1',
    ])
  })
})

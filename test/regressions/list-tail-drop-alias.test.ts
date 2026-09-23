import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { lookup } from '../../src/js/index.js'

// https://github.com/slag-plt/scamper/issues/649
//
// `list-drop`'s docstring says "An alias for `(list-tail l k)`", but the two
// were separate natives, and #553 guarded only `list-drop` -- it is the one
// `sort` reaches past its contract. So the same call answered differently
// depending on which name reached the native: `((js-var "prelude_listDrop") 5
// 1)` raised, while `((js-var "prelude_listTail") 5 1)` walked off a number
// and returned void.
//
// They are now one native under two bindings -- the shape `procedure?` and
// `function?` already share -- which is what makes the docstring true by
// construction rather than by anyone remembering to keep the two in step. The
// native carries the primary name, `list-tail`, and `list-drop`'s own
// docstring is what explains its binding.

describe('list-tail and list-drop are one procedure (#649)', () => {
  test('there is only one native left to guard', () => {
    expect(() => lookup('prelude_listDrop')).toThrow(/not bound/)
    expect(typeof lookup('prelude_listTail')).toBe('function')
  })

  test('the raw native is guarded, under either binding', async () => {
    expect(await runProgram(`
((js-var "prelude_listTail") 5 1)
((js-var "prelude_listTail") null "a")
`, { stripRanges: true })).toEqual([
      'Runtime error: (prelude_listTail) list-tail: expected a list',
      'Runtime error: (prelude_listTail) list-tail: expected an integer',
    ])
  })

  test('a student reaching either through its contract sees its own name', async () => {
    expect(await runProgram(`
(list-tail 5 1)
(list-drop 5 1)
`, { stripRanges: true })).toEqual([
      'Runtime error: (error) expected a list as the first argument, received number',
      'Runtime error: (error) expected a list as the first argument, received number',
    ])
  })

  test('both still do what they always did', async () => {
    expect(await runProgram(`
(list-tail (list 1 2 3 4) 2)
(list-drop (list 1 2 3 4) 2)
(list-tail (list 1 2 3) 0)
(list-tail (list 1 2 3) 3)
(list-tail null 0)
(sort (list 3 1 2) <)
`)).toEqual([
      '(list 3 4)',
      '(list 3 4)',
      '(list 1 2 3)',
      'null',
      'null',
      '(list 1 2 3)',
    ])
  })
})

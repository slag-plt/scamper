import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/648
//
// #614 made the five numeric comparisons variadic with an arity floor of 0, to
// match the character and string comparisons, which already had one. That made
// `(< 1)` and `(<)` answer `#t`: a student who wrote `(< x)` meaning
// `(< x y)` no longer got an arity error, just a silently wrong branch.
//
// R7RS-small gives every one of these procedures *two* arguments before the
// ellipsis -- `(= z1 z2 z3 ...)`, `(char<? char1 char2 char3 ...)`,
// `(string=? string1 string2 string3 ...)` -- and says nothing about shorter
// calls, so the signature is the specification. The floor is two for all 25:
// the five numeric, the ten character, and the ten string.
//
// The floor is enforced by the docstring signature in src/lib/prelude.scm,
// which is where a contract comes from; the natives are untouched.

const ARITY_1 =
  'Runtime error: Arity mismatch in function call: expected 2 arguments, got 1'
const ARITY_0 =
  'Runtime error: Arity mismatch in function call: expected 2 arguments, got 0'

/** Runs `src` with source ranges dropped, so only messages are asserted. */
const run = (src: string): Promise<string[]> =>
  runProgram(src, { stripRanges: true })

const ops = ['=', '<', '>', '<=', '>=']

/**
 * Two operands, ordered so that `op` holds, drawn from `lo` < `hi`. `eq` is
 * the second operand for the three comparisons that hold on equal arguments;
 * the case-insensitive families pass a case-flipped `lo` there, so ignoring
 * case is what makes the call true.
 */
function satisfying(op: string, lo: string, hi: string, eq: string = lo): string {
  switch (op) {
    case '<': return `${lo} ${hi}`
    case '>': return `${hi} ${lo}`
    default: return `${lo} ${eq}`
  }
}

/** The 25 comparisons, each with a one-argument call and a two-argument one. */
const comparisons = [
  ...ops.map((op) =>
    [op, `(${op} 1)`, `(${op} ${satisfying(op, '1', '2')})`] as const),
  ...ops.map((op) =>
    [`char${op}?`, `(char${op}? #\\a)`,
      `(char${op}? ${satisfying(op, '#\\a', '#\\b')})`] as const),
  ...ops.map((op) =>
    [`char-ci${op}?`, `(char-ci${op}? #\\a)`,
      `(char-ci${op}? ${satisfying(op, '#\\a', '#\\B', '#\\A')})`] as const),
  ...ops.map((op) =>
    [`string${op}?`, `(string${op}? "a")`,
      `(string${op}? ${satisfying(op, '"a"', '"b"')})`] as const),
  ...ops.map((op) =>
    [`string-ci${op}?`, `(string-ci${op}? "a")`,
      `(string-ci${op}? ${satisfying(op, '"a"', '"B"', '"A"')})`] as const),
]

describe('a comparison applied to fewer than two arguments is an arity error (#648)', () => {
  test('the calls named in the issue', async () => {
    expect(
      await run('(< 1)\n(<)\n(= 1)\n(char=? #\\a)\n(string=? "a")'),
    ).toEqual([ARITY_1, ARITY_0, ARITY_1, ARITY_1, ARITY_1])
  })

  test('all 25 comparisons agree, so no family is an exception', () => {
    expect(comparisons.length).toBe(25)
  })

  test.each(comparisons)('(%s ...) with one argument', async (_name, one) => {
    expect(await run(one)).toEqual([ARITY_1])
  })

  test('with no arguments at all', async () => {
    expect(await run('(<)\n(char<?)\n(string-ci=?)')).toEqual([
      ARITY_0,
      ARITY_0,
      ARITY_0,
    ])
  })

  test('apply over a too-short list is caught too', async () => {
    // `apply` reaches the contract-wrapped binding like any other call, so a
    // list assembled at run time cannot slip under the floor.
    expect(await run('(apply < (list 1))')).toEqual([ARITY_1])
  })
})

describe('two arguments and more still work (#648)', () => {
  test.each(comparisons)('(%s ...) with two arguments', async (_name, _one, two) => {
    expect(await run(two)).toEqual(['#t'])
  })

  test('three and more, in every family', async () => {
    expect(
      await run(`
      (< 1 2 3) (<= 1 1 2) (> 3 2 1) (>= 3 3 1) (= 1 1 1)
      (char<? #\\a #\\b #\\c) (char-ci<? #\\a #\\B #\\c)
      (string<? "a" "b" "c") (string-ci<? "a" "B" "c")
      `),
    ).toEqual(['#t', '#t', '#t', '#t', '#t', '#t', '#t', '#t', '#t'])
  })

  test('a bad second argument is now reported by position', async () => {
    // A consequence of the signature: the second argument is a fixed parameter
    // now, so a non-number there is named by position rather than as one of
    // the rest list. The third argument on is still the rest list.
    expect(await run('(< 1 "a")')).toEqual([
      'Runtime error: (error) expected a number as the second argument, received string',
    ])
    expect(await run('(< 1 2 "a")')).toEqual([
      'Runtime error: (error) expected every value of v3 to be a number, but at least one was not',
    ])
  })

  test('a non-monotonic run is still #f, not an error', async () => {
    expect(
      await run(`
      (< 1 2 0 3) (= 1 1 2)
      (char<? #\\a #\\c #\\b) (string-ci<? "a" "C" "b")
      `),
    ).toEqual(['#f', '#f', '#f', '#f'])
  })
})

describe("#614's numeric behaviour is unchanged from two arguments up (#648)", () => {
  /** Scamper has no `+nan.0` literal; `(sqrt -1)` is how a NaN is obtained. */
  const NAN = '(sqrt -1)'

  test('transitivity is not assumed: every adjacent pair is tested', async () => {
    expect(await run('(< 1 2 0 3)\n(= 1 2 1)')).toEqual(['#f', '#f'])
  })

  test('a NaN argument makes every comparison #f', async () => {
    expect(
      await run(`
      (< ${NAN} 1) (< 1 ${NAN} 3) (<= 1 ${NAN} 3)
      (> 3 ${NAN} 1) (>= 3 ${NAN} 1) (= ${NAN} ${NAN})
      `),
    ).toEqual(['#f', '#f', '#f', '#f', '#f', '#f'])
  })

  test('inexact zero and inexact negative zero are not distinguished', async () => {
    expect(
      await run('(= 0.0 -0.0)\n(<= 0.0 -0.0)\n(< -0.0 0.0)'),
    ).toEqual(['#t', '#t', '#f'])
  })
})

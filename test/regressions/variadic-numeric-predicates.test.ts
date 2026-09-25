import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/614
//
// R7RS-small p.36 specifies `=`, `<`, `>`, `<=`, and `>=` as variadic: they
// hold when their arguments are, respectively, equal, monotonically
// increasing, monotonically decreasing, monotonically non-decreasing, or
// monotonically non-increasing. Scamper implemented all five as binary, so
// `(< 1 2 3)` was an arity error rather than `#t`.
//
// The character and string comparisons named in the same issue were already
// variadic; the fix makes the numeric five share their helper
// (`pairwiseSatisfies`), so all three families now answer the same way.
//
// #614 also set the arity floor at 0, which #648 raised to 2 for all three
// families; see variadic-comparisons-need-two-arguments.test.ts. Everything
// below is about two arguments or more, which is unaffected.
//
// Scamper's numbers are IEEE doubles, so the report's two numeric caveats are
// live here: any NaN argument makes every predicate `#f`, and inexact zero and
// inexact negative zero are not distinguished.

/** Scamper has no `+nan.0` literal; `(sqrt -1)` is how a NaN is obtained. */
const NAN = '(sqrt -1)'

describe('the numeric comparisons are variadic (#614)', () => {
  test('three or more arguments are accepted', async () => {
    expect(
      await runProgram(`
      (< 1 2 3)
      (<= 1 1 2)
      (> 3 2 1)
      (>= 3 3 1)
      (= 1 1 1)
      (< 1 2 3 4 5 6)
      `),
    ).toEqual(['#t', '#t', '#t', '#t', '#t', '#t'])
  })

  test('a non-monotonic run is #f', async () => {
    expect(
      await runProgram(`
      (< 1 2 0 3)
      (> 3 1 2)
      (<= 1 2 0 3)
      (>= 3 1 2)
      (= 1 1 2)
      `),
    ).toEqual(['#f', '#f', '#f', '#f', '#f'])
  })

  test('transitivity is not assumed: only the ends agreeing is not enough', async () => {
    // A check that compared only the first and last argument -- or that folded
    // `(< 1 2)` and `(< 0 3)` -- would call this #t. Every adjacent pair has to
    // be tested.
    expect(await runProgram('(< 1 2 0 3)')).toEqual(['#f'])
    expect(await runProgram('(= 1 2 1)')).toEqual(['#f'])
  })

  test('apply over a list works, which is where the arity limit bit', async () => {
    expect(
      await runProgram(`
      (apply < (list 1 2 3 4))
      (apply < (list 1 2 4 3))
      `),
    ).toEqual(['#t', '#f'])
  })
})

describe('a NaN argument makes every comparison #f (#614)', () => {
  test('NaN anywhere in the arguments is #f', async () => {
    expect(
      await runProgram(`
      (nan? ${NAN})
      (< ${NAN} 1)
      (< 1 ${NAN})
      (< 1 ${NAN} 3)
      (<= 1 ${NAN} 3)
      (> 3 ${NAN} 1)
      (>= 3 ${NAN} 1)
      (= 1 ${NAN} 1)
      (= ${NAN} ${NAN})
      `),
    ).toEqual(['#t', '#f', '#f', '#f', '#f', '#f', '#f', '#f', '#f'])
  })

  test('NaN is not skipped over by the pairs around it', async () => {
    // `(< 1 nan 3)` has an increasing run if the NaN is dropped, so a check
    // that compared only the non-NaN arguments would wrongly say #t.
    expect(await runProgram(`(< 1 ${NAN} 3)`)).toEqual(['#f'])
  })
})

describe('inexact zero and inexact negative zero are not distinguished (#614)', () => {
  test('-0.0 really is a negative zero, so the next test is not vacuous', async () => {
    expect(await runProgram('(expt -0.0 -1) (expt 0.0 -1)')).toEqual([
      '-Infinity',
      'Infinity',
    ])
  })

  test('0.0 and -0.0 compare equal', async () => {
    expect(
      await runProgram(`
      (= 0.0 -0.0)
      (= -0.0 0.0 0)
      (<= 0.0 -0.0)
      (>= 0.0 -0.0)
      (< -0.0 0.0)
      (> -0.0 0.0)
      `),
    ).toEqual(['#t', '#t', '#t', '#t', '#f', '#f'])
  })
})

describe('the char and string comparisons answer the same way (#614)', () => {
  // This block asserted that fewer than two arguments is vacuously true --
  // `(< 1)`, `(<)`, `(char<? #\a)` and `(string<? "a")` were all `#t`, an
  // arity floor of 0 shared by all three families. #648 raised the floor to 2,
  // so those cases are now arity errors and live in the test named above; what
  // remains here is the agreement across the families that #614 is about.
  test('two arguments and more agree across the three families', async () => {
    expect(
      await runProgram(`
      (char<? #\\a #\\b)
      (string<? "a" "b")
      (char<? #\\a #\\b #\\c)
      (string<? "a" "b" "c")
      (char<? #\\a #\\c #\\b)
      (string<? "a" "c" "b")
      `),
    ).toEqual(['#t', '#t', '#t', '#t', '#f', '#f'])
  })
})

describe('the element contract still applies to every argument (#614)', () => {
  test('a non-number anywhere is rejected', async () => {
    const stripRange = (msgs: string[]): string[] =>
      msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))
    expect(stripRange(await runProgram('(< 1 2 "a")'))).toEqual([
      'Runtime error [..]: (error) expected every value of v3 to be a number, but at least one was not',
    ])
  })
})

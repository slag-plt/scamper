import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/756
//
// `and`, `or` and `cond` all lower to `if` (expansion.ts), so the one raise in
// `IfHandler` served four surface forms and named only `if` -- the form the
// student is least likely to have typed:
//
//   > (and 1 2)
//   Runtime error: if: expected a boolean guard, received number
//
// The information was already there: `A.mkIf` is tagged with its provenance at
// expansion and `interface If` keeps it, so the raise can name the form the
// student wrote.
//
// Ranges are stripped throughout: this test is about *blame*, and the range is
// #753's question (answered) and #762's (the clause of a `cond`, open). Pinning
// both here would make a range fix look like a blame regression.

/**
 * The expected error for a non-boolean guard, blamed on `form`.
 *
 * DESIGN NOT YET SETTLED (#756). This spells the recommended shape: the
 * message's own head names the form the student wrote, replacing `if:`. The
 * alternative is to leave the message alone and pass the provenance as
 * `ScamperError`'s `source`, which renders as a `(name) ` prefix --
 * `(and) if: expected a boolean guard, received number`. Switching this test
 * to that answer is this one template string.
 */
const guardError = (form: string) =>
  `Runtime error: ${form}: expected a boolean guard, received number`

describe('a non-boolean guard blames the form it was written in (#756)', () => {
  test('an and-form is blamed on and', async () => {
    expect(await runProgram('(and 1 2)', { stripRanges: true })).toEqual([
      guardError('and'),
    ])
  })

  test('an or-form is blamed on or', async () => {
    expect(await runProgram('(or #f 5)', { stripRanges: true })).toEqual([
      guardError('or'),
    ])
  })

  test('a cond clause is blamed on cond', async () => {
    expect(await runProgram('(cond [5 1])', { stripRanges: true })).toEqual([
      guardError('cond'),
    ])
  })

  // The last clause of a `cond` that ends in `[else ...]` expands to an `if`
  // tagged `cond-else` rather than `cond`, a distinction that exists so
  // sugaring can tell an else body from the rest of the chain. It is not a
  // form a student can write, so as a blame label it reads `cond`.
  test('the clause before an else clause is blamed on cond, not cond-else', async () => {
    expect(
      await runProgram('(cond [5 1] [else 2])', { stripRanges: true }),
    ).toEqual([guardError('cond')])
  })

  // The innermost form wins: `or`'s own `if` raises before `and`'s is reached.
  test('a nested derived form is blamed on the inner form', async () => {
    expect(await runProgram('(and (or 1 2) 3)', { stripRanges: true })).toEqual([
      guardError('or'),
    ])
  })

  // Already true today, and the point of keeping provenance optional: an `if`
  // straight from the parser carries none, so the fallback is the message
  // Scamper has always printed. A fix that works by renaming the message head
  // for every `if` would break this one.
  test('a plain if is still blamed on if', async () => {
    expect(await runProgram('(if 1 2 3)', { stripRanges: true })).toEqual([
      guardError('if'),
    ])
  })
})

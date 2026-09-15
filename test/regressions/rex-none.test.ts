import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/600
//
// #533 established `(?!)` as the pattern that matches nothing -- alternation's
// unit -- but left it anonymous. `(rex-empty)` names concatenation's unit; its
// dual had no name to be written, pointed at from a docstring, or returned.
// `(rex-none)` is that name, and the two empty cases now *are* it rather than
// each rendering `(?!)` for themselves.
//
// The other half: `(rex-char-set "")` used to compile to `[]`, which matches
// nothing in Javascript and nowhere else. The behaviour was already right and
// right by accident, so nothing recorded it and nothing kept it that way.
//
// Escaping is the same complaint one step over, found while fixing the above:
// `^` and `-` mean something only inside a character class, which is exactly
// where `rex-char-set` puts them, and neither was escaped.
//
// Behaviour is asserted rather than the rendered pattern wherever there is a
// behaviour to assert, so the spelling of "matches nothing" stays an
// implementation detail -- the exceptions being the `[]` and the `[^]` this
// fixes, which are behaviourally identical to what replaces them.

describe('rex says what it means at the edges (#600)', () => {
  test('nothing matches it, not even the empty string', async () => {
    expect(
      await runProgram(`
(import rex)
(rex-matches? (rex-none) "")
(rex-matches? (rex-none) "a")
(rex-matches? (rex-none) "anything")
(rex-find-matches (rex-none) "abc")
(rex-split-string (rex-none) "abc")
(rex? (rex-none))
`),
    ).toEqual(['#f', '#f', '#f', 'null', '(list "abc")', '#t'])
  })

  // The unit has to be absorbed by the operation it is the unit of, and has to
  // survive the other combinators: one repetition of nothing is impossible,
  // zero repetitions is the empty string, and an optional nothing is the empty
  // string. Same properties #533 pinned for `(rex-any-of)`, now under a name.
  test('it composes as alternation\'s unit', async () => {
    expect(
      await runProgram(`
(import rex)
(rex-matches? (rex-any-of (rex-none) (rex-string "a")) "a")
(rex-matches? (rex-any-of (rex-none) (rex-string "a")) "")
(rex-matches? (rex-concat (rex-string "a") (rex-none)) "a")
(rex-matches? (rex-repeat (rex-none)) "")
(rex-matches? (rex-repeat-0 (rex-none)) "")
(rex-matches? (rex-optional (rex-none)) "")
(rex-matches? (rex-optional (rex-none)) "a")
`),
    ).toEqual(['#t', '#f', '#f', '#f', '#t', '#t', '#f'])
  })

  // The two cases that had to reach it some other way before it had a name.
  test('an empty alternation and an empty character set are it', async () => {
    expect(
      await runProgram(`
(import rex)
(equal? (rex-any-of) (rex-none))
(equal? (rex-char-set "") (rex-none))
(rex-matches? (rex-char-set "") "")
(rex-matches? (rex-char-set "") "a")
(rex-find-matches (rex-char-set "") "abc")
(rex-matches? (rex-concat (rex-string "a") (rex-char-set "")) "a")
`),
    ).toEqual(['#t', '#t', '#f', '#f', 'null', '#f'])
  })

  // `[]` was the whole complaint, and it is a complaint about the *spelling*:
  // `[]` and `(?!)` behave identically in Javascript, so only the rendered
  // pattern can tell them apart. The one place this file reads one on purpose.
  test('an empty character set no longer compiles to []', async () => {
    expect(
      await runProgram(`
(import rex)
(rex->string (rex-none))
(rex->string (rex-any-of))
(rex->string (rex-char-set ""))
`),
    ).toEqual(['"(?!)"', '"(?!)"', '"(?!)"'])
  })

  // The last producer of `[^]` in the library, and the same complaint one step
  // over: `^` and `-` mean something only inside a character class, which is
  // exactly where rex-char-set puts them. Unescaped, `(rex-char-set "^a")` was
  // the *inverse* of what it promises and `(rex-char-set "a-z")` was a range.
  test('a character set takes ^ and - literally', async () => {
    expect(
      await runProgram(`
(import rex)
(rex-matches? (rex-char-set "^a") "^")
(rex-matches? (rex-char-set "^a") "a")
(rex-matches? (rex-char-set "^a") "z")
(rex-matches? (rex-char-set "a-z") "-")
(rex-matches? (rex-char-set "a-z") "m")
(rex-matches? (rex-char-antiset "^a") "z")
(rex-matches? (rex-char-antiset "^a") "^")
(rex-matches? (rex-string "a-b") "a-b")
`),
    ).toEqual(['#t', '#t', '#f', '#t', '#f', '#t', '#f', '#t'])
  })

  // A range starting at `^` used to render `[^-z]`, which Javascript reads as
  // a negated class containing `-` and `z` rather than as a range at all.
  test('a character range starting at ^ is still a range', async () => {
    expect(
      await runProgram(`
(import rex)
(rex-matches? (rex-char-range #\\^ #\\z) "a")
(rex-matches? (rex-char-range #\\^ #\\z) "-")
(rex-matches? (rex-char-range #\\a #\\z) "m")
`),
    ).toEqual(['#t', '#f', '#t'])
  })

  // The dual, which the issue does not name but which relied on the same
  // quirk: excluding nothing admits every character, newline included, where
  // `rex-any-char` -- a bare `.` -- stops at the newline.
  test('an empty character antiset still admits every character', async () => {
    expect(
      await runProgram(`
(import rex)
(rex-matches? (rex-char-antiset "") "a")
(rex-matches? (rex-char-antiset "") "\\n")
(rex-matches? (rex-any-char) "\\n")
(rex-matches? (rex-char-antiset "") "ab")
(rex->string (rex-char-antiset ""))
`),
    ).toEqual(['#t', '#t', '#f', '#f', '"[\\\\s\\\\S]"'])
  })
})

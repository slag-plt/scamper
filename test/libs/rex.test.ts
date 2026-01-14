import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import rex from '../../src/lib/rex.js'

// Adapted froM: https://eikmeier.sites.grinnell.edu/csc-151-fall-2025/readings/regexp.html

test('string', () => {
  expect(runProgram(`
    (import rex)
    (define hello (rex-string "hello"))
    (rex-matches? hello "hello")
    (rex-matches? hello "hello!")
    (rex-matches? hello "hi")
  `)).toEqual([
    '#t',
    '#f',
    '#f'
  ])
})

test('repeat', () => {
  expect(runProgram(`
    (import rex)
    (define hellos (rex-repeat (rex-string "hello")))
    (rex-matches? hellos "hello")
    (rex-matches? hellos "hellohello")
    (rex-matches? hellos "hellohellohello")
    (rex-matches? hellos "hellohello hello")
    (rex-matches? hellos "")
  `)).toEqual([
    '#t',
    '#t',
    '#t',
    '#f',
    '#f'
  ])
})

test('repeat0', () => {
  expect(runProgram(`
    (import rex)
    (define hellos0 (rex-repeat-0 (rex-string "hello")))
    (rex-matches? hellos0 "hello")
    (rex-matches? hellos0 "")
    (rex-matches? hellos0 "hellohellohello")
    (rex-matches? hellos0 "hellohellohelloo")
  `)).toEqual([
    '#t',
    '#t',
    '#t',
    '#f'
  ])
})

test('concat', () => {
  expect(runProgram(`
    (import rex)
    (define hello-echo (rex-concat (rex-string "hello") (rex-repeat-0 (rex-string "o"))))
    (rex-matches? hello-echo "hello")
    (rex-matches? hello-echo "helloooooooooooo")
    (rex-matches? hello-echo "hellooo")
    (rex-matches? hello-echo "goodbye")
`)).toEqual([
    '#t',
    '#t',
    '#t',
    '#f'
  ])
})

test('repeat combination', () => {
  expect(runProgram(`
    (import rex)
    (define hello-echo (rex-concat (rex-string "hello") (rex-repeat-0 (rex-string "o"))))
    (define remaining-hellos (rex-repeat-0 (rex-concat (rex-string " ") hello-echo)))
    (define echoing-hellos (rex-concat hello-echo remaining-hellos))
    (rex-matches? echoing-hellos "hello")
    (rex-matches? echoing-hellos "helloooo")
    (rex-matches? echoing-hellos "hello hello hello")
    (rex-matches? echoing-hellos "hellooo hello hello")
    (rex-matches? echoing-hellos "hellohello") ; no space
    (rex-matches? echoing-hellos "hello he")   ; incomplete second hello
  `)).toEqual([
    '#t',
    '#t',
    '#t',
    '#t',
    '#f',
    '#f'
  ])
})

test('any-char', () => {
  expect(runProgram(`
    (import rex)
    (rex-matches? (rex-any-char) "a")
    (rex-matches? (rex-any-char) "+")
    (rex-matches? (rex-any-char) "a+")
  `)).toEqual([
    '#t',
    '#t',
    '#f'
  ])
})

test('any-char and repeat', () => {
  expect(runProgram(`
    (import rex)
    (define sr (rex-concat (rex-string "s")
                          (rex-repeat-0 (rex-any-char))
                          (rex-string "r")))
    (rex-matches? sr "super")
    (rex-matches? sr "stranger")
    (rex-matches? sr "sr")
    (rex-matches? sr "samr")
    (rex-matches? sr "computer")
    (rex-matches? sr "science")
    (rex-matches? sr "science computer")
  `)).toEqual([
    '#t',
    '#t',
    '#t',
    '#t',
    '#f',
    '#f',
    '#t'
  ])
})

test('char types', () => {
  expect(runProgram(`
    (import rex)
    (define lowercase (rex-char-range #\\a #\\z))
    (define vowel (rex-char-set "aeiou"))
    (define non-vowel (rex-char-antiset "aeiou"))
    (rex-matches? lowercase "c")
    (rex-matches? lowercase "C")
    (rex-matches? lowercase "cc")
    (rex-matches? vowel "c")
    (rex-matches? vowel "e")
    (rex-matches? non-vowel "f")
    (rex-matches? non-vowel "e")
  `)).toEqual([
    '#t',
    '#f',
    '#f',
    '#f',
    '#t',
    '#t',
    '#f'
  ])
})

test('double vowel', () => {
  expect(runProgram(`
    (import rex)
    (define lowercase (rex-char-range #\\a #\\z))
    (define vowel (rex-char-set "aeiou"))
    (define double-vowel (rex-concat (rex-repeat-0 lowercase) vowel vowel (rex-repeat-0 lowercase)))
    (rex-matches? double-vowel "hello")
    (rex-matches? double-vowel "helloo")
    (rex-matches? double-vowel "field")
    (rex-matches? double-vowel "aardvark")
  `)).toEqual([
    '#f',
    '#t',
    '#t',
    '#t'    
  ])
})

test('empty', () => {
  expect(runProgram(`
    (import rex)
    (rex-matches? (rex-empty) "")
    (rex-matches? (rex-empty) "hello")
  `)).toEqual([
    '#t',
    '#f'
  ])
})

test('find-matches', () => {
  expect(runProgram(`
    (import rex)
    (define lowercase (rex-char-range #\\a #\\z))
    (define vowel (rex-char-set "aeiou"))
    (define double-vowel-word
      (rex-concat (rex-repeat-0 lowercase) vowel vowel (rex-repeat-0 lowercase)))
    (rex-find-matches double-vowel-word "now is the time for all good people to come to the aid of their country")
  `)).toEqual([
   '(list "good" "people" "aid" "their" "country")'
  ])
})

test('splitter', () => {
  expect(runProgram(`
    (import rex)
    (define splitter
      (rex-concat (rex-char-set ",;")
                  (rex-string " ")
                  (rex-optional (rex-string "and "))))
    (rex-split-string splitter "me, you, and a dog named boo")
    (rex-split-string splitter "alpha, beta; gamma")
  `)).toEqual([
    '(list "me" "you" "a dog named boo")',
    '(list "alpha" "beta" "gamma")'
  ])
})

test('display', () => {
  expect(runProgram(`
    (import rex)
    (define lowercase (rex-char-range #\\a #\\z))
    (define vowel (rex-char-set "aeiou"))
    (define double-vowel-word
      (rex-concat (rex-repeat-0 lowercase) vowel vowel (rex-repeat-0 lowercase)))
    (define splitter
      (rex-concat (rex-char-set ",;")
                  (rex-string " ")
                  (rex-optional (rex-string "and "))))
    vowel
    double-vowel-word
    splitter
  `)).toEqual([
    '(rex-char-set "aeiou")',
    '(rex-concat (vector (rex-repeat-0 (rex-char-range #\\a #\\z)) (rex-char-set "aeiou") (rex-char-set "aeiou") (rex-repeat-0 (rex-char-range #\\a #\\z))))',
    '(rex-concat (vector (rex-char-set ",;") (rex-string " ") (rex-optional (rex-string "and "))))'
  ])
})

test('rex->string', () => {
  expect(runProgram(`
    (import rex)
    (define lowercase (rex-char-range #\\a #\\z))
    (define vowel (rex-char-set "aeiou"))
    (define double-vowel-word
      (rex-concat (rex-repeat-0 lowercase) vowel vowel (rex-repeat-0 lowercase)))
    (define splitter
      (rex-concat (rex-char-set ",;")
                  (rex-string " ")
                  (rex-optional (rex-string "and "))))
    (rex->string vowel)
    (rex->string double-vowel-word)
    (rex->string splitter)
  `)).toEqual([
    '"[aeiou]"',
    '"(?:[a-z])*[aeiou][aeiou](?:[a-z])*"',
    '"[,;] (?:and )?"'
  ])
})
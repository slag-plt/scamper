import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/138

test('fold-arguments', () => {
  expect(runProgram(`
  (fold-left string-append "" (list "!" "%" "#" "@"))
  (fold-right string-append "" (list "!" "%" "#" "@"))

  (fold-left - 0 (list 1 2 3 4 5))
  (fold-right - 0 (list 1 2 3 4 5))
  `)).toEqual([
    '"!%#@"',
    '"!%#@"',
    '-15',
    '3'
  ])
})

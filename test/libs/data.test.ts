import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

test('tally-all', () => {
  expect(runProgram(`
  (import data)
  (tally-all (list "a" "b" "a" "c" "c" "d" "b" "a" "q" "r" "r" "a" "d"))
  `)).toEqual([
    '(list (pair "a" 4) (pair "b" 2) (pair "c" 2) (pair "d" 2) (pair "q" 1) (pair "r" 2))'
  ])
})


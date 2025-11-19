import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/151

test('literal-escape', () => {
  expect(runProgram(`
    "\["
    "\]"
    "\\\\"
  `)).toEqual([
    '"["',
    '"]"',
    '"\\"'
  ])
})

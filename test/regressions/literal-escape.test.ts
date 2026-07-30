import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/151

test('literal-escape', async () => {
  expect(await runProgram(`
    "["
    "]"
    "\\\\"
  `)).toEqual([
    '"["',
    '"]"',
    '"\\\\"' // string containing a single backslash -> escaped as "\\" (see #252)
  ])
})

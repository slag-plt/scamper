import { expect, test } from 'vitest'
import { runProgram } from './harness.js'

test('test-result-ok prints as a passing result', async () => {
  expect(await runProgram(`
  (import test)
  (test-result-ok "desc")
  `)).toEqual(['Test "desc"\n✅ Passed!'])
})

test('test-result-error-expected carries the expected and actual values', async () => {
  expect(await runProgram(`
  (import test)
  (test-result-error-expected "desc" 5 4)
  `)).toEqual(['Test "desc"\n❌ Failed! Expected 5, received 4'])
})

test('test-result-error-exn carries the thrown exception', async () => {
  expect(await runProgram(`
  (import test)
  (test-result-error-exn "desc" "boom")
  `)).toEqual(['Test "desc"\n❌ Failed! Exception thrown: "boom"'])
})

test('test-result? is true for a real test result', async () => {
  expect(await runProgram(`
  (import test)
  (test-result? (test-result-ok "desc"))
  `)).toEqual(['#t'])
})

test('test-result? is false for an arbitrary non-test-result value', async () => {
  expect(await runProgram(`
  (import test)
  (test-result? 5)
  (test-result? "hi")
  `)).toEqual(['#f', '#f'])
})

// test-case (test.scm) now catches the tested function's own exception via the
// `with-handler` special form and surfaces its message string as the exn result.
test('test-case surfaces the tested function\'s exception as an exception result', async () => {
  const [line] = await runProgram(`
  (import test)
  (test-case "boom" equal? 4 (lambda () (error "boom")))
  `)
  expect(line).toBe('Test "boom"\n❌ Failed! Exception thrown: "boom"')
})

test('test-case reports Ok when the comparator confirms the actual value', async () => {
  expect(await runProgram(`
  (import test)
  (test-case "add" equal? 4 (lambda () (+ 2 2)))
  (test-case "sum" = 10 (lambda () (+ 4 6)))
  `)).toEqual([
    'Test "add"\n✅ Passed!',
    'Test "sum"\n✅ Passed!',
  ])
})

test('test-case reports the expected/actual mismatch when the wrong value is produced', async () => {
  expect(await runProgram(`
  (import test)
  (test-case "add" equal? 5 (lambda () (+ 2 2)))
  `)).toEqual(['Test "add"\n❌ Failed! Expected 5, received 4'])
})

test('test-exn reports Ok when the function throws, failure when it does not', async () => {
  expect(await runProgram(`
  (import test)
  (test-exn "throws" (lambda () (error "boom")))
  (test-exn "quiet" (lambda () 42))
  `)).toEqual([
    'Test "throws"\n✅ Passed!',
    'Test "quiet"\n❌ Failed! Test case did not throw an exception',
  ])
})

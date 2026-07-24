import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

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

// test-case's exception branch fires on EVERY call right now: test-fn is
// invoked via callScamperFn, which unconditionally throws (#248), so the
// lambda's own `(error "boom")` never runs. This pins the exn-result shape
// AND that the surfaced exception is currently the #248 error, not the
// tested function's throw. Once #248 lands, this should assert the real
// "boom" exception instead (see the skipped Ok/mismatch tests below).
test('test-case surfaces the #248 callScamperFn error as an exception result', async () => {
  const [line] = await runProgram(`
  (import test)
  (test-case "boom" equal? 4 (lambda () (error "boom")))
  `)
  expect(line).toBe(
    'Test "boom"\n❌ Failed! Exception thrown: Runtime error: Javascript library functions can no longer call Scamper functions',
  )
})

// Regression: test_testCase (src/js/test/index.ts) calls the user-supplied
// test-fn via L.callScamperFn, which src/lpm/lang.ts now unconditionally
// throws from (same limitation noted in fold-arguments.test.ts and
// lang.test.ts). Every test-case call, regardless of the comparator or
// values involved, currently lands in the exception branch above instead of
// reaching these branches.
test.skip('test-case reports Ok when the comparator confirms the actual value', async () => {
  expect(await runProgram(`
  (import test)
  (test-case "add" equal? 4 (lambda () (+ 2 2)))
  (test-case "sum" = 10 (lambda () (+ 4 6)))
  `)).toEqual([
    'Test "add"\n✅ Passed!',
    'Test "sum"\n✅ Passed!',
  ])
})

test.skip('test-case reports the expected/actual mismatch when the wrong value is produced', async () => {
  expect(await runProgram(`
  (import test)
  (test-case "add" equal? 5 (lambda () (+ 2 2)))
  `)).toEqual(['Test "add"\n❌ Failed! Expected 5, received 4'])
})

import { expect, test } from 'vitest'
import { runProgram } from './harness.js'

/** One case of Gradescope's results JSON, as the renderer emits it. */
interface Case {
  name: string
  status: string
  score: number
  max_score: number
  output: string
}

/** Runs `src` and parses its single output line as JSON. */
async function runJson<T>(src: string): Promise<T> {
  const lines = await runProgram(src)
  expect(lines).toHaveLength(1)
  return JSON.parse(lines[0]) as T
}

test('a suite renders as the JSON blob Gradescope reads', async () => {
  expect(
    await runJson(`
    (import test)
    (import gradescope)
    (gradescope-test-suite
      (list (test-case "add" equal? 4 (lambda () (+ 2 2)))
            (test-case "bad" equal? 5 (lambda () (+ 2 2)))))
    `),
  ).toEqual({
    tests: [
      {
        name: 'add',
        status: 'passed',
        score: 1,
        max_score: 1,
        output: 'Test "add"\n✅ Passed!',
      },
      {
        name: 'bad',
        status: 'failed',
        score: 0,
        max_score: 1,
        output: 'Test "bad"\n❌ Failed! Expected 5, received 4',
      },
    ],
  })
})

test('every kind of failure scores zero and carries its own message', async () => {
  const { tests } = await runJson<{ tests: Case[] }>(`
  (import test)
  (import gradescope)
  (gradescope-test-suite
    (list (test-result-error-expected "exp" 5 4)
          (test-result-error-exn "exn" "boom")
          (test-result-error-gen "gen" "just because")))
  `)
  expect(tests.map((t) => [t.name, t.status, t.score])).toEqual([
    ['exp', 'failed', 0],
    ['exn', 'failed', 0],
    ['gen', 'failed', 0],
  ])
  expect(tests.map((t) => t.output)).toEqual([
    'Test "exp"\n❌ Failed! Expected 5, received 4',
    'Test "exn"\n❌ Failed! Exception thrown: "boom"',
    'Test "gen"\n❌ Failed! just because',
  ])
})

test('an empty suite is an empty tests array, not an error', async () => {
  expect(
    await runJson(`
    (import gradescope)
    (gradescope-test-suite null)
    `),
  ).toEqual({ tests: [] })
})

// The point of a struct rather than a bare string: an instructor can build a
// case by hand, and a string output is used verbatim rather than re-rendered
// (which would print it with its quotes).
test('a hand-built test result renders on its own, with a string output verbatim', async () => {
  expect(
    await runJson(`
    (import gradescope)
    (gradescope-test-result "style" "passed" 2 3 "Nicely done.")
    `),
  ).toEqual({
    name: 'style',
    status: 'passed',
    score: 2,
    max_score: 3,
    output: 'Nicely done.',
  })
})

test('the suite accessors reach the cases it built', async () => {
  expect(
    await runProgram(`
    (import test)
    (import gradescope)
    (length (gradescope-test-suite-output-tests
              (gradescope-test-suite (list (test-result-ok "a") (test-result-ok "b")))))
    `),
  ).toEqual(['2'])
})

test('a list of something other than test results is an error', async () => {
  expect(
    await runProgram(`
    (import gradescope)
    (gradescope-test-suite (list 1 2))
    `),
  ).toEqual([
    'Runtime error: (error) gradescope-test-suite: expected a list of test results',
  ])
})

test('a non-list is caught by the contract', async () => {
  expect(
    await runProgram(`
    (import gradescope)
    (gradescope-test-suite 5)
    `),
  ).toEqual(['Runtime error: (error) expected a list, received number'])
})

// The library is written in Scamper and calls its own struct constructors, so
// it has to keep working when its names are not in the importer's scope.
test('a qualified import works', async () => {
  expect(
    await runJson(`
    (import test)
    (import gradescope gs)
    (gs.gradescope-test-suite (list (test-result-ok "a")))
    `),
  ).toEqual({
    tests: [
      {
        name: 'a',
        status: 'passed',
        score: 1,
        max_score: 1,
        output: 'Test "a"\n✅ Passed!',
      },
    ],
  })
})

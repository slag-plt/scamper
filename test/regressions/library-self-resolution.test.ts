import { afterEach, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import { runProgram } from '../harness.js'

function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) => Promise.resolve(files[f]),
  } as unknown as ReturnType<typeof fs.getFS>)
}

afterEach(() => {
  vi.restoreAllMocks()
})

// Regression tests for the re-homing hole found while adding the `gradescope`
// library (#404), which is the first builtin written in Scamper rather than as
// a wrapper around JS -- so it is the first whose functions call names of their
// own module.
//
// Env.rehomeExports re-homes a module's bindings so their free names resolve
// against the module rather than against whoever imported it. It only ever
// walked the bindings themselves, and a documented library function is not its
// own binding: contract insertion (scheme/contract.ts) replaces it with a
// wrapper that closes over the original, and it is the original -- reachable
// only through the wrapper's captured scopes -- that holds the calls. So every
// contract-carrying library function resolved its calls against the running
// program's scope instead. The fix walks captured scopes too.

test('a library function is unaffected by a user redefining what it calls', async () => {
  // test-case (test.scm) calls prelude's `equal?` and `error` internally.
  expect(
    await runProgram(`
    (import test)
    (define equal? "not a function")
    (define error "not a function either")
    (test-case "add" (lambda (a b) (= a b)) 4 (lambda () (+ 2 2)))
    `),
  ).toEqual(['Test "add"\n✅ Passed!'])
})

test('a library function reaches its own module through a qualified import', async () => {
  // gradescope-test-suite builds structs its own module defines, which a
  // qualified import deliberately keeps out of the importer's scope.
  const [line] = await runProgram(`
  (import test)
  (import gradescope gs)
  (gs.gradescope-test-suite (list (test-result-ok "a")))
  `)
  expect(JSON.parse(line)).toEqual({
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

// The walk must not *steal* a closure that already has a home: a module can
// capture a closure from a module it imported, and that closure has to keep
// resolving against its own module, not the capturing one.
test('a closure captured from another module keeps its own module', async () => {
  mockFS({
    'b.scm': [
      '(define secret 42)',
      '(define get-secret (lambda () secret))',
      '(export get-secret)',
    ].join('\n'),
    'a.scm': [
      '(import "b.scm" b)',
      '(define wrapper (let ([g b.get-secret]) (lambda () (g))))',
      '(export wrapper)',
    ].join('\n'),
  })
  expect(
    await runProgram(`
    (import "a.scm")
    (wrapper)
    `),
  ).toEqual(['42'])
})

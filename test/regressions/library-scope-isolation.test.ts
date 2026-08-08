import { describe, expect, test, vi } from 'vitest'
import { runProgram } from '../harness.js'
import * as FS from '../../src/fs'
import * as S from '../../src/scheme'
import { Env, LoggingChannel, Module, Value } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { Scheduler } from '../../src/lpm/scheduler'
import { MockFileSystem } from '../stubs/mock-file-system'
import { patchSchedulerYieldForTests } from '../util'

patchSchedulerYieldForTests()

/** Runs `src` on a real Scheduler with `files` on a mock file system. */
async function runWithFiles(
  files: Record<string, string>,
  src: string,
): Promise<string[]> {
  const fs = new MockFileSystem()
  for (const [name, contents] of Object.entries(files)) {
    await fs.saveFile(name, contents)
  }
  vi.spyOn(FS, 'getFS').mockReturnValue(fs)
  const { prog, diagnostics } = await S.compile(src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  if (prog === undefined) throw new Error('compile produced no program')
  const ch = new LoggingChannel(true, false)
  const sched = new Scheduler()
  await new Promise<void>((resolve) => {
    sched.schedule({
      id: crypto.randomUUID(),
      fiber: new Fiber(prog, S.mkInitialEnv()),
      out: ch,
      err: ch,
      isTracing: false,
      onComplete: resolve,
    })
  })
  sched.pauseExecution()
  return [...(ch.log as string[]), ...ch.errLog]
}

// A library's closures must resolve their free names against the *library's*
// own scope. They used not to: Env.lookup consults the importer's top level
// before its imports, and Env.extendWithImport re-homed a module only when it
// had private (non-exported) bindings -- which no builtin library does. So
// every name the prelude used internally was capturable by a user define
// (#336): `(define car 5)` broke `list-ref`, `(define cdr 5)` broke `length`.
describe('a user define cannot capture a library\'s internals', () => {
  test('shadowing a list primitive leaves library functions working', async () => {
    expect(await runProgram('(define car 5)\n(list-ref (list 1 2) 1)')).toEqual([
      '2',
    ])
    expect(await runProgram('(define cdr 5)\n(length (list 1 2 3))')).toEqual([
      '3',
    ])
    expect(
      await runProgram('(define null? 5)\n(all-satisfy? number? (list 1 2))'),
    ).toEqual(['#t'])
    expect(await runProgram('(define + 5)\n(length (list 1 2 3))')).toEqual(['3'])
    expect(
      await runProgram('(define map 5)\n(filter even? (list 1 2 3 4))'),
    ).toEqual(['(list 2 4)'])
  })

  test('a contract violation still reports properly when its helpers are bound', async () => {
    // contract.ts builds a violation's message with `string-append` and checks
    // a rest parameter with `all-satisfy?`; both were capturable, so a
    // contract error turned into "Not a function or closure: 5" pointing at a
    // prelude line number.
    expect(await runProgram('(define string-append 5)\n(list-ref 1 2)')).toEqual([
      'Runtime error [2:1-2:14]: (error) expected a list, received number',
    ])
    expect(await runProgram('(define all-satisfy? 5)\n(+ 1 "a")')).toEqual([
      'Runtime error [2:1-2:9]: (error) expected every value of v1 to be a number, but at least one was not',
    ])
  })

  test('the user\'s own binding still shadows for the user\'s own code', async () => {
    // Isolation runs one way only: the library stops seeing the user's scope,
    // but the user's `car` is still theirs.
    expect(await runProgram('(define car 5)\ncar')).toEqual(['5'])
    expect(await runProgram('(define car 5)\n(car (list 1 2))')).toEqual([
      'Runtime error [2:1-2:16]: Not a function or closure: 5',
    ])
  })

  test('a user closure passed into a library function still works', async () => {
    // Isolation must not cut the other way: values crossing the boundary carry
    // their own captured scope.
    expect(
      await runProgram('(define f (lambda (x) (+ x 1)))\n(map f (list 1 2))'),
    ).toEqual(['(list 2 3)'])
    expect(
      await runProgram('(define acc 10)\n(map (lambda (x) (+ x acc)) (list 1 2))'),
    ).toEqual(['(list 11 12)'])
  })
})

describe('re-homing preserves a module\'s whole export set', () => {
  test('a binding whose value is undefined survives the import', () => {
    // Regression: rehomeExports rebuilt the exported Module by testing
    // `bindings.get(name) !== undefined`, which silently dropped any binding
    // whose *value* is undefined -- and Scamper's `void` is exactly that. It
    // only surfaced once every import began re-homing.
    const mod = new Module()
    mod.registerValue('void', undefined)
    mod.registerValue('x', 1)
    const env = Env.empty.extendWithImport('m', mod)
    expect(env.has('void')).toBe(true)
    expect(env.get('void')).toBe(undefined)
    expect(env.get('x')).toBe(1)
  })

  test('void is still reachable through the prelude', async () => {
    // `void` is a value, not a procedure -- and its value is undefined, which
    // is precisely what the dropped-binding bug keyed on.
    expect(await runProgram('(void? void)')).toEqual(['#t'])
  })

  test('an exported closure still reaches its private siblings', () => {
    // The case re-homing was originally written for; it must keep working now
    // that it runs unconditionally.
    const mod = new Module()
    mod.allBindings = new Map<string, Value>([
      ['helper', 1],
      ['exported', 2],
    ])
    mod.registerValue('exported', 2)
    const env = Env.empty.extendWithImport('m', mod)
    expect(env.has('exported')).toBe(true)
    expect(env.has('helper')).toBe(false)
  })
})

// A student's own multi-file program hits the same mechanism: the helper
// module and the main program each have their own top level.
describe('a file module is isolated from its importer\'s scope', () => {
  test("the importer's define cannot capture the module's internals", async () => {
    expect(
      await runWithFiles(
        { 'm.scm': '(define-export second (lambda (l) (car (cdr l))))' },
        '(import "m.scm")\n(define car 5)\n(second (list 1 2 3))',
      ),
    ).toEqual(['2'])
  })

  test("the module cannot capture the importer's define either way round", async () => {
    // The import statement runs before the define here, and after it above;
    // isolation must not depend on statement order.
    expect(
      await runWithFiles(
        { 'm.scm': '(define-export second (lambda (l) (car (cdr l))))' },
        '(define car 5)\n(import "m.scm")\n(second (list 1 2 3))',
      ),
    ).toEqual(['2'])
  })
})

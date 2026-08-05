import { describe, expect, test, vi } from 'vitest'
import * as FS from '../../src/fs'
import * as S from '../../src/scheme'
import { LoggingChannel } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { Scheduler } from '../../src/lpm/scheduler'
import { MockFileSystem } from '../stubs/mock-file-system'
import { patchSchedulerYieldForTests } from '../util'

// Regression test for #329: a file module was loaded by running its program in
// an *empty* environment, so the standard library was unavailable while the
// module loaded. Any top-level statement needing a prelude/runtime binding
// failed at load time with "Variable not found" -- which broke `struct`
// entirely (it expands to calls to `##mkCtorFn##` and friends, which live in
// the `runtime` library).
//
// The fix seeds the module fiber's env the same way a user program's is
// seeded (mkInitialEnv). Only the `Scheduler` executes file imports, so these
// tests drive the real scheduler over a mock file system.

patchSchedulerYieldForTests()

/**
 * Runs `src` on a real Scheduler with `files` visible on a mock file system.
 * @returns the program's displayed output and reported errors.
 */
async function run(
  files: Record<string, string>,
  src: string,
): Promise<{ out: string[]; errs: string[] }> {
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
  return { out: ch.log as string[], errs: ch.errLog }
}

describe('#329: a file module loads with the standard library available', () => {
  test('a top-level define calling a prelude function resolves at load', async () => {
    const { out, errs } = await run(
      { 'm.scm': '(define-export v (* 6 7))' },
      '(import "m.scm")\nv',
    )
    expect(errs).toEqual([])
    expect(out).toEqual(['42'])
  })

  test('a struct in a file module loads and its parts are usable', async () => {
    const { out, errs } = await run(
      {
        'm.scm': '(struct point (x y))\n(export point point? point-x point-y)',
      },
      '(import "m.scm")\n(point-x (point 3 4))\n(point? (point 3 4))',
    )
    expect(errs).toEqual([])
    expect(out).toEqual(['3', '#t'])
  })

  test('a struct in a qualified file module is usable through its alias', async () => {
    const { out, errs } = await run(
      { 'm.scm': '(struct point (x y))\n(export point point-y)' },
      '(import "m.scm" geo)\n(geo.point-y (geo.point 3 4))',
    )
    expect(errs).toEqual([])
    expect(out).toEqual(['4'])
  })

  test("the standard library is not re-exported by the module it loaded", async () => {
    // The module's env imports prelude/runtime; those bindings live in the
    // env's *imports*, not its top level, so they must not leak into what the
    // module exports.
    const { out, errs } = await run(
      { 'm.scm': '(define-export v 1)' },
      '(import "m.scm")\nv',
    )
    expect(errs).toEqual([])
    expect(out).toEqual(['1'])
  })
})

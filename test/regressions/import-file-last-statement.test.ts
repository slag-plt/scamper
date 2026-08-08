import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import type { FS } from '../../src/fs/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { runProgram } from '../harness.js'

// Regression test for #341: a `(import "...")` of a *file* as the last
// statement of a program aborted with
//
//   ICE: Scheduling invariant violated: scheduling completed fibers is
//   disallowed! (Scheduler.schedule)
//
// The scheduler's `import-file` branch re-scheduled the importing task
// unconditionally after `advanceStmt()` -- on both the module-loaded and the
// load-failed path. When the import was the final statement, advanceStmt
// completed the program, and `schedule` rejects a finished fiber.
//
// Same shape as the `block-on` branch, which already guards with isDone() and
// signals completion instead. The ICE was thrown inside a detached promise, so
// it surfaced as an unhandled rejection and the run simply never completed --
// hence the timeouts these tests would hit before the fix.

let fs: MockFileSystem

beforeEach(async () => {
  fs = await MockFileSystem.create()
  setBackend(localBackend(fs))
})

/** An FS whose every file exists but refuses to load, as a directory would. */
function unreadableFS(): FS {
  const nope = () => Promise.reject(new Error('EISDIR'))
  return {
    getFileList: () => Promise.resolve([]),
    fileExists: () => Promise.resolve(true),
    loadFile: nope,
    saveFile: nope,
    deleteFile: nope,
    renameFile: nope,
  }
}

describe('#341: a file import as the last statement of a program', () => {
  test('completes the program instead of raising a scheduling ICE', async () => {
    await fs.saveFile('mod.scm', '(define-export helper 42)\n')
    expect(await runProgram('(display 1)\n(import "mod.scm")')).toEqual(['1'])
  })

  test('is still the sole statement case', async () => {
    await fs.saveFile('mod.scm', '(define-export helper 42)\n')
    expect(await runProgram('(import "mod.scm")')).toEqual([])
  })

  test("binds the module's exports when it is not the last statement", async () => {
    // The pre-#341 behavior that must be preserved: a non-final import resumes
    // the fiber with the module in scope.
    await fs.saveFile('mod.scm', '(define-export helper 42)\n')
    expect(await runProgram('(import "mod.scm")\n(display helper)')).toEqual([
      '42',
    ])
  })

  test('reports the failure and completes when the file cannot be loaded', async () => {
    // The other unguarded re-schedule: the load-failure path also advances the
    // statement and re-schedules. A file that exists but will not load (a
    // directory, a permission error) as the last statement hit the same ICE.
    setBackend(localBackend(unreadableFS()))
    const output = await runProgram('(display 1)\n(import "mod.scm")')
    expect(output[0]).toBe('1')
    expect(output.join('\n')).toMatch(/failed to load/)
  })
})

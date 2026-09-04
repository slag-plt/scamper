import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import type { FS } from '../../src/fs/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { runProgram } from '../harness.js'

// Regression test for #342: an error raised inside a blocking primitive's async
// action carried no source range, so the IDE could not underline the call that
// caused it:
//
//   Runtime error: File "missing.txt" does not exist          <- no range
//   Runtime error [3:1-3:9]: (error) expected a number, ...   <- ordinary error
//
// The ScamperError is constructed inside the action, long after the call that
// suspended the fiber has been left behind, and the scheduler's block-on
// rejection handler wrapped a non-ScamperError rejection without attaching one
// either. Every `file` procedure, `with-file`, and `with-image-from-url` were
// affected.
//
// The fix records the suspending call's range on the SuspendSignal -- using the
// same call-site recovery applyFn already performs for a *synchronous*
// primitive's error (see #254/#239 and contract-error-call-site.test.ts) -- and
// the scheduler attaches it to an error that has none.

let fs: MockFileSystem

beforeEach(async () => {
  fs = await MockFileSystem.create()
  setBackend(localBackend(fs))
})

describe("#342: a blocking primitive's error points at its call", () => {
  test('a top-level call to a file procedure', async () => {
    expect(
      await runProgram('(import file)\n(file->string "missing.txt")'),
    ).toEqual(['Runtime error [2:1-2:28]: File "missing.txt" does not exist'])
  })

  test('the range tracks the statement the call occurs in', async () => {
    await fs.saveFile('here.txt', 'hi')
    expect(
      await runProgram(
        '(import file)\n(file->string "here.txt")\n(file->string "missing.txt")',
      ),
    ).toEqual([
      '"hi"',
      'Runtime error [3:1-3:28]: File "missing.txt" does not exist',
    ])
  })

  test("a call inside a user function reports that call, not the function's", async () => {
    // The same blind spot the contract-error fix had to cover: the failing call
    // sits inside f's body, which is where the underline belongs.
    expect(
      await runProgram(
        '(import file)\n(define f (lambda (n) (file->string n)))\n(f "missing.txt")',
      ),
    ).toEqual(['Runtime error [2:23-2:38]: File "missing.txt" does not exist'])
  })

  test('a write that fails reports the call site', async () => {
    setBackend(localBackend(unwritableFS()))
    expect(
      await runProgram('(import file)\n(string->file "data" "out.txt")'),
    ).toEqual([
      'Runtime error [2:1-2:31]: Could not write to the file "out.txt"',
    ])
  })

  test('image-load, bound directly rather than wrapped (#452)', async () => {
    // On the good side of the split below: `image` binds it to the primitive
    // itself, so the range applyFn recovers is the student's own call.
    expect(
      await runProgram('(import image)\n(image-load "missing.png")'),
    ).toEqual(['Runtime error [2:1-2:26]: File "missing.png" does not exist'])
  })

  test('with-file reports the call site too (gap closed in #476)', async () => {
    // with-file and with-image-from-url are the only two primitives called from
    // a *library-defined Scheme* body, so the range applyFn could recover was
    // the one inside prelude.scm/image.scm rather than the student's call --
    // and reporting unlocated was the lesser evil. A call made from library
    // source now passes along the range its own caller was called from (see
    // applyFn), which walks out of that wrapper for free.
    expect(
      await runProgram('(with-file "missing.txt" (lambda (s) s))'),
    ).toEqual([
      'Runtime error [1:1-1:40]: File "missing.txt" does not exist',
    ])
  })

  test('a rejection that is not a ScamperError also gets a range', async () => {
    // file-exists? hands the FS promise to the scheduler unguarded, so a host
    // failure arrives as a plain Error and is wrapped by the block-on handler.
    // That wrapper had no range either.
    setBackend(localBackend(brokenFS()))
    expect(
      await runProgram('(import file)\n(file-exists? "any.txt")'),
    ).toEqual(['Runtime error [2:1-2:24]: host is on fire'])
  })
})

/** An FS that refuses every write. */
function unwritableFS(): FS {
  return {
    ...emptyFS(),
    saveFile: () => Promise.reject(new Error('EACCES')),
  }
}

/** An FS whose existence check fails outright, with a non-ScamperError. */
function brokenFS(): FS {
  return {
    ...emptyFS(),
    fileExists: () => Promise.reject(new Error('host is on fire')),
  }
}

function emptyFS(): FS {
  const nope = () => Promise.reject(new Error('unimplemented'))
  return {
    getFileList: () => Promise.resolve([]),
    fileExists: () => Promise.resolve(false),
    loadFile: nope,
    saveFile: nope,
    deleteFile: nope,
    renameFile: nope,
  }
}

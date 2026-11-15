import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { reductionTrace } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/758
//
// #737 made a frame completion a major step, so a trace shows the call
// becoming its value -- but only in `Fiber.stepFrame`. A frame can also
// complete on the blocking-primitive path: `Fiber.resumeWithValue` pushes the
// resolved value, finds the frame finished, and calls `completeCurrentFrame`
// without producing a `StepResult` at all, so the scheduler's `block-on`
// branch never classifies it and the tracer is never offered the step.
//
// Every blocking primitive is a library procedure, so each is reached through
// its contract wrapper: the wrapper's frame is what the resume completes, and
// that completion is exactly the reduction in which the call becomes its
// value in the caller. `(string-length (file->string "data.txt"))` therefore
// traces straight to `12`.
//
// The assertions are whole step sequences, as #737's are: a fix that merely
// adds a step somewhere must still produce exactly these reductions, and in
// particular must not add a second copy of a statement's final value.

let fs: MockFileSystem

beforeEach(async () => {
  // A fresh in-memory FS per test, as test/libs/file.test.ts uses: these are
  // about the trace, not about Node I/O.
  fs = await MockFileSystem.create()
  setBackend(localBackend(fs))
  await fs.saveFile('data.txt', 'hello world\n')
})

describe('a blocking primitive that completes a frame takes a trace step (#758)', () => {
  test("the issue's own program shows file->string reducing", async () => {
    expect(
      await reductionTrace(
        '(import file)\n(string-length (file->string "data.txt"))',
      ),
    ).toEqual([
      '(string-length (file->string "data.txt"))',
      '(string-length "hello world\\n")',
      '12',
    ])
  })

  test('the blocking call need not be the only argument', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(string-append "<" (file->string "data.txt") ">")',
      ),
    ).toEqual([
      '(string-append "<" (file->string "data.txt") ">")',
      '(string-append "<" "hello world\\n" ">")',
      '"<hello world\\n>"',
    ])
  })

  test('two blocking calls take a step each, not one between them', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(list (file->string "data.txt") (file->string "data.txt"))',
      ),
    ).toEqual([
      '(list (file->string "data.txt") (file->string "data.txt"))',
      '(list "hello world\\n" (file->string "data.txt"))',
      '(list "hello world\\n" "hello world\\n")',
    ])
  })

  test('file->string is not the only such primitive', async () => {
    // file-exists? suspends the same way, and `if` shows its test reducing.
    expect(
      await reductionTrace(
        '(import file)\n(if (file-exists? "data.txt") "yes" "no")',
      ),
    ).toEqual([
      '(if (file-exists? "data.txt") "yes" "no")',
      '(if #t "yes" "no")',
      '"yes"',
    ])
  })

  test('a blocking call standing alone gains no duplicate at the end', async () => {
    // The mirror of #737's own last case, and the property a fix must keep:
    // the frame the resume completes here is the statement's own, and with no
    // frames left there is no state to raise -- so the step must not produce a
    // second copy of the value the statement already ends on.
    expect(
      await reductionTrace('(import file)\n(file->string "data.txt")'),
    ).toEqual(['(file->string "data.txt")', '"hello world\\n"'])
  })

  test('a blocking call inside a define traces the same way', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(define n (string-length (file->string "data.txt")))',
      ),
    ).toEqual([
      '(string-length (file->string "data.txt"))',
      '(string-length "hello world\\n")',
      '12',
    ])
  })
})

import { afterAll, afterEach, beforeAll, describe, expect, test } from 'vitest'
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import * as Scheme from '../../src/scheme'
import * as LPM from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { Scheduler } from '../../src/lpm/scheduler'
import { diagnosticToError } from '../../src/scheme/diagnostic'
import { setFS } from '../../src/fs'
import NodeFileSystem from '../../src/fs/node'
import { runProgram } from '../harness.js'

// The `file` library (issue #315): whole-file reads and writes, each of which
// suspends the fiber on an async filesystem action (SuspendSignal / Scheduler
// `block-on`). Because those only resolve under the scheduler, the happy path
// cannot run on test/harness.ts's synchronous runProgram -- runFileProgram
// below drives a real Scheduler instead. Argument contracts still fail eagerly,
// so those cases use the ordinary harness.

let dir: string

beforeAll(async () => {
  dir = await mkdtemp(path.join(tmpdir(), 'scamper-file-'))
  // Mirror the CLI: a Node-backed FS rooted at a scratch directory.
  setFS(await NodeFileSystem.create(dir))
})

afterAll(async () => {
  await rm(dir, { recursive: true, force: true })
})

afterEach(async () => {
  // Each test starts from an empty directory so writes can't leak between them.
  for (const f of await (await import('node:fs/promises')).readdir(dir)) {
    await rm(path.join(dir, f), { force: true })
  }
})

/** Strips the `[line:col-line:col]` span from an error line. Library contract
 *  errors point into the .scm source, so their ranges shift on any library
 *  edit; the messages are what these tests are about. */
const stripRanges = (s: string): string => s.replace(/ \[\d+:\d+-\d+:\d+\]/g, '')

/** Compiles and runs `src` under a real Scheduler, returning its output lines. */
async function runFileProgram(src: string): Promise<string[]> {
  const out = new LPM.LoggingChannel()
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  diagnostics.forEach((d) => {
    out.report(diagnosticToError(d))
  })
  if (out.log.length !== 0) {
    return (out.log as string[]).map(stripRanges)
  }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  const fiber = new Fiber(prog, Scheme.mkInitialEnv())
  const sched = new Scheduler()
  await new Promise<void>((resolve) => {
    sched.schedule({
      id: crypto.randomUUID(),
      fiber,
      out,
      err: out,
      isTracing: false,
      onComplete: resolve,
    })
  })
  return (out.log as string[]).map(stripRanges)
}

const read = (name: string) => readFile(path.join(dir, name), 'utf-8')
const write = (name: string, contents: string) =>
  writeFile(path.join(dir, name), contents, 'utf-8')

describe('file->string', () => {
  test('reads a file back as a string', async () => {
    await write('greet.txt', 'hello\nworld\n')
    expect(await runFileProgram('(import file)\n(file->string "greet.txt")')).toEqual([
      '"hello\nworld\n"',
    ])
  })

  test('reads an empty file as the empty string', async () => {
    await write('empty.txt', '')
    expect(await runFileProgram('(import file)\n(file->string "empty.txt")')).toEqual(['""'])
  })

  test('a missing file raises a runtime error', async () => {
    expect(await runFileProgram('(import file)\n(file->string "nope.txt")')).toEqual([
      'Runtime error: File "nope.txt" does not exist',
    ])
  })

  test('a missing file is catchable by with-handler', async () => {
    expect(
      await runFileProgram(`
(import file)
(with-handler (lambda (e) "caught") (lambda () (file->string "nope.txt")))
`),
    ).toEqual(['"caught"'])
  })
})

describe('file->lines', () => {
  test('splits on newlines', async () => {
    await write('lines.txt', 'a\nb\nc')
    expect(await runFileProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "b" "c")',
    ])
  })

  test('a trailing newline does not produce a final empty line', async () => {
    await write('lines.txt', 'a\nb\nc\n')
    expect(await runFileProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "b" "c")',
    ])
  })

  test('only one trailing empty line is dropped', async () => {
    // "a\n\n" is a line "a" followed by a genuinely blank line.
    await write('lines.txt', 'a\n\n')
    expect(await runFileProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "")',
    ])
  })

  test('handles CRLF line endings', async () => {
    await write('crlf.txt', 'a\r\nb\r\n')
    expect(await runFileProgram('(import file)\n(file->lines "crlf.txt")')).toEqual([
      '(list "a" "b")',
    ])
  })

  test('an empty file reads as the empty list', async () => {
    await write('empty.txt', '')
    expect(await runFileProgram('(import file)\n(file->lines "empty.txt")')).toEqual(['null'])
  })
})

describe('string->file', () => {
  test('creates a file and round-trips through file->string', async () => {
    expect(
      await runFileProgram(`
(import file)
(string->file "some text" "out.txt")
(file->string "out.txt")
`),
    ).toEqual(['void', '"some text"'])
    expect(await read('out.txt')).toBe('some text')
  })

  test('replaces the contents of an existing file', async () => {
    await write('out.txt', 'original contents, much longer')
    expect(
      await runFileProgram(`
(import file)
(string->file "new" "out.txt")
(file->string "out.txt")
`),
    ).toEqual(['void', '"new"'])
    expect(await read('out.txt')).toBe('new')
  })
})

describe('lines->file', () => {
  test('writes one line each, ending with a trailing newline', async () => {
    expect(
      await runFileProgram('(import file)\n(lines->file (list "a" "b" "c") "out.txt")'),
    ).toEqual(['void'])
    expect(await read('out.txt')).toBe('a\nb\nc\n')
  })

  test('round-trips with file->lines', async () => {
    expect(
      await runFileProgram(`
(import file)
(lines->file (list "one" "two" "three") "out.txt")
(file->lines "out.txt")
`),
    ).toEqual(['void', '(list "one" "two" "three")'])
  })

  test('the empty list writes an empty file, not a lone newline', async () => {
    expect(await runFileProgram('(import file)\n(lines->file null "out.txt")')).toEqual(['void'])
    expect(await read('out.txt')).toBe('')
  })

  test('a non-string element is a runtime error', async () => {
    expect(
      await runFileProgram('(import file)\n(lines->file (list "a" 5) "out.txt")'),
    ).toEqual([
      'Runtime error: lines->file: expected a list of strings, but the list contains number',
    ])
  })
})

// Argument contracts are checked before the primitive suspends, so these fail
// eagerly and run on the ordinary synchronous harness.
describe('argument contracts', () => {
  test('reject non-string filenames', async () => {
    expect(
      await runProgram(
        `
(import file)
(file->string 5)
(file->lines 5)
(string->file "s" 5)
(lines->file (list "a") 5)
`,
        { stripRanges: true },
      ),
    ).toEqual([
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
    ])
  })

  test('reject wrong argument types for the written value', async () => {
    expect(
      await runProgram(
        `
(import file)
(string->file 5 "out.txt")
(lines->file "not a list" "out.txt")
`,
        { stripRanges: true },
      ),
    ).toEqual([
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a list, received string',
    ])
  })
})

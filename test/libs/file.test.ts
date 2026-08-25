import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { runProgram } from './harness.js'

// The `file` library (issue #315): whole-file reads and writes, each of which
// suspends the fiber on an async filesystem action (SuspendSignal / Scheduler
// `block-on`). Those only resolve under the scheduler, which is what runProgram
// drives, so the happy path runs there like anything else.
//
// Backed by the in-memory MockFileSystem: these tests are about the library's
// semantics, not Node I/O, and an in-memory FS keeps them fast and leaves no
// temp directories to clean up. The real filesystem is covered a tier up, by
// the CLI end-to-end test in test/apps/cli/cli.test.ts.

let fs: MockFileSystem

beforeEach(async () => {
  // A fresh FS per test, so writes can't leak between them.
  fs = await MockFileSystem.create()
  setBackend(localBackend(fs))
})

const read = (name: string) => fs.loadFile(name)
const write = (name: string, contents: string) => fs.saveFile(name, contents)

describe('file-exists?', () => {
  test('is #t for a file that exists and #f otherwise', async () => {
    await write('here.txt', 'contents')
    expect(
      await runProgram(`
(import file)
(file-exists? "here.txt")
(file-exists? "nope.txt")
`),
    ).toEqual(['#t', '#f'])
  })

  test('sees a file created earlier in the same program', async () => {
    expect(
      await runProgram(`
(import file)
(file-exists? "made.txt")
(string->file "x" "made.txt")
(file-exists? "made.txt")
`),
    ).toEqual(['#f', 'void', '#t'])
  })

  test('an empty file still exists', async () => {
    await write('empty.txt', '')
    expect(await runProgram('(import file)\n(file-exists? "empty.txt")')).toEqual(['#t'])
  })

  test('guards a read that would otherwise raise', async () => {
    // The idiom file-exists? is for: check before reading, rather than wrapping
    // the read in with-handler.
    expect(
      await runProgram(`
(import file)
(if (file-exists? "nope.txt") (file->string "nope.txt") "no such file")
`),
    ).toEqual(['"no such file"'])
  })
})

describe('file->string', () => {
  test('reads a file back as a string', async () => {
    await write('greet.txt', 'hello\nworld\n')
    // N.B., the newlines render as `\n` escapes, not as real line breaks: a
    // rendered string literal is now always a single flat line.
    expect(await runProgram('(import file)\n(file->string "greet.txt")')).toEqual([
      '"hello\\nworld\\n"',
    ])
  })

  test('reads an empty file as the empty string', async () => {
    await write('empty.txt', '')
    expect(await runProgram('(import file)\n(file->string "empty.txt")')).toEqual(['""'])
  })

  test('a missing file raises a runtime error', async () => {
    expect(await runProgram('(import file)\n(file->string "nope.txt")')).toEqual([
      'Runtime error: File "nope.txt" does not exist',
    ])
  })

  test('a missing file is catchable by with-handler', async () => {
    expect(
      await runProgram(`
(import file)
(with-handler (lambda (e) "caught") (lambda () (file->string "nope.txt")))
`),
    ).toEqual(['"caught"'])
  })
})

describe('file->lines', () => {
  test('splits on newlines', async () => {
    await write('lines.txt', 'a\nb\nc')
    expect(await runProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "b" "c")',
    ])
  })

  test('a trailing newline does not produce a final empty line', async () => {
    await write('lines.txt', 'a\nb\nc\n')
    expect(await runProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "b" "c")',
    ])
  })

  test('only one trailing empty line is dropped', async () => {
    // "a\n\n" is a line "a" followed by a genuinely blank line.
    await write('lines.txt', 'a\n\n')
    expect(await runProgram('(import file)\n(file->lines "lines.txt")')).toEqual([
      '(list "a" "")',
    ])
  })

  test('handles CRLF line endings', async () => {
    await write('crlf.txt', 'a\r\nb\r\n')
    expect(await runProgram('(import file)\n(file->lines "crlf.txt")')).toEqual([
      '(list "a" "b")',
    ])
  })

  test('an empty file reads as the empty list', async () => {
    await write('empty.txt', '')
    expect(await runProgram('(import file)\n(file->lines "empty.txt")')).toEqual(['null'])
  })
})

describe('string->file', () => {
  test('creates a file and round-trips through file->string', async () => {
    expect(
      await runProgram(`
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
      await runProgram(`
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
      await runProgram('(import file)\n(lines->file (list "a" "b" "c") "out.txt")'),
    ).toEqual(['void'])
    expect(await read('out.txt')).toBe('a\nb\nc\n')
  })

  test('round-trips with file->lines', async () => {
    expect(
      await runProgram(`
(import file)
(lines->file (list "one" "two" "three") "out.txt")
(file->lines "out.txt")
`),
    ).toEqual(['void', '(list "one" "two" "three")'])
  })

  test('the empty list writes an empty file, not a lone newline', async () => {
    expect(await runProgram('(import file)\n(lines->file null "out.txt")')).toEqual(['void'])
    // The existence check matters: without it this passes even if nothing is
    // written at all, so long as reading a missing file yields "".
    expect(await fs.fileExists('out.txt')).toBe(true)
    expect(await read('out.txt')).toBe('')
  })

  test('a non-string element is a runtime error', async () => {
    expect(
      await runProgram('(import file)\n(lines->file (list "a" 5) "out.txt")'),
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
      await runProgram(`
(import file)
(file-exists? 5)
(file->string 5)
(file->lines 5)
(string->file "s" 5)
(lines->file (list "a") 5)
`),
    ).toEqual([
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
    ])
  })

  test('reject wrong argument types for the written value', async () => {
    expect(
      await runProgram(`
(import file)
(string->file 5 "out.txt")
(lines->file "not a list" "out.txt")
`),
    ).toEqual([
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a list, received string',
    ])
  })
})

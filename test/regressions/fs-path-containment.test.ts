import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { existsSync } from 'node:fs'
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { setFS } from '../../src/fs'
import NodeFileSystem from '../../src/fs/node'
import { compile } from '../../src/scheme'
import { runProgramAsync } from '../harness.js'

// Regression test for #340: NodeFileSystem resolved names with path.join and no
// containment check, so a name that climbed out of the root ("../x") or named
// an absolute path was honoured. Reads had been escapable for a while; #338
// added writes, which let a program clobber files outside the directory it was
// run from. The browser host (OPFS) is unaffected, so this is Node-only.
//
// The fix rejects any name that resolves outside the root, in the file system
// itself -- so the `file` library, `with-file`, and file imports are all
// covered at once.

let base: string
let root: string

beforeEach(async () => {
  base = await mkdtemp(path.join(tmpdir(), 'scamper-340-'))
  root = path.join(base, 'root')
  // A file and a module *outside* the root, both of which must stay unreachable.
  await writeFile(path.join(base, 'secret.txt'), 'classified\n', 'utf-8')
  await writeFile(path.join(base, 'outside.scm'), '(define-export x 1)\n', 'utf-8')
  setFS(await NodeFileSystem.create(root))
})

afterEach(async () => {
  await rm(base, { recursive: true, force: true })
})

describe('#340: the Node file system contains names to its root', () => {
  test('a name that climbs out of the root is rejected', async () => {
    const fs = await NodeFileSystem.create(root)
    await expect(fs.loadFile('../secret.txt')).rejects.toThrow(
      /outside the working directory/,
    )
    await expect(fs.fileExists('../secret.txt')).rejects.toThrow(
      /outside the working directory/,
    )
    await expect(fs.deleteFile('../secret.txt')).rejects.toThrow(
      /outside the working directory/,
    )
    await expect(fs.renameFile('../secret.txt', 'mine.txt')).rejects.toThrow(
      /outside the working directory/,
    )
  })

  test('a write that climbs out of the root is rejected and creates nothing', async () => {
    const fs = await NodeFileSystem.create(root)
    await expect(fs.saveFile('../escaped.txt', 'PWNED')).rejects.toThrow(
      /outside the working directory/,
    )
    expect(existsSync(path.join(base, 'escaped.txt'))).toBe(false)
  })

  test('an absolute name is rejected', async () => {
    const fs = await NodeFileSystem.create(root)
    await expect(fs.loadFile(path.join(base, 'secret.txt'))).rejects.toThrow(
      /outside the working directory/,
    )
  })

  test('names within the root, including nested ones, still resolve', async () => {
    const fs = await NodeFileSystem.create(root)
    await mkdir(path.join(root, 'data'))
    await fs.saveFile('data/notes.txt', 'hello')
    expect(await fs.loadFile('data/notes.txt')).toBe('hello')
    // A ".." that stays inside the root is fine: it is the escape that is
    // rejected, not the syntax.
    expect(await fs.loadFile('data/../data/notes.txt')).toBe('hello')
    expect(await fs.fileExists('data/notes.txt')).toBe(true)
  })
})

describe('#340: a program cannot reach outside the working directory', () => {
  test('a write outside the root reports an error and creates nothing', async () => {
    const output = await runProgramAsync(`
(import file)
(string->file "PWNED" "../escaped.txt")
`)
    expect(output.join('\n')).toMatch(/outside the working directory/)
    expect(existsSync(path.join(base, 'escaped.txt'))).toBe(false)
  })

  test('a read outside the root reports an error', async () => {
    const output = await runProgramAsync(`
(import file)
(file->string "../secret.txt")
`)
    expect(output.join('\n')).toMatch(/outside the working directory/)
  })

  test('an import from outside the root is a scope diagnostic, not a crash', async () => {
    const { diagnostics } = await compile('(import "../outside.scm")\n(display x)', {
      scopeCheck: true,
    })
    expect(diagnostics.map((d) => d.message).join('\n')).toMatch(
      /outside the working directory/,
    )
  })

  test('an import from outside the root reports an error and the program finishes', async () => {
    const output = await runProgramAsync('(import "../outside.scm")\n(display 1)')
    expect(output.join('\n')).toMatch(/outside the working directory/)
  })
})

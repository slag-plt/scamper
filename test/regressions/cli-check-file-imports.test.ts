import { afterAll, beforeAll, describe, expect, test } from 'vitest'
import { mkdtemp, rm, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { compile } from '../../src/scheme'
import { localBackend, setBackend } from '../../src/fs'
import NodeFileSystem from '../../src/fs/node'

// Regression test for #287: `scamper --check foo.scm` (compile with
// { scopeCheck: true }) could not scope-check any program that used a file
// import. Scope checking resolves file imports through getFS(), but the CLI
// never initialized a file system outside the browser, so getFS() threw "File
// system not initialized" and the check bailed out without running.
//
// The fix lets a non-browser host install its own file system (setBackend) and wires
// a NodeFileSystem into the CLI, rooted at the checked file's directory. This
// test exercises that path directly: with a Node-backed FS installed, a
// file-importing program scope-checks like any other.

describe('#287: scope-checking file imports outside the browser', () => {
  let dir: string

  beforeAll(async () => {
    dir = await mkdtemp(path.join(tmpdir(), 'scamper-287-'))
    await writeFile(path.join(dir, 'bar.scm'), '(define-export helper 1)\n', 'utf-8')
    // Mirror the CLI: install a Node-backed FS rooted at the file's directory.
    setBackend(localBackend(await NodeFileSystem.create(dir)))
  })

  afterAll(async () => {
    await rm(dir, { recursive: true, force: true })
  })

  test('a file import resolves and its names are in scope', async () => {
    const { diagnostics } = await compile('(import "bar.scm")\n(display helper)', {
      scopeCheck: true,
    })
    expect(diagnostics.map((d) => d.message)).toEqual([])
  })

  test('a name the imported file does not define is still flagged', async () => {
    const { diagnostics } = await compile('(import "bar.scm")\n(display nope)', {
      scopeCheck: true,
    })
    expect(diagnostics.map((d) => d.message)).toEqual([
      "Undefined variable 'nope'",
    ])
  })
})

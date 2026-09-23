import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// Regression test for #638: a malformed character literal or string escape
// left the parser by being *thrown* rather than collected, since literals.ts
// raises a ScamperError where every other parse error pushes a diagnostic.
// Nothing on the run path caught it, so the CLI died with a raw Node stack
// trace naming src/scheme/literals.ts.

describe('a malformed literal is a parse diagnostic, not a thrown error', () => {
  test('a bad character literal', async () => {
    const log = await runProgram('(display #\\zzz)')
    expect(log).toEqual([
      'Parser error [1:10-1:14]: Invalid character literal: #\\zzz',
    ])
  })

  test('a rejected string escape', async () => {
    const log = await runProgram('(display "\\x41;")')
    expect(log).toEqual([
      'Parser error [1:10-1:16]: Hex escape codes not supported',
    ])
  })
})

// The user-visible symptom: the CLI reported the error the way it reports every
// other malformed input, instead of crashing with Node's stack trace.
test('the CLI reports a bad literal without a stack trace', { timeout: 30_000 }, () => {
  const dir = mkdtempSync(path.join(tmpdir(), 'scamper-638-'))
  const file = path.join(dir, 'bad-char.scm')
  writeFileSync(file, '(display #\\zzz)\n')
  try {
    const result = spawnSync('npx', ['tsx', 'src/app/cli/index.ts', file], {
      cwd: path.resolve(import.meta.dirname, '../..'),
      encoding: 'utf-8',
      timeout: 15000,
    })
    expect(result.stderr).toContain(
      'Parser error [1:10-1:14]: Invalid character literal: #\\zzz',
    )
    expect(result.stderr).not.toContain('literals.ts')
    expect(result.status).toBe(1)
  } finally {
    rmSync(dir, { recursive: true, force: true })
  }
})

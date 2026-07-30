import { spawnSync } from 'node:child_process'
import path from 'node:path'
import { describe, expect, test } from 'vitest'

const repoRoot = path.resolve(import.meta.dirname, '../../..')
const fixture = (name: string) => path.join(import.meta.dirname, 'fixtures', name)

function runCli(args: string[]) {
  return spawnSync('npx', ['tsx', 'src/app/cli/index.ts', ...args], {
    cwd: repoRoot,
    encoding: 'utf-8',
    timeout: 15000,
  })
}

describe('scamper CLI', () => {
  test('successful program prints its value and exits 0', () => {
    const result = runCli([fixture('success.scm')])

    expect(result.stdout).toBe('3\n')
    expect(result.status).toBe(0)
  })

  test('runtime error prints prior output, reports the error, and exits 1', () => {
    const result = runCli([fixture('runtime-error.scm')])

    expect(result.stdout).toBe('"before"\n')
    expect(result.stderr).toContain('Runtime error')
    expect(result.stderr).toContain('boom')
    expect(result.status).toBe(1)
  })

  test('no arguments prints usage and exits 0', () => {
    const result = runCli([])

    expect(result.stdout).toContain('Usage: scamper')
    expect(result.status).toBe(0)
  })

  test('--help prints usage and exits 0', () => {
    const result = runCli(['--help'])

    expect(result.stdout).toContain('Usage: scamper')
    expect(result.status).toBe(0)
  })

  // Regression for #265: colorsys is CommonJS, so under the CLI's Node/tsx ESM
  // interop a namespace import left rgb->hsv/hsv->rgb calling
  // colorsys.rgbToHsv/hsvToRgb, which were undefined ("... is not a function").
  // This tier runs the real tsx path where that surfaced; the fix is a default
  // import in color.ts.
  test('rgb->hsv and hsv->rgb resolve colorsys under Node/tsx (#265)', () => {
    const result = runCli([fixture('colorsys.scm')])

    expect(result.stdout).toBe('(hsv 0 100 100 255)\n(rgba 255 0 0 255)\n')
    expect(result.stderr).toBe('')
    expect(result.status).toBe(0)
  })
})

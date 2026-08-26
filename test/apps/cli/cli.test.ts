import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
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

  // A step is laid out by the same rules as the panes, and the "--> " marker is
  // part of the line: the step is laid out as beginning after it, so an `if`'s
  // branches sit under its test rather than under the marker, and eighty
  // columns still means the finished line. (reductionTrace in test/harness.ts
  // flattens this away -- those tests are about the reduction, not the layout.)
  test('--trace prints the reduction trace of factorial', () => {
    const result = runCli(['--trace', fixture('factorial.scm')])

    expect(result.stdout).toBe(
      [
        '(fact 3)',
        '--> (if (= 3 0)',
        '        1',
        '        (* 3 (fact (- 3 1))))',
        '--> (if #f',
        '        1',
        '        (* 3 (fact (- 3 1))))',
        '--> (* 3 (fact (- 3 1)))',
        '--> (* 3 (fact 2))',
        '--> (* 3',
        '       (if (= 2 0)',
        '           1',
        '           (* 2 (fact (- 2 1)))))',
        '--> (* 3',
        '       (if #f',
        '           1',
        '           (* 2 (fact (- 2 1)))))',
        '--> (* 3 (* 2 (fact (- 2 1))))',
        '--> (* 3 (* 2 (fact 1)))',
        '--> (* 3',
        '       (* 2',
        '          (if (= 1 0)',
        '              1',
        '              (* 1 (fact (- 1 1))))))',
        '--> (* 3',
        '       (* 2',
        '          (if #f',
        '              1',
        '              (* 1 (fact (- 1 1))))))',
        '--> (* 3 (* 2 (* 1 (fact (- 1 1)))))',
        '--> (* 3 (* 2 (* 1 (fact 0))))',
        '--> (* 3',
        '       (* 2',
        '          (* 1',
        '             (if (= 0 0)',
        '                 1',
        '                 (* 0 (fact (- 0 1)))))))',
        '--> (* 3',
        '       (* 2',
        '          (* 1',
        '             (if #t',
        '                 1',
        '                 (* 0 (fact (- 0 1)))))))',
        '--> (* 3 (* 2 (* 1 1)))',
        '--> (* 3 (* 2 1))',
        '--> (* 3 2)',
        '--> 6',
        '',
      ].join('\n'),
    )
    expect(result.status).toBe(0)
  })

  test('--trace prints the reduction trace of list length', () => {
    const result = runCli(['--trace', fixture('list-length.scm')])

    expect(result.stdout).toBe(
      [
        '(len (list 7 8))',
        '--> (if (null? (list 7 8))',
        '        0',
        '        (+ 1 (len (cdr (list 7 8)))))',
        '--> (if #f',
        '        0',
        '        (+ 1 (len (cdr (list 7 8)))))',
        '--> (+ 1 (len (cdr (list 7 8))))',
        '--> (+ 1 (len (list 8)))',
        '--> (+ 1',
        '       (if (null? (list 8))',
        '           0',
        '           (+ 1 (len (cdr (list 8))))))',
        '--> (+ 1',
        '       (if #f',
        '           0',
        '           (+ 1 (len (cdr (list 8))))))',
        '--> (+ 1 (+ 1 (len (cdr (list 8)))))',
        '--> (+ 1 (+ 1 (len null)))',
        '--> (+ 1',
        '       (+ 1',
        '          (if (null? null)',
        '              0',
        '              (+ 1 (len (cdr null))))))',
        '--> (+ 1',
        '       (+ 1',
        '          (if #t',
        '              0',
        '              (+ 1 (len (cdr null))))))',
        '--> (+ 1 (+ 1 0))',
        '--> (+ 1 1)',
        '--> 2',
        '',
      ].join('\n'),
    )
    expect(result.status).toBe(0)
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

  // The `file` library (#315) suspends the fiber on every read/write, so it only
  // works under the real scheduler. This runs the actual binary against a real
  // Node filesystem end-to-end. Written to a temp directory rather than
  // fixtures/, since the program creates files as it runs.
  test('the file library round-trips through the real filesystem (#315)', () => {
    const dir = mkdtempSync(path.join(tmpdir(), 'scamper-cli-file-'))
    try {
      writeFileSync(
        path.join(dir, 'prog.scm'),
        [
          '(import file)',
          '(string->file "alpha" "a.txt")',
          '(file->string "a.txt")',
          '(lines->file (list "x" "y") "b.txt")',
          '(file->lines "b.txt")',
        ].join('\n'),
        'utf-8',
      )

      const result = runCli([path.join(dir, 'prog.scm')])

      expect(result.stderr).toBe('')
      expect(result.status).toBe(0)
      expect(result.stdout).toBe('void\n"alpha"\nvoid\n(list "x" "y")\n')
      // The writes really landed on disk, next to the program.
      expect(readFileSync(path.join(dir, 'a.txt'), 'utf-8')).toBe('alpha')
      expect(readFileSync(path.join(dir, 'b.txt'), 'utf-8')).toBe('x\ny\n')
    } finally {
      rmSync(dir, { recursive: true, force: true })
    }
  })

  // #339: --trace used to step the fiber by hand, so it could not service the
  // suspend a blocking primitive raises -- it bailed out with "Blocking
  // operations ... cannot be traced". It is an ordinary traced run now, so
  // reading a file traces like anything else.
  test('--trace traces a program that reads a file (#339)', () => {
    const dir = mkdtempSync(path.join(tmpdir(), 'scamper-cli-trace-file-'))
    try {
      writeFileSync(path.join(dir, 'greet.txt'), 'hello\n', 'utf-8')
      writeFileSync(
        path.join(dir, 'prog.scm'),
        ['(import file)', '(string-length (file->string "greet.txt"))'].join(
          '\n',
        ),
        'utf-8',
      )

      const result = runCli(['--trace', path.join(dir, 'prog.scm')])

      expect(result.stderr).not.toContain('cannot be traced')
      expect(result.stdout).toBe(
        ['(string-length (file->string "greet.txt"))', '--> 6', ''].join('\n'),
      )
      expect(result.status).toBe(0)
    } finally {
      rmSync(dir, { recursive: true, force: true })
    }
  })
  // #404: a Gradescope autograder is exactly this -- a harness run by the CLI
  // whose stdout is piped to results.json -- so nothing else may reach stdout.
  // In particular an imported file's own output is discarded, which is what
  // lets a student's stray top-level expressions be harmless.
  test('a gradescope harness prints only its results JSON', () => {
    const dir = mkdtempSync(path.join(tmpdir(), 'scamper-cli-gradescope-'))
    try {
      writeFileSync(
        path.join(dir, 'hw01.scm'),
        [
          '(define double (lambda (n) (* 2 n)))',
          '(export double)',
          '"a stray expression a student left behind"',
        ].join('\n'),
        'utf-8',
      )
      writeFileSync(
        path.join(dir, 'autograder.scm'),
        [
          '(import test)',
          '(import gradescope)',
          '(import "hw01.scm")',
          '(gradescope-test-suite',
          '  (list (test-case "double 4" equal? 8 (lambda () (double 4)))',
          '        (test-case "double 5" equal? 11 (lambda () (double 5)))))',
        ].join('\n'),
        'utf-8',
      )

      const result = runCli([path.join(dir, 'autograder.scm')])

      expect(result.status).toBe(0)
      expect(JSON.parse(result.stdout)).toEqual({
        tests: [
          {
            name: 'double 4',
            status: 'passed',
            score: 1,
            max_score: 1,
            output: 'Test "double 4"\n\u2705 Passed!',
          },
          {
            name: 'double 5',
            status: 'failed',
            score: 0,
            max_score: 1,
            output: 'Test "double 5"\n\u274c Failed! Expected 11, received 10',
          },
        ],
      })
    } finally {
      rmSync(dir, { recursive: true, force: true })
    }
  })
})

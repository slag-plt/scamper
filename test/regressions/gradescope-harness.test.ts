import { spawnSync } from 'node:child_process'
import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { describe, expect, test } from 'vitest'

// The Gradescope harness (#404). run_autograder fills one directory with the
// submission and the instructor's autograder.scm and runs the CLI over it, so
// the order it copies them in decides whose program is graded.

const repoRoot = path.resolve(import.meta.dirname, '../..')

interface Layout {
  /** The instructor's harness, as gradescope/autograder.scm would be. */
  harness: string
  /** The student's files, by name. */
  submission: Record<string, string>
}

function runHarness(layout: Layout): { results: unknown; status: number | null } {
  const dir = mkdtempSync(path.join(tmpdir(), 'scamper-gradescope-'))
  try {
    mkdirSync(path.join(dir, 'source'))
    mkdirSync(path.join(dir, 'submission'))
    writeFileSync(path.join(dir, 'source', 'autograder.scm'), layout.harness, 'utf-8')
    for (const [name, src] of Object.entries(layout.submission)) {
      writeFileSync(path.join(dir, 'submission', name), src, 'utf-8')
    }
    const result = spawnSync(path.join(repoRoot, 'gradescope', 'run_autograder'), {
      encoding: 'utf-8',
      timeout: 60000,
      env: {
        ...process.env,
        SCAMPER_DIR: repoRoot,
        SOURCE_DIR: path.join(dir, 'source'),
        SUBMISSION_DIR: path.join(dir, 'submission'),
        RESULTS_DIR: path.join(dir, 'results'),
        WORK_DIR: path.join(dir, 'work'),
        SCAMPER_TIMEOUT: '30',
      },
    })
    return {
      results: JSON.parse(
        readFileSync(path.join(dir, 'results', 'results.json'), 'utf-8'),
      ),
      status: result.status,
    }
  } finally {
    rmSync(dir, { recursive: true, force: true })
  }
}

const HARNESS = [
  '(import test)',
  '(import gradescope)',
  '(import "hw01.scm")',
  '(gradescope-test-suite',
  '  (list (test-case "double 4" equal? 8 (lambda () (double 4)))))',
].join('\n')

describe('run_autograder', () => {
  test('grades the submission against the instructor\'s harness', () => {
    const { results, status } = runHarness({
      harness: HARNESS,
      submission: { 'hw01.scm': '(define double (lambda (n) (* 2 n)))\n(export double)' },
    })
    expect(status).toBe(0)
    expect(results).toMatchObject({ tests: [{ name: 'double 4', score: 1 }] })
  })

  // A student may upload whatever files they like, autograder.scm included. If
  // theirs won, the CLI would run *their* program and its output would become
  // their results file -- a self-awarded grade.
  test('a submitted autograder.scm cannot replace the instructor\'s', () => {
    const { results } = runHarness({
      harness: HARNESS,
      submission: {
        'hw01.scm': '(define double (lambda (n) (+ n 1)))\n(export double)',
        'autograder.scm': [
          '(import gradescope)',
          '(gradescope-test-suite',
          '  (list (gradescope-test-result "all good" "passed" 100 100 "Perfect!")))',
        ].join('\n'),
      },
    })
    // The instructor's single test ran, and the wrong `double` failed it.
    expect(results).toMatchObject({ tests: [{ name: 'double 4', score: 0 }] })
  })

  test('a submission that does not compile scores zero with the error', () => {
    const { results } = runHarness({
      harness: HARNESS,
      submission: { 'hw01.scm': '(define double (lambda (n) (* 2 n)' },
    })
    expect(results).toMatchObject({ score: 0 })
    expect((results as { output: string }).output).toContain('Malformed define')
  })
})

import { afterEach, describe, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import * as Scheme from '../../src/scheme'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'
import { ScamperError } from '../../src/lpm/error'
import { NotebookDisplay } from '../../src/app/web/notebook-display'
import { captionOf, splitIntoCells } from '../../src/app/web/notebook-cells'
import { Range } from '../../src/lpm/range'
import { isPlaceable, mkDiagnostic } from '../../src/scheme/diagnostic'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/557
//
// A range says where in a file an error is, but nothing said *which* file. An
// error raised while running an imported file therefore reported an offset
// into that file while the editor was showing the main one:
//
//   Runtime error [2:22-2:31]: let: value did not match pattern (pair x y)
//
// -- coordinates that mean nothing in the program being read, whose line 2 is
// twelve characters long. The IDE placed them in it all the same, filing the
// error against whatever happened to sit there.
//
// The file now travels with the error (ScamperError.modName), stamped from the
// frame that raised it, and a consumer that cannot place a foreign range says
// the file in words instead of pointing into the wrong document.

/** Backs a run's file imports with `files`, by name. */
function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) => Promise.resolve(files[f]),
  } as unknown as ReturnType<typeof fs.getFS>)
}

afterEach(() => {
  vi.restoreAllMocks()
})

// One definition per throw site named in the issue: LetHandler, MatchHandler
// and applyFn (here the error `car` raises). Each is on its own line of a file
// the main program never shows, and every one of those lines is past the end
// of the main program below.
const MODULE = `(define-export first-of
  (lambda (p) (let ([(pair x y) p]) x)))
(define-export label
  (lambda (n) (match n [0 "zero"])))
(define-export head
  (lambda (v) (car v)))`

const MAIN = `(import "m.scm")
(first-of 5)
(label 5)
(head 5)`

describe('an error in an imported file says which file (#557)', () => {
  test('every throw site names the file its range is an offset into', async () => {
    mockFS({ 'm.scm': MODULE })
    expect(await runProgram(MAIN)).toEqual([
      'Runtime error [m.scm 2:22-2:31]: let: value did not match pattern (pair x y)',
      'Runtime error [m.scm 4:15-4:34]: Inexhaustive pattern match failure',
      'Runtime error [m.scm 6:15-6:21]: (error) expected pair or nonempty-list as the first argument, received number',
    ])
  })

  // The other half of the rule: the file is named only when it is *not* the
  // one being run, so the common case reads exactly as it always has.
  test('the same mistakes in the main program name no file', async () => {
    mockFS({})
    expect(
      await runProgram(
        '(let ([(pair x y) 5]) x)\n(match 5 [0 "zero"])\n(car 5)',
      ),
    ).toEqual([
      'Runtime error [1:8-1:17]: let: value did not match pattern (pair x y)',
      'Runtime error [2:1-2:20]: Inexhaustive pattern match failure',
      'Runtime error [3:1-3:7]: (error) expected pair or nonempty-list as the first argument, received number',
    ])
  })

  // A mistake at the *call* is the caller's, even when the procedure called
  // lives in another file: the range is the main program's, so no file is
  // named. Over-stamping here would send a student to the wrong file.
  test("a bad call to an imported procedure stays the caller's", async () => {
    mockFS({ 'm.scm': MODULE })
    expect(await runProgram('(import "m.scm")\n(head 1 2)')).toEqual([
      'Runtime error [2:1-2:10]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  // The file travels with the closure, not with the call: a lambda the student
  // wrote in the main program still reports the main program, however deep in
  // another file it is applied.
  test('a lambda from the main program keeps the main program', async () => {
    mockFS({ 'm.scm': '(define-export twice\n  (lambda (f x) (f (f x))))' })
    expect(
      await runProgram('(import "m.scm")\n(twice (lambda (n) (car n)) 5)'),
    ).toEqual([
      'Runtime error [2:20-2:26]: (error) expected pair or nonempty-list as the first argument, received number',
    ])
  })

  // Each file names itself, however far down the import graph it is: a module
  // runs on its own fiber, which carries the name it was loaded under.
  test('a file imported by an imported file names itself', async () => {
    mockFS({
      'a.scm': '(import "b.scm")\n(deep 5)',
      'b.scm': '(define-export deep\n  (lambda (v) (car v)))',
    })
    expect(await runProgram('(import "a.scm")\n"done"')).toEqual([
      'Runtime error [b.scm 2:15-2:21]: (error) expected pair or nonempty-list as the first argument, received number',
      '"done"',
    ])
  })

  // A module that does not compile is reported the same way: its diagnostics
  // are offsets into it, not into the program being run.
  test("a module's own parse errors name the module", async () => {
    mockFS({ 'm.scm': '(define x\n   (1 2' })
    expect(await runProgram('(import "m.scm")\n"after"')).toEqual([
      'Parser error [m.scm 1:1-2:7]: Malformed define statement (a name and a value).',
      '"after"',
    ])
  })
})

// The rule both of the IDE's placement consumers share: the editor's
// diagnostics and the notebook's cells alike may point at a range only when it
// is an offset into the document they are showing.
describe('a range that belongs to another file cannot be placed (#557)', () => {
  const range = Range.of(1, 1, 0, 1, 4, 3)

  test('a diagnostic about this file is placeable', () => {
    expect(isPlaceable(mkDiagnostic('Scope', 'error', 'boom', range))).toBe(true)
  })

  test('the same diagnostic about another file is not', () => {
    expect(
      isPlaceable(mkDiagnostic('Scope', 'error', 'boom', range, 'm.scm')),
    ).toBe(false)
  })
})

/**
 * Runs `src` as the notebook does -- split into cells, each run's output filed
 * under the cell that produced it -- and reports where each error landed.
 */
async function runNotebook(src: string): Promise<{
  cells: string[][]
  unplaced: string[]
}> {
  const cells = splitIntoCells(src)
  if (cells === null) throw new Error(`${src} does not parse`)
  const display = new NotebookDisplay()
  display.setSlots(
    cells.map((cell) => ({
      from: cell.from,
      to: cell.to,
      caption: cell.kind === 'code' ? captionOf(cell, src) : '',
    })),
  )
  const { prog } = await Scheme.compile(src)
  if (prog === undefined) throw new Error(`${src} does not compile`)
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out: display,
    err: display,
    src,
  })
  // An error as the file it names, so a misplaced one is visible as such.
  const shown = (v: unknown): string =>
    v instanceof ScamperError ? `${String(v.modName)}: ${v.message}` : String(v)
  return {
    cells: cells.map((_, i) => display.outputOf(i).map(shown)),
    unplaced: display.unplaced.map(shown),
  }
}

describe('the IDE does not place a foreign range in the open file (#557)', () => {
  test('an error from an imported file is not filed under a cell of this one', async () => {
    // `(car 5)` sits at offsets 19-25 of m.scm, which in the main program
    // below is inside the second cell -- a display that has nothing to do with
    // it. It belongs above the notebook instead, named by its file.
    mockFS({ 'm.scm': '(define-export v\n  (car 5))' })
    const { cells, unplaced } = await runNotebook(
      '(import "m.scm")\n\n(display "hello")\n\n(display "world")',
    )
    expect(cells).toEqual([[], ['hello'], ['world']])
    expect(unplaced).toEqual([
      'm.scm: expected pair or nonempty-list as the first argument, received number',
    ])
  })
})

import { describe, expect, test } from 'vitest'
import * as Scheme from '../../../src/scheme'
import { Fiber } from '../../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../../src/lpm/run'
import { ScamperError } from '../../../src/lpm/error'
import { Range } from '../../../src/lpm/range'
import { diagnosticToError } from '../../../src/scheme/diagnostic'
import type { Value } from '../../../src/lpm'
import { NotebookDisplay } from '../../../src/app/web/notebook-display'
import {
  captionOf,
  splitIntoCells,
  type Cell,
} from '../../../src/app/web/notebook-cells'

/** A value as a test can compare it. */
function shown(v: Value): string {
  return v instanceof ScamperError ? `error: ${v.message}` : String(v)
}

/**
 * Splits `src` into cells, runs it, and returns what each cell produced.
 *
 * The same order of events the IDE's own run has -- compile, report whatever
 * that found, then run -- so what a student would see is what is asserted.
 */
async function runNotebook(
  src: string,
): Promise<{ cells: Cell[]; output: string[][]; unplaced: string[] }> {
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
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => {
    display.report(diagnosticToError(d))
  })
  if (prog !== undefined) {
    await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
      out: display,
      err: display,
      src,
    })
  }
  return {
    cells,
    output: cells.map((_, i) => display.outputOf(i).map(shown)),
    unplaced: display.unplaced.map(shown),
  }
}

describe('a run fills in the notebook', () => {
  test('each cell keeps its own output', async () => {
    const { output } = await runNotebook('(display 1)\n\n(display 2)')
    expect(output).toEqual([['1'], ['2']])
  })

  test('a cell that prints nothing has nothing under it', async () => {
    const { output } = await runNotebook(
      '(define x 5)\n\n(display x)\n\n(define y 6)',
    )
    expect(output).toEqual([[], ['5'], []])
  })

  test('a bare expression shows its value under itself', async () => {
    const { output } = await runNotebook('(+ 1 2)\n\n(* 3 4)')
    expect(output).toEqual([['3'], ['12']])
  })

  test('prose cells take no output', async () => {
    const { cells, output } = await runNotebook(
      '; First we count.\n\n(display 1)\n\n; Then again.\n\n(display 2)',
    )
    expect(cells.map((c) => c.kind)).toEqual(['prose', 'code', 'prose', 'code'])
    expect(output).toEqual([[], ['1'], [], ['2']])
  })

  test('a documented function keeps its output with its cell', async () => {
    const src =
      ';;; (sq x) -> number?\n;;; x: number?\n;;; Squares x.\n(define sq (lambda (x) (* x x)))\n\n(display (sq 4))'
    const { output } = await runNotebook(src)
    expect(output).toEqual([[], ['16']])
  })

  // A struct expands to a define per field, all carrying the range of the form
  // the student wrote; they are one cell and one caption.
  test('a form that expands to several statements is one cell', async () => {
    const { cells, output } = await runNotebook(
      '(struct point (x y))\n\n(display (point-x (point 1 2)))',
    )
    expect(cells).toHaveLength(2)
    expect(output).toEqual([[], ['1']])
  })

  test('a runtime error lands in the cell that raised it', async () => {
    const { output } = await runNotebook(
      '(display 1)\n\n(display (/ 1 0))\n\n(display 3)',
    )
    expect(output[0]).toEqual(['1'])
    expect(output[1][0]).toMatch(/^error: /)
    expect(output[2]).toEqual(['3'])
  })

  test('a later cell still runs after an earlier one fails', async () => {
    const { output } = await runNotebook('(car null)\n\n(display 2)')
    expect(output[0][0]).toMatch(/^error: /)
    expect(output[1]).toEqual(['2'])
  })
})

describe('output with no cell to go to', () => {
  test('a form the split does not know about does not shift the rest', () => {
    const display = new NotebookDisplay()
    display.setSlots([
      { from: 0, to: 12, caption: '(display 1)' },
      { from: 14, to: 26, caption: '(display 2)' },
    ])
    // A form nobody knows: its output goes to the first cell that has one, and
    // the cell that follows still gets its own.
    display.beginStatement('(display 99)')
    display.send('99')
    display.beginStatement('(display 2)')
    display.send('2')
    expect(display.outputOf(0)).toEqual(['99'])
    expect(display.outputOf(1)).toEqual(['2'])
  })

  test('everything between two forms belongs to the first', () => {
    const display = new NotebookDisplay()
    display.setSlots([
      { from: 0, to: 12, caption: '(display 1)' },
      { from: 14, to: 26, caption: '(display 2)' },
    ])
    display.beginStatement('(display 1)')
    display.send('a')
    display.send('b')
    display.beginStatement('(display 2)')
    display.send('c')
    expect(display.outputOf(0)).toEqual(['a', 'b'])
    expect(display.outputOf(1)).toEqual(['c'])
    expect(display.totalSends).toBe(3)
  })

  // Reported before anything ran, so what says which cell it is about is its
  // range: a file that would not compile still shows its error where it is.
  test('an error before the run lands in the cell it points at', () => {
    const display = new NotebookDisplay()
    display.setSlots([
      { from: 0, to: 12, caption: '(display 1)' },
      { from: 14, to: 26, caption: '(display 2)' },
    ])
    const e = new ScamperError(
      'Parser',
      'something is wrong here',
      undefined,
      Range.of(3, 1, 16, 3, 5, 20),
    )
    display.report(e)
    expect(display.outputOf(1)).toEqual([e])
    expect(display.unplaced).toEqual([])
  })

  test('an error with no range is shown above the notebook', () => {
    const display = new NotebookDisplay()
    display.setSlots([{ from: 0, to: 12, caption: '(display 1)' }])
    const e = new ScamperError('Parser', 'nothing to point at')
    display.report(e)
    expect(display.unplaced).toEqual([e])
    expect(display.outputOf(0)).toEqual([])
  })

  test('resetting empties the cells but keeps them', () => {
    const display = new NotebookDisplay()
    display.setSlots([{ from: 0, to: 12, caption: '(display 1)' }])
    display.beginStatement('(display 1)')
    display.send('1')
    display.reset()
    expect(display.outputOf(0)).toEqual([])
    expect(display.totalSends).toBe(0)
  })
})

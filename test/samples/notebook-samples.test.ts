import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { splitIntoCells, type Cell } from '../../src/app/web/notebook-cells'

// The samples in samples/ are the closest thing in the repository to a file a
// student would actually write: a couple of hundred lines with headers,
// docstrings, sections and comments beside code. The notebook has to be able to
// show one of them without losing any of it (#410).
//
// The bar here is not what the cells *are* -- that is notebook-cells.test.ts,
// case by case -- but that between them they still hold the whole file.

const SAMPLES = path.resolve(import.meta.dirname, '../../samples')

function sample(name: string): string {
  return readFileSync(path.join(SAMPLES, name), 'utf-8')
}

/** The cells of `src`, or a failure if it did not split. */
function cellsOf(src: string): Cell[] {
  const cells = splitIntoCells(src)
  expect(cells).not.toBeNull()
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return cells!
}

describe.each(['showcase.scm', 'libs.scm'])('%s as a notebook', (name) => {
  test('splits into cells', () => {
    expect(cellsOf(sample(name)).length).toBeGreaterThan(0)
  })

  test('has one code cell per top-level form', () => {
    const src = sample(name)
    const { program } = tokenizeAndParse(src)
    const code = cellsOf(src).filter((cell) => cell.kind === 'code')
    expect(code).toHaveLength(program?.length ?? -1)
  })

  // The whole point of a view: everything in the file is somewhere in it, and
  // nothing is in it twice.
  test('loses nothing but whitespace', () => {
    const src = sample(name)
    const cells = cellsOf(src)
    let at = 0
    for (const cell of cells) {
      expect(cell.from).toBeGreaterThanOrEqual(at)
      // What falls between two cells belongs to nobody, and must be spacing.
      expect(src.slice(at, cell.from).trim()).toBe('')
      expect(cell.text).toBe(src.slice(cell.from, cell.to))
      at = cell.to
    }
    expect(src.slice(at).trim()).toBe('')
  })

  test('never separates a docstring from what it documents', () => {
    const src = sample(name)
    const { program } = tokenizeAndParse(src)
    const cells = cellsOf(src)
    for (const stmt of program ?? []) {
      const docLines = ('docComments' in stmt ? (stmt.docComments ?? []) : [])
        .filter((c) => c.line.trimStart().startsWith(';;;'))
      if (docLines.length === 0) continue
      const cell = cells.find(
        (c) => c.kind === 'code' && c.stmtFrom === stmt.range.begin.idx,
      )
      expect(cell).toBeDefined()
      for (const comment of docLines) {
        expect(comment.range.begin.idx).toBeGreaterThanOrEqual(cell?.from ?? -1)
        expect(comment.range.end.idx).toBeLessThan(cell?.to ?? -1)
      }
    }
  })
})

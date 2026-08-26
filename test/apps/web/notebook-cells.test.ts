import { describe, expect, test } from 'vitest'
import {
  captionOf,
  markdownToProse,
  proseToMarkdown,
  shiftCells,
  splitIntoCells,
  type Cell,
  type CodeCell,
} from '../../../src/app/web/notebook-cells'
import { tokenizeAndParse } from '../../../src/scheme'

/** The cells of `src`, or a failure if it did not parse. */
function split(src: string): Cell[] {
  const cells = splitIntoCells(src)
  expect(cells).not.toBeNull()
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return cells!
}

/** What each cell is and holds, which is what the notebook shows. */
function shape(src: string): [string, string][] {
  return split(src).map((c) => [c.kind, c.text])
}

describe('splitting a file into cells', () => {
  test('one form is one cell', () => {
    expect(shape('(+ 1 2)')).toEqual([['code', '(+ 1 2)']])
  })

  test('each top-level form is its own cell', () => {
    expect(shape('(define x 5)\n\n(+ x 1)')).toEqual([
      ['code', '(define x 5)'],
      ['code', '(+ x 1)'],
    ])
  })

  test('a form spanning several lines stays one cell', () => {
    const src = '(define f\n  (lambda (x)\n    (* x x)))'
    expect(shape(src)).toEqual([['code', src]])
  })

  test('an ordinary comment between forms is prose', () => {
    expect(shape('(define x 5)\n\n; Now we use it.\n\n(+ x 1)')).toEqual([
      ['code', '(define x 5)'],
      ['prose', '; Now we use it.'],
      ['code', '(+ x 1)'],
    ])
  })

  test('a paragraph of comments is one prose cell', () => {
    expect(shape('; One.\n; Two.\n\n(+ 1 2)')).toEqual([
      ['prose', '; One.\n; Two.'],
      ['code', '(+ 1 2)'],
    ])
  })

  test('a blank line starts a new prose cell', () => {
    expect(shape('; One.\n\n; Two.\n\n(+ 1 2)')).toEqual([
      ['prose', '; One.'],
      ['prose', '; Two.'],
      ['code', '(+ 1 2)'],
    ])
  })

  // A docstring is what the docs, the contracts and the @example checks read,
  // so it belongs to the function rather than to the prose around it.
  test('a docstring stays with the form it documents', () => {
    const src = ';;; (f x) -> number?\n;;; x: number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'
    expect(shape(src)).toEqual([['code', src]])
  })

  // The same rule the compiler follows (#413): a run of comments belongs to the
  // form directly below it, and a blank line is how you set one apart.
  test('a docstring set apart by a blank line is a header, and reads as prose', () => {
    const src = ';;; A lab about squares.\n\n(define x 5)'
    expect(shape(src)).toEqual([
      ['prose', ';;; A lab about squares.'],
      ['code', '(define x 5)'],
    ])
  })

  test('a blank line inside a docstring ends it', () => {
    const src = ';;; A header.\n\n;;; (f x) -> number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'
    expect(shape(src)).toEqual([
      ['prose', ';;; A header.'],
      ['code', ';;; (f x) -> number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'],
    ])
  })

  // A contiguous block is one block, as it is for the compiler: an ordinary
  // comment line among the `;;;` ones does not split the docstring in half.
  test('a comment written into a docstring stays with it', () => {
    const src = ';;; (f x) -> number?\n; still working on this\n;;; Squares x.\n(define f (lambda (x) (* x x)))'
    expect(shape(src)).toEqual([['code', src]])
  })

  test('prose set apart from a docstring is prose', () => {
    const src = '; Here is a function.\n\n;;; (f x) -> number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'
    expect(shape(src)).toEqual([
      ['prose', '; Here is a function.'],
      ['code', ';;; (f x) -> number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'],
    ])
  })

  test('a comment beside a form is part of that form', () => {
    expect(shape('(define x 5) ; five\n\n(+ x 1)')).toEqual([
      ['code', '(define x 5) ; five'],
      ['code', '(+ x 1)'],
    ])
  })

  test('a comment inside a form is part of that form', () => {
    const src = '(define x\n  ; the answer\n  42)'
    expect(shape(src)).toEqual([['code', src]])
  })

  test('comments below the last form are prose', () => {
    expect(shape('(+ 1 2)\n\n; That is all.')).toEqual([
      ['code', '(+ 1 2)'],
      ['prose', '; That is all.'],
    ])
  })

  test('a file of nothing but comments is all prose', () => {
    expect(shape('; A note.\n\n; Another.')).toEqual([
      ['prose', '; A note.'],
      ['prose', '; Another.'],
    ])
  })

  test('an empty file has no cells', () => {
    expect(split('')).toEqual([])
    expect(split('\n\n')).toEqual([])
  })

  // The notebook keeps the split it has while a cell is half-typed, rather
  // than taking every cell below it away until the parentheses balance again.
  test('a file that does not parse has no split', () => {
    expect(splitIntoCells('(define f (lambda (x)')).toBeNull()
  })

  test('a cell owns exactly its own text', () => {
    const src = '(define x 5)\n\n\n; A note.\n\n(+ x 1)'
    for (const cell of split(src)) {
      expect(src.slice(cell.from, cell.to)).toBe(cell.text)
    }
  })

  // The notebook decides for itself which comments belong to a form, and the
  // compiler decides which lines are its docstring. If those two rules drift
  // apart a student is shown a docstring in one cell that documents the form in
  // another -- so a form's cell must always cover its `;;;` lines.
  //
  // Only those lines. An ordinary comment the compiler happens to collect along
  // with them contributes nothing to the docstring (the parser drops every line
  // that is not `;;;`), and reading better as prose is the whole point here.
  test('a code cell holds every docstring line the compiler reads', () => {
    const sources = [
      ';;; Doc.\n(define x 5)',
      ';;; Header.\n\n(define x 5)',
      '; note\n;;; Doc.\n(define x 5)',
      '(define a 1) ; note\n(define b 2)',
      ';;; One.\n(define a 1)\n\n;;; Two.\n(define b 2)',
      '; a paragraph\n\n;;; Doc.\n(define x 5)',
      ';;; Doc.\n;; an aside\n;;; More doc.\n(define x 5)',
    ]
    for (const src of sources) {
      const { program } = tokenizeAndParse(src)
      expect(program).toBeDefined()
      const cells = split(src)
      for (const stmt of program ?? []) {
        const attached = 'docComments' in stmt ? (stmt.docComments ?? []) : []
        const docLines = attached.filter((c) =>
          c.line.trimStart().startsWith(';;;'),
        )
        if (docLines.length === 0) continue
        const cell = cells.find(
          (c): c is CodeCell =>
            c.kind === 'code' && c.stmtFrom === stmt.range.begin.idx,
        )
        expect(cell, `no cell for a form in ${JSON.stringify(src)}`).toBeDefined()
        for (const comment of docLines) {
          expect(
            comment.range.begin.idx,
            `${comment.line} in ${JSON.stringify(src)}`,
          ).toBeGreaterThanOrEqual(cell?.from ?? -1)
          expect(comment.range.end.idx).toBeLessThan(cell?.to ?? -1)
        }
      }
    }
  })

  // What the scheduler will announce for each form, which is how output finds
  // its way back to the cell that produced it.
  test('a code cell knows the caption its output will carry', () => {
    const src = ';;; Squares x.\n(define f (lambda (x) (* x x))) ; note'
    const [cell] = split(src)
    expect(captionOf(cell as CodeCell, src)).toBe('(define f (lambda (x) (* x x)))')
  })
})

describe('prose as Markdown', () => {
  test('the comment markers come off', () => {
    expect(proseToMarkdown('; # Heading\n; Some *text*.')).toBe(
      '# Heading\nSome *text*.',
    )
  })

  test('a bare marker is a blank line', () => {
    expect(proseToMarkdown('; One.\n;\n; Two.')).toBe('One.\n\nTwo.')
  })

  test('indentation and extra markers come off too', () => {
    expect(proseToMarkdown('  ;; Indented.')).toBe('Indented.')
  })

  test('writing it back restores the markers', () => {
    expect(markdownToProse('One.\n\nTwo.')).toBe('; One.\n;\n; Two.')
  })

  // A text cell opened and then left alone should cost the file nothing.
  test('nothing written is nothing to write', () => {
    expect(markdownToProse('')).toBe('')
    expect(markdownToProse('\n\n')).toBe('')
  })

  test('markers survive the round trip', () => {
    const prose = '; One.\n;\n; Two.'
    expect(markdownToProse(proseToMarkdown(prose))).toBe(prose)
  })
})

describe('shifting cells past an edit', () => {
  const src = '(define x 5)\n\n(+ x 1)'

  test('a cell below an edit moves by what the edit added', () => {
    const edited = '(define xyz 5)\n\n(+ x 1)'
    const cells = shiftCells(split(src), 8, 9, 3, edited)
    expect(cells).not.toBeNull()
    expect(cells?.map((c) => c.text)).toEqual(['(define xyz 5)', '(+ x 1)'])
  })

  test('a code cell keeps its caption in step', () => {
    const edited = '(define xyz 5)\n\n(+ x 1)'
    const cells = shiftCells(split(src), 8, 9, 3, edited)
    const cell = cells?.[0] as CodeCell
    expect(captionOf(cell, edited)).toBe('(define xyz 5)')
  })

  // An empty cell is a start and an end at the same place, so which way that
  // position moves decides whether the cell grows around what was typed into
  // it or the text lands outside it.
  test('an empty cell grows around what is typed into it', () => {
    const empty: Cell[] = [
      { kind: 'code', from: 0, to: 0, text: '', stmtFrom: 0, stmtTo: 0 },
    ]
    const cells = shiftCells(empty, 0, 0, 7, '(+ 1 2)')
    expect(cells?.[0].from).toBe(0)
    expect(cells?.[0].to).toBe(7)
    expect(cells?.[0].text).toBe('(+ 1 2)')
  })

  test('a cell grows when something is typed at its end', () => {
    const cells = shiftCells(split(src), 12, 12, 2, '(define x 5)!!\n\n(+ x 1)')
    expect(cells?.[0].text).toBe('(define x 5)!!')
    expect(cells?.[1].text).toBe('(+ x 1)')
  })

  // A paste across several cells, or an undo: only a re-split can say what the
  // notebook now holds.
  test('an edit crossing a cell boundary cannot be shifted', () => {
    expect(shiftCells(split(src), 5, 16, 0, '(defi 1)')).toBeNull()
  })
})

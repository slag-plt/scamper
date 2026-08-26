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

  test('prose above a docstring is still prose', () => {
    const src = '; Here is a function.\n;;; (f x) -> number?\n;;; Squares x.\n(define f (lambda (x) (* x x)))'
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

  // A paste across several cells, or an undo: only a re-split can say what the
  // notebook now holds.
  test('an edit crossing a cell boundary cannot be shifted', () => {
    expect(shiftCells(split(src), 5, 16, 0, '(defi 1)')).toBeNull()
  })
})

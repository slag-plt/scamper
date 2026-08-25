import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { layoutToString, stmtToLayout, type Prog } from '../../src/scheme/ast'
import { PRINT_WIDTH } from '../../src/scheme/style'

// Line breaking for the surface syntax (see FORMATTING.md). The editor's
// indenter and this printer read the same rule table, so the shapes asserted
// here are the shapes the output and step panes draw.
//
// Most cases below name a narrow width rather than padding the source out to
// eighty columns: the rule under test is the same either way, and short input
// makes the expected shape readable.

function parse(src: string): Prog {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) throw new Error(diagnostics.map((d) => d.message).join('; '))
  return program
}

/** Lay `src` out at `width` columns, the way a pane does. */
function fmt(src: string, width = PRINT_WIDTH): string {
  return parse(src)
    .map((s) => layoutToString(stmtToLayout(s), width))
    .join('\n')
}

describe('what fits is left alone', () => {
  test.each([
    '(define x 42)',
    '(lambda (x y) (+ x y))',
    '(if (> x 0) x (- 0 x))',
    '(let ([a 1] [b 2]) (+ a b))',
    '(cond [(< x 0) -1] [else 1])',
    '(match l [null 0] [(cons x xs) 1])',
    '(f a b c)',
    '[1 2 3]',
    '{"a" 1 "b" 2}',
  ])('%s', (src) => {
    expect(fmt(src)).toBe(src)
  })

  test('the default width is eighty columns', () => {
    const wide = `(f ${'a'.repeat(40)} ${'b'.repeat(40)})`
    expect(wide.length).toBeGreaterThan(PRINT_WIDTH)
    expect(fmt(wide)).toContain('\n')
  })
})

describe('breaking past the width', () => {
  test('rule 1: a lambda keeps its parameters, body indented two', () => {
    expect(fmt('(lambda (x y) (+ x y z w))', 20)).toBe(
      '(lambda (x y)\n  (+ x y z w))',
    )
  })

  test('rule 2: a define keeps its name, body indented two', () => {
    expect(fmt('(define x (f a b c))', 15)).toBe('(define x\n  (f a b c))')
  })

  test('rule 3: if branches align under the test, at column four', () => {
    expect(fmt('(if (> x 0) x (- 0 x))', 15)).toBe(
      '(if (> x 0)\n    x\n    (- 0 x))',
    )
  })

  test('rule 4: let bindings align at six, body at two', () => {
    expect(fmt('(let ([a 1] [b 2]) (+ a b))', 16)).toBe(
      '(let ([a 1]\n      [b 2])\n  (+ a b))',
    )
  })

  test('rule 4: a binding list that still fits stays on the let line', () => {
    expect(fmt('(let ([a 1] [b 2]) (+ a b))', 20)).toBe(
      '(let ([a 1] [b 2])\n  (+ a b))',
    )
  })

  test('rule 5: cond clauses sit at two', () => {
    expect(fmt('(cond [(< x 0) -1] [else 1])', 20)).toBe(
      '(cond\n  [(< x 0) -1]\n  [else 1])',
    )
  })

  test('rule 5: a clause too wide to fit splits, consequent at three', () => {
    expect(fmt('(cond [(< x 0) (neg x)])', 14)).toBe(
      '(cond\n  [(< x 0)\n   (neg x)])',
    )
  })

  test('rule 6: match keeps its scrutinee, clauses at two', () => {
    expect(fmt('(match l [null 0] [(cons x xs) 1])', 20)).toBe(
      '(match l\n  [null 0]\n  [(cons x xs) 1])',
    )
  })

  test('rule 7: arguments align under the first argument', () => {
    expect(fmt('(some-fn a b c)', 10)).toBe(
      '(some-fn a\n         b\n         c)',
    )
  })

  test('rule 7: all arguments break together, never packed greedily', () =>
    // "a b" would fit on the opening line; the rule says it may not.
    {
      expect(fmt('(fn a b ccccccccccc)', 16)).toBe(
        '(fn a\n    b\n    ccccccccccc)',
      )
    })

  test('begin indents its body rather than aligning it', () => {
    expect(fmt('(begin (f 1) (g 2))', 15)).toBe('(begin\n  (f 1)\n  (g 2))')
  })

  test('and takes the default rule and aligns', () => {
    expect(fmt('(and (p x) (q x))', 12)).toBe('(and (p x)\n     (q x))')
  })

  test('a vector aligns under its first element', () => {
    expect(fmt('[1 2 3]', 5)).toBe('[1\n 2\n 3]')
  })

  test('a map literal aligns under its first key', () => {
    expect(fmt('{"a" 1 "b" 2}', 9)).toBe('{"a" 1\n "b" 2}')
  })

  // A pair is one `unit` in the layout, so the break can only fall between
  // pairs -- a line never ends with a key whose value is on the next one.
  test('a map that must break never splits a key from its value', () => {
    const out = fmt('{"a" 1 "b" 2 "c" 3}', 8)
    expect(out).toBe('{"a" 1\n "b" 2\n "c" 3}')
    for (const line of out.split('\n')) expect(line).not.toMatch(/"\s*$/)
  })

  test('struct has no entry in the table, so it aligns', () => {
    expect(fmt('(struct s (x y z))', 17)).toBe('(struct s\n        (x y z))')
  })

  // The "#" is a column of its own, so the body aligns one past where the same
  // application would have without it.
  test('an anonymous function aligns past its hash', () => {
    expect(fmt('#(+ % 1)', 7)).toBe('#(+ %\n    1)')
    // the same shape without the hash lines up one column to the left
    expect(fmt('(+ a 1)', 6)).toBe('(+ a\n   1)')
  })
})

describe('a form is measured where it sits', () => {
  test('the same expression breaks nested and not at top level', () => {
    expect(fmt('(cons a (cons b null))', 22)).toBe('(cons a (cons b null))')
    // Indented two inside the define, the very same call no longer fits.
    expect(fmt('(define xs (cons a (cons b null)))', 22)).toBe(
      '(define xs\n  (cons a\n        (cons b\n              null)))',
    )
  })

  test('nesting composes down several levels', () => {
    expect(
      fmt('(define go (lambda (n) (cond [(zero? n) 1] [else (* n a b)])))', 24),
    ).toBe(
      '(define go\n' +
        '  (lambda (n)\n' +
        '    (cond\n' +
        '      [(zero? n) 1]\n' +
        '      [else\n       (* n a b)])))',
    )
  })
})

describe('lines stay inside the width', () => {
  // A form with a single argument has nowhere to break -- rule 7 keeps a head
  // and its first argument together -- so a long enough atom will always
  // overflow a narrow enough pane. These widths leave room for that.
  test.each([32, 48, 60, PRINT_WIDTH])('at width %i', (width) => {
    const src =
      '(define classify (lambda (n) (cond [(< n 0) (negative-branch n)] ' +
      '[(= n 0) zero] [else (positive-branch n)])))'
    for (const line of fmt(src, width).split('\n')) {
      expect(line.length).toBeLessThanOrEqual(width)
    }
  })
})

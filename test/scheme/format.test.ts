import { describe, expect, test } from 'vitest'
import * as A from '../../src/scheme/ast'
import { tokenizeAndParse } from '../../src/scheme'
import { formatSource } from '../../src/scheme/format'

// ---- helpers ----------------------------------------------------------------

function parse(src: string): A.Prog {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) {
    throw new Error(
      `Parse failed:\n${diagnostics.map((d) => d.message).join('\n')}`,
    )
  }
  return program
}

function format(src: string, width?: number): string {
  return formatSource(src, width)
}

function progEquals(p1: A.Prog, p2: A.Prog): boolean {
  return (
    p1.length === p2.length && p1.every((stmt, i) => A.stmtEquals(stmt, p2[i]))
  )
}

// ---- roundtrip: parse(src) ≡ parse(format(src)) ----------------------------

describe('roundtrip', () => {
  function roundtrip(src: string): void {
    const original = parse(src)
    const reformatted = parse(format(src))
    expect(progEquals(original, reformatted)).toBe(true)
  }

  test('literal define', () => {
    roundtrip('(define x 42)')
  })

  test('boolean define', () => {
    roundtrip('(define flag #t)')
  })

  test('lambda with multiple params', () => {
    roundtrip('(define f (lambda (x y) (+ x y)))')
  })

  test('lambda with a rest parameter (#272)', () => {
    roundtrip('(define f (lambda (x & xs) xs))')
  })

  test('lambda with a rest-only parameter list (#272)', () => {
    roundtrip('(define g (lambda (& xs) xs))')
  })

  test('rest parameters are printed with "&" (#272)', () => {
    expect(format('(define f (lambda (x & xs) xs))')).toContain('(x & xs)')
    expect(format('(define g (lambda (& xs) xs))')).toContain('(& xs)')
  })

  test('if expression', () => {
    roundtrip('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))')
  })

  test('let binding', () => {
    roundtrip('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))')
  })

  test('match expression', () => {
    roundtrip('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))')
  })

  test('match with constructor patterns', () => {
    roundtrip('(define head (lambda (l) (match l [(cons h _) h])))')
  })

  test('and / or', () => {
    roundtrip('(define both (lambda (a b) (and (> a 0) (< b 10))))')
  })

  test('cond expression', () => {
    roundtrip(
      '(define sign (lambda (n) (cond [(> n 0) 1] [(< n 0) -1] [#t 0])))',
    )
  })

  test('anonymous function #(...)', () => {
    roundtrip('(define inc #(+ %1 1))')
  })

  test('vector literal', () => {
    roundtrip('(define v [1 (+ 1 2) "x"])')
  })

  test('empty vector literal', () => {
    roundtrip('(define v [])')
  })

  test('map literal', () => {
    roundtrip('(define m {"a" 1 "b" (+ 1 2)})')
  })

  test('empty map literal', () => {
    roundtrip('(define m {})')
  })

  test('nested vector and map literals', () => {
    roundtrip('(define m {"a" {"b" [1 2]} "c" [[3] [4]]})')
  })

  test('vector pattern', () => {
    roundtrip('(define f (lambda (v) (match v [[1 x] x] [_ 0])))')
  })

  test('literals keep their bracket/brace spelling', () => {
    expect(format('(define v [1 2])')).toContain('[1 2]')
    expect(format('(define m {"a" 1})')).toContain('{"a" 1}')
    expect(format('(define v [])')).toContain('[]')
    expect(format('(define m {})')).toContain('{}')
  })

  test('a map that must break splits between pairs, never inside one', () => {
    const out = format(
      '(define m {"alpha" 1 "beta" 2 "gamma" 3 "delta" 4 "epsilon" 5 "zeta" 6})',
      40,
    )
    for (const line of out.trimEnd().split('\n')) {
      // Every wrapped line holds a whole "key value" pair, so no line ends with
      // a dangling key.
      expect(line).not.toMatch(/"\s*$/)
    }
    expect(parse(out)).toBeTruthy()
  })

  test('anonymous function with a rest parameter', () => {
    roundtrip('(define total #(apply + %&))')
  })

  test('anonymous function nested in an application', () => {
    roundtrip('(display (map #(* %1 %1) (list 1 2 3)))')
  })

  test('anonymous function with a special-form body', () => {
    roundtrip('(define f #(if (> % 0) % (- 0 %)))')
  })

  test('anonymous function with a let body', () => {
    roundtrip('(define g #(let ([d (* %1 2)]) (+ d %2)))')
  })

  test('empty anonymous function', () => {
    roundtrip('(define k #())')
  })

  test('anonymous function prints with the "#(" delimiter', () => {
    expect(format('(define inc #(+ %1 1))')).toContain('#(+ %1 1)')
  })

  test('nullary application', () => {
    roundtrip('(define zero (lambda () 0))')
  })

  test('import statement', () => {
    roundtrip('(import image)')
  })

  test('qualified import statement', () => {
    roundtrip('(import image img)')
  })

  test('qualified file import statement', () => {
    roundtrip('(import "utils.scm" u)')
  })

  test('struct definition', () => {
    roundtrip('(struct point (x y))')
  })

  test('export statement', () => {
    roundtrip('(export a b c)')
  })

  test('empty export statement', () => {
    roundtrip('(export)')
  })

  test('define-export statement', () => {
    roundtrip('(define-export x 5)')
  })

  test('multi-statement program', () => {
    roundtrip('(define x 1)\n(define y 2)\n(display (+ x y))')
  })

  test('normalizes extra whitespace', () => {
    roundtrip('(define   x      42)')
  })

  test('normalizes nested whitespace', () => {
    roundtrip('(define f   (lambda   (x   y)   (+   x   y)))')
  })
})

// ---- idempotence: format(src) === format(format(src)) -----------------------

describe('idempotence', () => {
  function idempotent(src: string): void {
    const once = format(src)
    const twice = format(once)
    expect(once).toBe(twice)
  }

  test('literal define', () => {
    idempotent('(define x 42)')
  })

  test('lambda', () => {
    idempotent('(define f (lambda (x y) (+ x y)))')
  })

  test('if expression', () => {
    idempotent('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))')
  })

  test('let binding', () => {
    idempotent('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))')
  })

  test('match expression', () => {
    idempotent('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))')
  })

  test('multi-statement program', () => {
    idempotent('(define x 1)\n(define y 2)\n(display (+ x y))')
  })
})

// ---- blank lines between statements ----------------------------------------

describe('blank lines between statements', () => {
  test('a run of one-liners the author packed stays packed', () => {
    const src = '(import image)\n(import music)\n(define radius 10)'
    expect(format(src)).toBe(src)
  })

  test('a blank line the author wrote survives', () => {
    expect(format('(define a 1)\n\n(define b 2)')).toBe(
      '(define a 1)\n\n(define b 2)',
    )
  })

  test('a wider gap collapses to one', () => {
    expect(format('(define a 1)\n\n\n\n(define b 2)')).toBe(
      '(define a 1)\n\n(define b 2)',
    )
  })

  test('a statement spread over several lines is always separated', () => {
    // Packed one-liners read as one thought; a form with a body does not. The
    // lambda breaks by rule 1 however short it is, so the define takes 2b.
    const out = format('(define a 1)\n(define f (lambda (x) x))\n(define b 2)')
    expect(out).toBe(
      '(define a 1)\n\n(define f\n  (lambda (x)\n    x))\n\n(define b 2)',
    )
  })

  test('a docstring keeps its define but gains a line above', () => {
    const out = format('(define a 1)\n;;; adds one\n(define add1 #(+ % 1))')
    expect(out).toBe('(define a 1)\n\n;;; adds one\n(define add1 #(+ % 1))')
  })

  test('a trailing comment does not break up a packed run', () => {
    const src = '(define a 1) ; note\n(define b 2)'
    expect(format(src)).toBe(src)
  })

  test('comments below the last statement stay together', () => {
    // They are one block, so a run of them is not spaced apart line by line.
    expect(format('(define a 1)\n; one\n; two')).toBe(
      '(define a 1)\n\n; one\n; two',
    )
  })

  test('a lone statement gains nothing', () => {
    expect(format('(define x 1)')).toBe('(define x 1)')
    expect(format('')).toBe('')
  })

  test('spacing is a fixed point, packed or separated', () => {
    for (const src of [
      '(import image)\n(import music)\n(define r 10)',
      '(define a 1)\n\n(define b 2)\n; note\n(define c 3)',
      '(define f (lambda (x) (if (> x 0) (big-call-name x) (other-name x))))',
    ]) {
      const once = format(src)
      expect(format(once)).toBe(once)
    }
  })
})

// ---- comments are preserved (#304) -----------------------------------------

describe('comment preservation', () => {
  test('a docstring above a define survives formatting', () => {
    const src = [
      ';;; (add1 n) -> number?',
      ';;;   n: number?',
      ';;; Adds one to n.',
      '(define add1 (lambda (n) (+ n 1)))',
    ].join('\n')
    const out = format(src)
    expect(out).toContain(';;; (add1 n) -> number?')
    expect(out).toContain(';;;   n: number?')
    expect(out).toContain(';;; Adds one to n.')
    // The comment stays immediately above the define it documents.
    expect(out.indexOf(';;; Adds one to n.')).toBeLessThan(
      out.indexOf('(define add1'),
    )
  })

  test('a standalone comment between statements is kept on its own line', () => {
    const out = format('(define a 1)\n; a note\n(define b 2)')
    // The blank line goes above the comment, which belongs to the define below.
    expect(out).toBe('(define a 1)\n\n; a note\n(define b 2)')
  })

  test('a trailing comment stays on the same line as its code', () => {
    const out = format('(define a 1) ; trailing')
    expect(out).toBe('(define a 1) ; trailing')
  })

  test('a comment inside a form is preserved', () => {
    const out = format('(let ([x 1] ; the x\n [y 2]) (+ x y))')
    expect(out).toContain('; the x')
    expect(out).toContain('[y 2]')
    expect(out).toContain('(+ x y)')
  })

  test('the issue #304 example keeps its docstring', () => {
    const src = [
      ';;; (factorial n) -> number?',
      ';;;   n: number?',
      ';;; Returns n!',
      '(define factorial',
      '  (lambda (n)',
      '    (if (zero? n) 1 (* n (factorial (- n 1))))))',
    ].join('\n')
    const out = format(src)
    expect(out).toContain(';;; (factorial n) -> number?')
    expect(out).toContain(';;; Returns n!')
  })

  test('formatting commented code is idempotent', () => {
    const src = [
      ';;; a documented helper',
      '(define f (lambda (x) (* x 2))) ; doubles',
      '; and a note',
      '(define g (lambda (y) (+ y 1)))',
    ].join('\n')
    const once = format(src)
    const twice = format(once)
    expect(twice).toBe(once)
    // All three comments are still present after two passes.
    expect(twice).toContain(';;; a documented helper')
    expect(twice).toContain('; doubles')
    expect(twice).toContain('; and a note')
  })

  test('a trailing comment on an inner element stays with that element', () => {
    // Comments attached to nodes (scheme/comments.ts) mean an inline comment no
    // longer migrates to the end of the whole form.
    const out = format('(+ 1 ; one\n 2)')
    expect(out).toContain('1 ; one')
    expect(tokenizeAndParse(out).diagnostics).toEqual([])
  })
})

// Comments in these positions are placed in situ and format idempotently.
describe('comment placement is valid and idempotent (#304)', () => {
  const cases: [string, string][] = [
    ['a comment-only file', '; just a comment\n'],
    ['a comment by a lambda body', '(lambda (x) ; body\n (+ x 1))'],
    ['a comment by a define name', '(define ; name\n x 1)'],
    ['a comment on a struct field', '(struct point (x ; ex\n y))'],
    ['a comment on a let binding', '(let ([x ; the x\n 1]) x)'],
    ['a comment before a closing paren', '(+ 1 2\n ; note\n)'],
  ]
  for (const [name, src] of cases) {
    test(name, () => {
      const out = format(src)
      expect(tokenizeAndParse(out).diagnostics).toEqual([])
      expect(out).toMatch(/;/) // preserved
      expect(format(out)).toBe(out) // idempotent
    })
  }
})

// Known limitations: these relocate the comment and stabilize after one further
// reformat rather than being immediately idempotent. They never lose the comment
// or produce invalid output.
describe('comment placement relocates but stays valid (#304)', () => {
  const cases: [string, string][] = [
    ["a trailing comment on a form's last element", '(+ 1 2 ; sum\n)'],
    ['a comment on a cond branch', '(cond [#t ; yes\n 1])'],
  ]
  for (const [name, src] of cases) {
    test(name, () => {
      const out = format(src)
      expect(tokenizeAndParse(out).diagnostics).toEqual([])
      expect(out).toMatch(/;/) // preserved
      const stable = format(out)
      expect(format(stable)).toBe(stable) // stable after one reformat
    })
  }

  // Vector and map literals are ordinary AST nodes (not raw data), so unlike
  // the quoted data they replaced, they round-trip through the printer exactly
  // -- comments included.
  test('a comment inside a vector literal is preserved, and formatting is idempotent', () => {
    const out = format('(define xs [1 ; one\n 2 3])')
    expect(tokenizeAndParse(out).diagnostics).toEqual([])
    expect(out).toContain('; one')
    expect(format(out)).toBe(out)
  })

  test('a comment inside a map literal is preserved, and formatting is idempotent', () => {
    const out = format('(define m {"a" 1 ; one\n "b" 2})')
    expect(tokenizeAndParse(out).diagnostics).toEqual([])
    expect(out).toContain('; one')
    expect(format(out)).toBe(out)
  })
})

// Guards the core invariant: no comment is ever silently dropped, at any
// position. There is no built-in "comment not printed" error to lean on, so
// this injects a probe comment at every token boundary of
// several realistic programs and asserts it survives in valid output.
describe('no comment is ever lost (#304)', () => {
  const programs = [
    '(define x 1)',
    '(define f (lambda (x y) (+ x y)))',
    '(let ([a 1] [b 2]) (+ a b))',
    '(let ([a 1] [b (+ a 1)]) b)',
    '(if (> x 0) 1 (- 0 x))',
    '(struct point (x y))',
    '(cond [(> n 0) 1] [else 0])',
    '(match n [0 "z"] [_ "n"])',
    '(and (> a 0) (< b 10))',
    '(display (+ 1 2))',
  ]
  const PROBE = ';__probe__'

  for (const prog of programs) {
    // Every space is an inter-token gap; probe both an own-line comment there
    // and a same-line (trailing) comment.
    const gaps = Array.from(prog).flatMap((ch, i) => (ch === ' ' ? [i] : []))
    for (const pos of gaps) {
      for (const [kind, insert] of [
        ['own-line', `\n${PROBE}\n`],
        ['trailing', ` ${PROBE}\n`],
      ] as const) {
        const src = prog.slice(0, pos) + insert + prog.slice(pos + 1)
        test(`${kind} probe at ${String(pos)} of ${prog}`, () => {
          const out = format(src)
          expect(tokenizeAndParse(out).diagnostics).toEqual([])
          expect(out).toContain(PROBE)
        })
      }
    }
  }
})

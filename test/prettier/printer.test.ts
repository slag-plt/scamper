import { describe, expect, test } from 'vitest'
import * as prettier from 'prettier'
import * as A from '../../src/scheme/ast'
import { tokenizeAndParse } from '../../src/scheme'
import ScamperPlugin from '../../src/prettier/prettier-plugin-scamper'

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

function format(src: string): Promise<string> {
  return prettier.format(src, {
    parser: 'scamper-scheme',
    plugins: [ScamperPlugin],
  })
}

function progEquals(p1: A.Prog, p2: A.Prog): boolean {
  return (
    p1.length === p2.length && p1.every((stmt, i) => A.stmtEquals(stmt, p2[i]))
  )
}

// ---- roundtrip: parse(src) ≡ parse(format(src)) ----------------------------

describe('roundtrip', () => {
  async function roundtrip(src: string): Promise<void> {
    const original = parse(src)
    const reformatted = parse(await format(src))
    expect(progEquals(original, reformatted)).toBe(true)
  }

  test('literal define', () => roundtrip('(define x 42)'))

  test('boolean define', () => roundtrip('(define flag #t)'))

  test('lambda with multiple params', () =>
    roundtrip('(define f (lambda (x y) (+ x y)))'))

  test('lambda with a rest parameter (#272)', () =>
    roundtrip('(define f (lambda (x & xs) xs))'))

  test('lambda with a rest-only parameter list (#272)', () =>
    roundtrip('(define g (lambda (& xs) xs))'))

  test('rest parameters are printed with "&" (#272)', async () => {
    expect(await format('(define f (lambda (x & xs) xs))')).toContain('(x & xs)')
    expect(await format('(define g (lambda (& xs) xs))')).toContain('(& xs)')
  })

  test('if expression', () =>
    roundtrip('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))'))

  test('let binding', () =>
    roundtrip('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))'))

  test('match expression', () =>
    roundtrip('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))'))

  test('match with constructor patterns', () =>
    roundtrip('(define head (lambda (l) (match l [(cons h _) h])))'))

  test('and / or', () =>
    roundtrip('(define both (lambda (a b) (and (> a 0) (< b 10))))'))

  test('cond expression', () =>
    roundtrip(
      '(define sign (lambda (n) (cond [(> n 0) 1] [(< n 0) -1] [#t 0])))',
    ))

  test('anonymous function #(...)', () =>
    roundtrip('(define inc #(+ %1 1))'))

  test('anonymous function with a rest parameter', () =>
    roundtrip('(define total #(apply + %&))'))

  test('anonymous function nested in an application', () =>
    roundtrip('(display (map #(* %1 %1) (list 1 2 3)))'))

  test('anonymous function with a special-form body', () =>
    roundtrip('(define f #(if (> % 0) % (- 0 %)))'))

  test('anonymous function with a let body', () =>
    roundtrip('(define g #(let ([d (* %1 2)]) (+ d %2)))'))

  test('empty anonymous function', () => roundtrip('(define k #())'))

  test('anonymous function prints with the "#(" delimiter', async () => {
    expect(await format('(define inc #(+ %1 1))')).toContain('#(+ %1 1)')
  })

  test('nullary application', () => roundtrip('(define zero (lambda () 0))'))

  test('import statement', () => roundtrip('(import image)'))

  test('struct definition', () => roundtrip('(struct point (x y))'))

  test('multi-statement program', () =>
    roundtrip('(define x 1)\n(define y 2)\n(display (+ x y))'))

  test('normalizes extra whitespace', () => roundtrip('(define   x      42)'))

  test('normalizes nested whitespace', () =>
    roundtrip('(define f   (lambda   (x   y)   (+   x   y)))'))
})

// ---- idempotence: format(src) === format(format(src)) -----------------------

describe('idempotence', () => {
  async function idempotent(src: string): Promise<void> {
    const once = await format(src)
    const twice = await format(once)
    expect(once).toBe(twice)
  }

  test('literal define', () => idempotent('(define x 42)'))

  test('lambda', () => idempotent('(define f (lambda (x y) (+ x y)))'))

  test('if expression', () =>
    idempotent('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))'))

  test('let binding', () =>
    idempotent('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))'))

  test('match expression', () =>
    idempotent('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))'))

  test('multi-statement program', () =>
    idempotent('(define x 1)\n(define y 2)\n(display (+ x y))'))
})

// ---- comments are preserved (#304) -----------------------------------------

describe('comment preservation', () => {
  test('a docstring above a define survives formatting', async () => {
    const src = [
      ';;; (add1 n) -> number?',
      ';;;   n: number?',
      ';;; Adds one to n.',
      '(define add1 (lambda (n) (+ n 1)))',
    ].join('\n')
    const out = await format(src)
    expect(out).toContain(';;; (add1 n) -> number?')
    expect(out).toContain(';;;   n: number?')
    expect(out).toContain(';;; Adds one to n.')
    // The comment stays immediately above the define it documents.
    expect(out.indexOf(';;; Adds one to n.')).toBeLessThan(out.indexOf('(define add1'))
  })

  test('a standalone comment between statements is kept on its own line', async () => {
    const out = await format('(define a 1)\n; a note\n(define b 2)')
    expect(out).toBe('(define a 1)\n; a note\n(define b 2)')
  })

  test('a trailing comment stays on the same line as its code', async () => {
    const out = await format('(define a 1) ; trailing')
    expect(out).toBe('(define a 1) ; trailing')
  })

  test('a comment inside a form is preserved', async () => {
    const out = await format('(let ([x 1] ; the x\n [y 2]) (+ x y))')
    expect(out).toContain('; the x')
    expect(out).toContain('[y 2]')
    expect(out).toContain('(+ x y)')
  })

  test('the issue #304 example keeps its docstring', async () => {
    const src = [
      ';;; (factorial n) -> number?',
      ';;;   n: number?',
      ';;; Returns n!',
      '(define factorial',
      '  (lambda (n)',
      '    (if (zero? n) 1 (* n (factorial (- n 1))))))',
    ].join('\n')
    const out = await format(src)
    expect(out).toContain(';;; (factorial n) -> number?')
    expect(out).toContain(';;; Returns n!')
  })

  test('formatting commented code is idempotent', async () => {
    const src = [
      ';;; a documented helper',
      '(define f (lambda (x) (* x 2))) ; doubles',
      '; and a note',
      '(define g (lambda (y) (+ y 1)))',
    ].join('\n')
    const once = await format(src)
    const twice = await format(once)
    expect(twice).toBe(once)
    // All three comments are still present after two passes.
    expect(twice).toContain(';;; a documented helper')
    expect(twice).toContain('; doubles')
    expect(twice).toContain('; and a note')
  })

  test('a trailing comment on an inner element stays with that element', async () => {
    // Comments attached to nodes (scheme/comments.ts) mean an inline comment no
    // longer migrates to the end of the whole form.
    const out = await format('(+ 1 ; one\n 2)')
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
    test(name, async () => {
      const out = await format(src)
      expect(tokenizeAndParse(out).diagnostics).toEqual([])
      expect(out).toMatch(/;/) // preserved
      expect(await format(out)).toBe(out) // idempotent
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
    test(name, async () => {
      const out = await format(src)
      expect(tokenizeAndParse(out).diagnostics).toEqual([])
      expect(out).toMatch(/;/) // preserved
      const stable = await format(out)
      expect(await format(stable)).toBe(stable) // stable after one reformat
    })
  }

  test('a comment inside quoted data is preserved (quote rendering is a separate bug)', async () => {
    // N.B. quoted data itself does not round-trip today: (quote (1 2 3)) prints
    // as '(list 1 2 3), which reparses differently -- a pre-existing quote
    // printer bug independent of comments. So we only assert the comment
    // survives in valid output, not idempotency.
    const out = await format("(define xs '(1 ; one\n 2 3))")
    expect(tokenizeAndParse(out).diagnostics).toEqual([])
    expect(out).toContain('; one')
  })
})

// Guards the core invariant: no comment is ever silently dropped, at any
// position. Without Prettier's comment system there is no built-in "comment not
// printed" error, so this injects a probe comment at every token boundary of
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
        test(`${kind} probe at ${String(pos)} of ${prog}`, async () => {
          const out = await format(src)
          expect(tokenizeAndParse(out).diagnostics).toEqual([])
          expect(out).toContain(PROBE)
        })
      }
    }
  }
})

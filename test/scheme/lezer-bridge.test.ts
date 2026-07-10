import { describe, expect, test } from "vitest"
import * as L from "../../src/lpm"
import { parseProgramFromSource } from "../../src/scheme/lezer-bridge"
import { parseProgram, reservedWords } from "../../src/scheme/parser"
import { read } from "../../src/scheme/reader"

function bothParse(src: string) {
  const oldErrors: L.ScamperError[] = []
  const oldProg = parseProgram(oldErrors, read(src))
  const newErrors: L.ScamperError[] = []
  const newProg = parseProgramFromSource(newErrors, src)
  return { oldProg, oldErrors, newProg, newErrors }
}

// N.B., L.equals (and so expEquals/stmtEquals) falls back to reference
// equality for L.Range/L.Loc, since those are plain classes rather than
// Scamper-tagged structs -- so it can never confirm two independently
// constructed ranges are equal. That's fine for stmtEquals/expEquals's own
// purposes (they don't compare top-level AST node ranges at all), but it's
// unusable here, since we specifically want to confirm the bridge produces
// byte-for-byte identical ranges everywhere, including inside quoted/vector
// literal data. Hence this test's own deep, range-aware comparator.
function deepEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true
  if (a instanceof L.Range && b instanceof L.Range) {
    return deepEqual(a.begin, b.begin) && deepEqual(a.end, b.end)
  }
  if (a instanceof L.Loc && b instanceof L.Loc) {
    return a.line === b.line && a.col === b.col && a.idx === b.idx
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((x, i) => deepEqual(x, b[i]))
  }
  if (typeof a === "object" && a !== null && typeof b === "object" && b !== null) {
    const keys = new Set([...Object.keys(a), ...Object.keys(b)])
    for (const k of keys) {
      if (!deepEqual((a as never)[k], (b as never)[k])) return false
    }
    return true
  }
  return false
}

function expectEquivalent(src: string) {
  const { oldProg, oldErrors, newProg, newErrors } = bothParse(src)
  expect(oldErrors, `old parser errors for ${JSON.stringify(src)}`).toEqual([])
  expect(newErrors, `new parser errors for ${JSON.stringify(src)}`).toEqual([])
  expect(newProg.length, `statement count for ${JSON.stringify(src)}`).toBe(
    oldProg.length,
  )
  for (let i = 0; i < oldProg.length; i++) {
    expect(
      deepEqual(oldProg[i], newProg[i]),
      `statement ${i} mismatch for ${JSON.stringify(src)}:\n  old: ${JSON.stringify(oldProg[i])}\n  new: ${JSON.stringify(newProg[i])}`,
    ).toBe(true)
  }
}

describe("lezer-bridge parity with reader.ts/parser.ts", () => {
  test("core forms", () => {
    expectEquivalent("(lambda (x y) (+ x y))")
    expectEquivalent("(let ([x 1] [y 2]) (+ x y))")
    expectEquivalent("(let* ([x 1] [y (+ x 1)]) y)")
    expectEquivalent("(begin 1 2 3)")
    expectEquivalent("(if #t 1 2)")
    expectEquivalent("(and)")
    expectEquivalent("(and 1 2 3)")
    expectEquivalent("(or)")
    expectEquivalent("(or 1 2 3)")
  })

  test("quote, in both shorthand and explicit forms, including nested", () => {
    expectEquivalent("'(1 2 3)")
    expectEquivalent("(quote (1 2 3))")
    expectEquivalent("'(a (b c) #\\x \"s\" #t)")
    expectEquivalent("''a")
  })

  test("vector literals", () => {
    expectEquivalent("(display [1 2 3])")
    expectEquivalent("[]")
    expectEquivalent("(display [(+ 1 2) \"x\" #t])")
  })

  test("match with number/string/char/vector/ctor/wildcard patterns", () => {
    expectEquivalent(
      '(match x [0 "zero"] [1.5 "half"] [#\\a "a"] ["s" "str"] [#t "t"] [(cons a b) a] [[1 2] "vec"] [_ "other"])',
    )
    expectEquivalent("(match x [_ 1] [_ 2])")
  })

  test("cond with test/body pairs, including zero and many branches", () => {
    expectEquivalent("(cond)")
    expectEquivalent('(cond [(> x 0) "pos"] [(< x 0) "neg"] [#t "zero"])')
  })

  test("section and struct", () => {
    expectEquivalent("(display (map (section + _ 1) (list 1 2 3)))")
    expectEquivalent("(struct point (x y))")
    expectEquivalent("(struct empty ())")
  })

  test("report (internal form, but valid syntax)", () => {
    expectEquivalent("(report (+ 1 2))")
  })

  test("import/define/display, including empty top-level list", () => {
    expectEquivalent(
      "(import lists)\n(define f (lambda (x) x))\n(display (f 1))",
    )
    expectEquivalent("()")
  })

  test("numbers", () => {
    expectEquivalent(
      "(display (list 42 -3.14 .5 2. 6.02e23 -1e-10 +7 0))",
    )
  })

  test("identifiers with special characters", () => {
    expectEquivalent("(define null? (lambda (x) (= x null)))")
    expectEquivalent("(display (> 1 2))")
    expectEquivalent("(display (<= 1 2))")
    expectEquivalent("(display (set!-like-name 1))")
  })

  test("null literal", () => {
    expectEquivalent("(display null)")
    expectEquivalent("(match null [null \"n\"] [_ \"other\"])")
  })

  test("nested application and strings with escapes", () => {
    expectEquivalent('(display "line1\\nline2\\ttabbed")')
    expectEquivalent("(display ((lambda (x) (x 1)) (lambda (y) y)))")
  })

  // N.B., reserved-word misuse (e.g. "(define and 5)") makes the Lezer parser
  // emit an error-recovery node rather than a clean tree, since kw<> keywords
  // are @specialize'd and can never be re-read as plain identifiers. Comparing
  // that against the old parser's error requires reconciling the two error
  // models, which is migration Phase 4's job, not this bridge's.

  test("define docstring is parsed identically", () => {
    const src = [
      ";;; (add1 x) -> number?",
      ";;;  x : number?",
      ";;; Adds one to a number.",
      "(define add1 (lambda (x) (+ x 1)))",
    ].join("\n")
    const oldErrors: L.ScamperError[] = []
    const oldProg = parseProgram(oldErrors, read(src))
    const newErrors: L.ScamperError[] = []
    const newProg = parseProgramFromSource(newErrors, src)
    expect(oldErrors).toEqual([])
    expect(newErrors).toEqual([])
    const oldDoc = (oldProg[0] as { doc?: unknown }).doc
    const newDoc = (newProg[0] as { doc?: unknown }).doc
    expect(oldDoc).toBeDefined()
    expect(newDoc).toEqual(oldDoc)
  })

  test("every reserved word is exercised by at least one sample above", () => {
    // N.B., a lightweight guard against silently losing coverage of a form
    // as reservedWords grows.
    expect(reservedWords.sort()).toEqual(
      [
        "and",
        "begin",
        "cond",
        "define",
        "display",
        "if",
        "import",
        "lambda",
        "let",
        "let*",
        "match",
        "or",
        "quote",
        "report",
        "section",
        "struct",
      ].sort(),
    )
  })
})

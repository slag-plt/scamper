import { describe, expect, test } from "vitest"
import { reservedWords } from "../../src/scheme/parser"
import { bothParse, expectEquivalent } from "./lezer-bridge-test-utils"

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

  test("curly braces as an alternate to parens, mixed freely (each closed with its own kind)", () => {
    expectEquivalent("{+ 1 2}")
    expectEquivalent("(+ {* 3 4} (- 5 1))")
    expectEquivalent("{define f (lambda (x) {+ x 1})}")
    expectEquivalent("{if #t {+ 1 2} 3}")
    expectEquivalent("{let ([x 1]) {+ x 1}}")
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
    const { oldProg, oldErrors, newProg, newErrors } = bothParse(src)
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

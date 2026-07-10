import { describe, expect, test } from "vitest"
import * as A from "../../src/scheme/ast"
import { reservedWords } from "../../src/scheme/reserved-words"
import { expectParses, parse } from "./lezer-bridge-test-utils"

describe("lezer-bridge parsing", () => {
  test("core forms", () => {
    expectParses("(lambda (x y) (+ x y))")
    expectParses("(let ([x 1] [y 2]) (+ x y))")
    expectParses("(let* ([x 1] [y (+ x 1)]) y)")
    expectParses("(begin 1 2 3)")
    expectParses("(if #t 1 2)")
    expectParses("(and)")
    expectParses("(and 1 2 3)")
    expectParses("(or)")
    expectParses("(or 1 2 3)")
  })

  test("curly braces as an alternate to parens, mixed freely (each closed with its own kind)", () => {
    expectParses("{+ 1 2}")
    expectParses("(+ {* 3 4} (- 5 1))")
    expectParses("{define f (lambda (x) {+ x 1})}")
    expectParses("{if #t {+ 1 2} 3}")
    expectParses("{let ([x 1]) {+ x 1}}")
  })

  test("quote shorthand desugars to (quote <unwrapped payload>), matching reader.ts's own shorthand desugaring", () => {
    const { prog, errors } = parse("'(1 2 3)")
    expect(errors).toEqual([])
    expect(prog.length).toBe(1)
    const stmt = prog[0]
    expect(stmt.tag).toBe("stmtexp")
    if (stmt.tag !== "stmtexp") return
    expect(stmt.expr.tag).toBe("quote")
    if (stmt.expr.tag !== "quote") return
    // N.B., nested elements of quoted data stay Syntax-wrapped (mkSyntax),
    // matching parser.ts's real S.stripSyntax(arr[1]) behavior -- only the
    // outermost wrapper is stripped. See lezer-bridge.ts's nodeToRawValue.
    const value = stmt.expr.value as { head: unknown; tail: unknown }
    expect(value).toHaveProperty("head")
    const head = value.head as { value: unknown }
    expect(head.value).toBe(1)
  })

  test("explicit quote form and nested quote", () => {
    expectParses("(quote (1 2 3))")
    expectParses("'(a (b c) #\\x \"s\" #t)")
    expectParses("''a")
  })

  test("vector literals are literal data, not sub-expressions to evaluate", () => {
    const { prog, errors } = parse("(display [1 2 3])")
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe("display")
    if (stmt.tag !== "display") return
    expect(stmt.value.tag).toBe("lit")
    if (stmt.value.tag !== "lit") return
    const arr = stmt.value.value as { value: unknown }[]
    expect(arr.map((x) => x.value)).toEqual([1, 2, 3])

    expectParses("[]")
    expectParses('(display [(+ 1 2) "x" #t])')
  })

  test("match with number/string/char/vector/ctor/wildcard patterns", () => {
    expectParses(
      '(match x [0 "zero"] [1.5 "half"] [#\\a "a"] ["s" "str"] [#t "t"] [(cons a b) a] [[1 2] "vec"] [_ "other"])',
    )
  })

  test("wildcard pattern produces pwild, not pvar, so repeated _ doesn't collide", () => {
    const { prog, errors } = parse("(match x [_ 1] [_ 2])")
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe("stmtexp")
    if (stmt.tag !== "stmtexp") return
    expect(stmt.expr.tag).toBe("match")
    if (stmt.expr.tag !== "match") return
    expect(stmt.expr.branches.map((b) => b.pat.tag)).toEqual(["pwild", "pwild"])
  })

  test("cond with test/body pairs, including zero and many branches", () => {
    expectParses("(cond)")
    expectParses('(cond [(> x 0) "pos"] [(< x 0) "neg"] [#t "zero"])')
  })

  test("section and struct", () => {
    expectParses("(display (map (section + _ 1) (list 1 2 3)))")
    expectParses("(struct point (x y))")
    expectParses("(struct empty ())")
  })

  test("report (internal form, but valid syntax)", () => {
    expectParses("(report (+ 1 2))")
  })

  test("import/define/display, including empty top-level list", () => {
    expectParses("(import lists)\n(define f (lambda (x) x))\n(display (f 1))")
    expectParses("()")
  })

  test("numbers", () => {
    expectParses("(display (list 42 -3.14 .5 2. 6.02e23 -1e-10 +7 0))")
  })

  test("identifiers with special characters", () => {
    expectParses("(define null? (lambda (x) (= x null)))")
    expectParses("(display (> 1 2))")
    expectParses("(display (<= 1 2))")
    expectParses("(display (set!-like-name 1))")
  })

  test("null literal", () => {
    expectParses("(display null)")
    expectParses('(match null [null "n"] [_ "other"])')
  })

  test("nested application and strings with escapes", () => {
    const { prog, errors } = parse('(display "line1\\nline2\\ttabbed")')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe("display")
    if (stmt.tag !== "display") return
    expect(stmt.value).toEqual(
      A.mkLit("line1\nline2\ttabbed", stmt.value.range),
    )
    expectParses("(display ((lambda (x) (x 1)) (lambda (y) y)))")
  })

  // N.B., reserved-word misuse (e.g. "(define and 5)") makes the Lezer parser
  // emit an error-recovery node rather than a clean tree, since kw<> keywords
  // are @specialize'd and can never be re-read as plain identifiers -- this
  // is exercised in lezer-bridge-errors.test.ts instead.

  test("define's preceding doc comments are captured, unparsed", () => {
    // N.B., the bridge only captures the raw comments -- parsing them into a
    // FunctionDoc is deferred (see docstring.ts's parseFunctionDocFromComments),
    // so malformed docstrings can never fail this parse. Parsing on demand is
    // exercised directly in docstring.test.ts.
    const src = [
      ";;; (add1 x) -> number?",
      ";;;  x : number?",
      ";;; Adds one to a number.",
      "(define add1 (lambda (x) (+ x 1)))",
    ].join("\n")
    const { prog, errors } = parse(src)
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe("define")
    if (stmt.tag !== "define") return
    expect(stmt.docComments?.map((c) => c.line)).toEqual([
      ";;; (add1 x) -> number?",
      ";;;  x : number?",
      ";;; Adds one to a number.",
    ])
  })

  test("every reserved word is exercised by at least one sample above", () => {
    // N.B., a lightweight guard against silently losing coverage of a form
    // as reservedWords grows.
    expect(reservedWords.slice().sort()).toEqual(
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

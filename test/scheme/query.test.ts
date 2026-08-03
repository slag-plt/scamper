import { describe, expect, test } from 'vitest'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { getQueriedProgram, getReportedExp, getReportedStmt } from '../../src/scheme/query'
import { isStmtExp, mkId, mkLit, Prog } from '../../src/scheme/ast'
import { Loc } from '../../src/lpm'
import * as L from '../../src/lpm'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { anyRange } from './util'
import { compile } from '../../src/scheme'

const testLit = 'test lit'
const testDispLit = 2

const testProgram = `"${testLit}"
()
(display ${testDispLit.toString()})
(test-func1
  "yo"
  (test-func2 "what's up"))
("not-a-fn" 1)
((lambda (x) x) 5)
(if #t 1 2)`

function parseTestProgram(): Prog {
  const errors: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errors, testProgram)
  expect(errors).toEqual([])
  return prog
}

// Finds the Loc of `needle` (first occurrence, or `occurrence`-th if given)
// within src, so tests can point at a specific token by content instead of
// hand-computing line/col/idx by hand.
function locIn(src: string, needle: string, occurrence = 0): Loc {
  let idx = -1
  for (let i = 0; i <= occurrence; i++) {
    idx = src.indexOf(needle, idx + 1)
  }
  const before = src.slice(0, idx)
  const line = before.split('\n').length
  const lineStart = before.lastIndexOf('\n') + 1
  return new Loc(line, idx - lineStart + 1, idx)
}

function locOf(needle: string, occurrence = 0): Loc {
  return locIn(testProgram, needle, occurrence)
}

// Parses src as a single bare-expression statement and returns its
// expression, for tests that only need a small standalone Exp to query.
function parseExp(src: string) {
  const errors: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errors, src)
  expect(errors).toEqual([])
  const stmt = prog[0]
  if (!isStmtExp(stmt)) {
    throw new Error(`expected a bare expression statement, got tag "${stmt.tag}"`)
  }
  return stmt.expr
}

describe('AST querying', () => {
  describe('getQueriedProgram', () => {
    test('returns a new program without mutating the input', () => {
      const prog = parseTestProgram()
      const queryLoc = locOf('yo')
      const originalStmt = prog[3]
      const queried = getQueriedProgram(prog, queryLoc)
      expect(queried.ok).toBe(true)
      if (!queried.ok) return
      const reportedProg = queried.prog

      expect(reportedProg).not.toBe(prog)
      expect(reportedProg[3]).not.toBe(prog[3])
      expect(prog[3]).toBe(originalStmt)
      expect(reportedProg[3]).toStrictEqual(
        getReportedStmt(originalStmt, queryLoc).stmt,
      )
    })

    test('returns the range of the queried expression', () => {
      const prog = parseTestProgram()
      const queryLoc = locOf('yo')
      const queried = getQueriedProgram(prog, queryLoc)
      expect(queried.ok).toBe(true)
      if (!queried.ok) return
      expect(queried.range.begin.idx).toBe(testProgram.indexOf('"yo"'))
    })

    test('returns a diagnostic for a query location outside every statement', () => {
      const prog = parseTestProgram()
      const result = getQueriedProgram(prog, new Loc(1000, 1, 100000))
      expect(result.ok).toBe(false)
    })
  })

  describe('getReportedExp', () => {
    test("wraps a bare literal statement's expression in report", () => {
      const prog = parseTestProgram()
      const stmt = prog[0]
      expect(stmt.tag).toBe('stmtexp')
      if (stmt.tag !== 'stmtexp') return
      const { stmt: reported } = getReportedStmt(stmt, locOf(`"${testLit}"`))
      expect(reported).toStrictEqual({
        tag: 'stmtexp',
        expr: { tag: 'report', exp: mkLit(testLit, anyRange), range: anyRange },
        range: anyRange,
      })
    })

    test('wraps the null literal from an empty list', () => {
      const prog = parseTestProgram()
      const stmt = prog[1]
      expect(stmt.tag).toBe('stmtexp')
      if (stmt.tag !== 'stmtexp') return
      const { stmt: reported } = getReportedStmt(stmt, locOf('()'))
      expect(reported).toStrictEqual({
        tag: 'stmtexp',
        expr: { tag: 'report', exp: mkLit(null, anyRange), range: anyRange },
        range: anyRange,
      })
    })

    test('wraps the entire application when the query lands on its closing bracket, not any argument', () => {
      const prog = parseTestProgram()
      const stmt = prog[2]
      expect(stmt.tag).toBe('display')
      if (stmt.tag !== 'display') return
      // N.B., there is no sub-expression slot for a Lit like `2` itself, so
      // querying anywhere within its range (including its own "closing
      // bracket" position, conceptually) always wraps the whole thing --
      // there's nothing deeper to recurse into.
      const { exp } = getReportedExp(stmt.value, locOf(testDispLit.toString()))
      expect(exp.tag).toBe('report')
    })

    describe('recursive case: queried a non-head argument', () => {
      test('reports one level deep', () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const { exp } = getReportedExp(stmt.expr, locOf('"yo"'))
        expect(exp.tag).toBe('app')
        if (exp.tag !== 'app') return
        expect(exp.args[0]).toStrictEqual({
          tag: 'report',
          exp: mkLit('yo', anyRange),
          range: anyRange,
        })
        // the sibling argument is untouched
        expect(exp.args[1].tag).toBe('app')
      })

      test('reports two levels deep', () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const { exp } = getReportedExp(stmt.expr, locOf("what's up"))
        expect(exp.tag).toBe('app')
        if (exp.tag !== 'app') return
        expect(exp.args[0]).toStrictEqual(mkLit('yo', anyRange))
        const inner = exp.args[1]
        expect(inner.tag).toBe('app')
        if (inner.tag !== 'app') return
        expect(inner.args[0]).toStrictEqual({
          tag: 'report',
          exp: mkLit("what's up", anyRange),
          range: anyRange,
        })
      })

      test('does not mutate the input expression', () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const before = JSON.stringify(stmt.expr)
        getReportedExp(stmt.expr, locOf('"yo"'))
        getReportedExp(stmt.expr, locOf("what's up"))
        expect(JSON.stringify(stmt.expr)).toBe(before)
      })
    })

    describe("base case: queried a special form's own syntax (not a sub-expression)", () => {
      test('wraps the entire if-expression when the query lands on `if` itself', () => {
        const prog = parseTestProgram()
        const stmt = prog[6]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const { exp } = getReportedExp(stmt.expr, locOf('if'))
        expect(exp.tag).toBe('report')
        if (exp.tag !== 'report') return
        expect(exp.exp.tag).toBe('if')
      })
    })

    describe('base case: queried the head of an application', () => {
      test("wraps just the head when it's a non-function literal", () => {
        const prog = parseTestProgram()
        const stmt = prog[4]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const { exp } = getReportedExp(stmt.expr, locOf('"not-a-fn"'))
        expect(exp.tag).toBe('app')
        if (exp.tag !== 'app') return
        expect(exp.head).toStrictEqual({
          tag: 'report',
          exp: mkLit('not-a-fn', anyRange),
          range: anyRange,
        })
        // the argument is untouched
        expect(exp.args[0]).toStrictEqual(mkLit(1, anyRange))
      })

      test('recurses into an anonymous function used as the head', () => {
        const prog = parseTestProgram()
        const stmt = prog[5]
        expect(stmt.tag).toBe('stmtexp')
        if (stmt.tag !== 'stmtexp') return
        const { exp } = getReportedExp(stmt.expr, locOf('x)', 1))
        expect(exp.tag).toBe('app')
        if (exp.tag !== 'app') return
        expect(exp.head.tag).toBe('lam')
        if (exp.head.tag !== 'lam') return
        expect(exp.head.body).toStrictEqual({
          tag: 'report',
          exp: mkId('x', anyRange),
          range: anyRange,
        })
      })
    })

    describe('error', () => {
      test('reports the wrapped expression', () => {
        const src = '(error "boom")'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, '"boom"'))
        expect(reported.tag).toBe('error')
        if (reported.tag !== 'error') return
        expect(reported.exp).toStrictEqual({
          tag: 'report',
          exp: mkLit('boom', anyRange),
          range: anyRange,
        })
      })
    })

    describe('lam', () => {
      test('reports the body slot', () => {
        const src = '(lambda (x) x)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'x', 1))
        expect(reported.tag).toBe('lam')
        if (reported.tag !== 'lam') return
        expect(reported.body).toStrictEqual({
          tag: 'report',
          exp: mkId('x', anyRange),
          range: anyRange,
        })
      })
    })

    describe('if', () => {
      const src = '(if a b c)'

      test('reports the guard slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'a'))
        expect(reported.tag).toBe('if')
        if (reported.tag !== 'if') return
        expect(reported.guard).toStrictEqual({
          tag: 'report',
          exp: mkId('a', anyRange),
          range: anyRange,
        })
        expect(reported.ifB).toStrictEqual(mkId('b', anyRange))
        expect(reported.elseB).toStrictEqual(mkId('c', anyRange))
      })

      test('reports the ifB slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'b'))
        expect(reported.tag).toBe('if')
        if (reported.tag !== 'if') return
        expect(reported.guard).toStrictEqual(mkId('a', anyRange))
        expect(reported.ifB).toStrictEqual({
          tag: 'report',
          exp: mkId('b', anyRange),
          range: anyRange,
        })
        expect(reported.elseB).toStrictEqual(mkId('c', anyRange))
      })

      test('reports the elseB slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'c'))
        expect(reported.tag).toBe('if')
        if (reported.tag !== 'if') return
        expect(reported.guard).toStrictEqual(mkId('a', anyRange))
        expect(reported.ifB).toStrictEqual(mkId('b', anyRange))
        expect(reported.elseB).toStrictEqual({
          tag: 'report',
          exp: mkId('c', anyRange),
          range: anyRange,
        })
      })
    })

    describe('and/or/begin/section (shared flat-list-of-expressions slot logic)', () => {
      test('and reports an element and rebuilds via mkAnd', () => {
        const src = '(and p q)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'q'))
        expect(reported.tag).toBe('and')
        if (reported.tag !== 'and') return
        expect(reported.exps[0]).toStrictEqual(mkId('p', anyRange))
        expect(reported.exps[1]).toStrictEqual({
          tag: 'report',
          exp: mkId('q', anyRange),
          range: anyRange,
        })
      })

      test('begin reports an element and rebuilds via mkBegin', () => {
        const src = '(begin p q)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'p'))
        expect(reported.tag).toBe('begin')
        if (reported.tag !== 'begin') return
        expect(reported.exps[0]).toStrictEqual({
          tag: 'report',
          exp: mkId('p', anyRange),
          range: anyRange,
        })
        expect(reported.exps[1]).toStrictEqual(mkId('q', anyRange))
      })
    })

    describe('let/let*', () => {
      test('let reports a binding value slot', () => {
        const src = '(let ([x 1] [y 2]) z)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, '1'))
        expect(reported.tag).toBe('let')
        if (reported.tag !== 'let') return
        expect(reported.bindings[0]).toStrictEqual({
          id: mkId('x', anyRange),
          value: { tag: 'report', exp: mkLit(1, anyRange), range: anyRange },
        })
        expect(reported.bindings[1]).toStrictEqual({
          id: mkId('y', anyRange),
          value: mkLit(2, anyRange),
        })
        expect(reported.body).toStrictEqual(mkId('z', anyRange))
      })

      test('let reports the body slot', () => {
        const src = '(let ([x 1] [y 2]) z)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'z'))
        expect(reported.tag).toBe('let')
        if (reported.tag !== 'let') return
        expect(reported.bindings[0]).toStrictEqual({
          id: mkId('x', anyRange),
          value: mkLit(1, anyRange),
        })
        expect(reported.bindings[1]).toStrictEqual({
          id: mkId('y', anyRange),
          value: mkLit(2, anyRange),
        })
        expect(reported.body).toStrictEqual({
          tag: 'report',
          exp: mkId('z', anyRange),
          range: anyRange,
        })
      })

      test('let* reports a binding value slot', () => {
        const src = '(let* ([x 1]) x)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, '1'))
        expect(reported.tag).toBe('let*')
        if (reported.tag !== 'let*') return
        expect(reported.bindings[0]).toStrictEqual({
          id: mkId('x', anyRange),
          value: { tag: 'report', exp: mkLit(1, anyRange), range: anyRange },
        })
        expect(reported.body).toStrictEqual(mkId('x', anyRange))
      })
    })

    describe('cond', () => {
      const src = '(cond [p q] [r s])'

      test('reports a branch test slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'p'))
        expect(reported.tag).toBe('cond')
        if (reported.tag !== 'cond') return
        expect(reported.branches[0]).toStrictEqual({
          test: { tag: 'report', exp: mkId('p', anyRange), range: anyRange },
          body: mkId('q', anyRange),
        })
        expect(reported.branches[1]).toStrictEqual({
          test: mkId('r', anyRange),
          body: mkId('s', anyRange),
        })
      })

      test('reports a branch body slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 's'))
        expect(reported.tag).toBe('cond')
        if (reported.tag !== 'cond') return
        expect(reported.branches[0]).toStrictEqual({
          test: mkId('p', anyRange),
          body: mkId('q', anyRange),
        })
        expect(reported.branches[1]).toStrictEqual({
          test: mkId('r', anyRange),
          body: { tag: 'report', exp: mkId('s', anyRange), range: anyRange },
        })
      })
    })

    describe('match', () => {
      const src = '(match v [x y] [_ z])'

      test('reports the scrutinee slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'v'))
        expect(reported.tag).toBe('match')
        if (reported.tag !== 'match') return
        expect(reported.scrutinee).toStrictEqual({
          tag: 'report',
          exp: mkId('v', anyRange),
          range: anyRange,
        })
        // patterns and branch bodies are untouched
        expect(reported.branches[0].body).toStrictEqual(mkId('y', anyRange))
        expect(reported.branches[1].body).toStrictEqual(mkId('z', anyRange))
      })

      // N.B., patterns aren't queryable slots (see slotsOf's comment in
      // query.ts), so only scrutinee and branch bodies are tested here.
      test('reports a branch body slot', () => {
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'z'))
        expect(reported.tag).toBe('match')
        if (reported.tag !== 'match') return
        expect(reported.scrutinee).toStrictEqual(mkId('v', anyRange))
        expect(reported.branches[0].body).toStrictEqual(mkId('y', anyRange))
        expect(reported.branches[1].body).toStrictEqual({
          tag: 'report',
          exp: mkId('z', anyRange),
          range: anyRange,
        })
      })
    })

    describe('report', () => {
      test('is transparent when nested inside another slot', () => {
        const src = '(f (report x) y)'
        const exp = parseExp(src)
        const { exp: reported } = getReportedExp(exp, locIn(src, 'x'))
        expect(reported.tag).toBe('app')
        if (reported.tag !== 'app') return
        expect(reported.args[0]).toStrictEqual({
          tag: 'report',
          exp: { tag: 'report', exp: mkId('x', anyRange), range: anyRange },
          range: anyRange,
        })
        // the sibling argument is untouched
        expect(reported.args[1]).toStrictEqual(mkId('y', anyRange))
      })
    })
  })

  describe('compilation with query loc', () => {
    test('returns first-line queriedRange for multi-line expressions', () => {
      const src = `(define foo
  (bar
    x))`
      const closeIdx = src.indexOf('x)') + 1
      const line = src.slice(0, closeIdx).split('\n').length
      const lineStart = src.lastIndexOf('\n', closeIdx - 1) + 1
      const queryLoc = new Loc(line, closeIdx - lineStart + 1, closeIdx)

      const errors: ScamperDiagnostic[] = []
      const prog = parseProgramFromSource(errors, src)
      const queried = getQueriedProgram(prog, queryLoc)
      if (!queried.ok) {
        expect.fail('expected a valid query location')
        return
      }
      const range = queried.range
      const firstLine = range.firstLineSpan(src)

      expect(range.begin.line).toBeLessThan(range.end.line)
      expect(firstLine.begin).toEqual(range.begin)
      expect(firstLine.end.line).toBe(range.begin.line)
      expect(firstLine.end.idx).toBeLessThan(range.end.idx)
    })

    test('compile returns queriedRange for valid queries', async () => {
      const src = `;;;
;;; (foo) -> number?
;;; constant one
;;; @example (foo) -> 1
(define foo 1)`
      const oneIdx = src.indexOf('(define foo 1)') + '(define foo '.length
      const line = src.slice(0, oneIdx).split('\n').length
      const lineStart = src.lastIndexOf('\n', oneIdx - 1) + 1
      const queryLoc = new Loc(line, oneIdx - lineStart + 1, oneIdx)

      const { prog, queriedRange, diagnostics } = await compile(src, { queryLoc })

      expect(diagnostics).toStrictEqual([])
      if (prog === undefined) {
        expect.fail('expected compile to return a program')
        return
      }
      const errors: ScamperDiagnostic[] = []
      const parsed = parseProgramFromSource(errors, src)
      const queried = getQueriedProgram(parsed, queryLoc)
      if (!queried.ok) {
        expect.fail('expected a valid query location')
        return
      }
      expect(queriedRange).toEqual(queried.range.firstLineSpan(src))
    })

    // The query loc points at `a` in the lambda body of the *first* statement
    // (the define), so `report` lowers to a rept op inside that closure body
    // (`[var a, rept]`) -- not the synthesized @example disp, whose expr is a
    // plain call (`[var myid, lit 1, lit 2, ap 2]`). We assert by op-tag
    // location, robust to ranges and closure internals.
    test('a queried sub-expression lowers `report` to a rept op in its own statement', async () => {
      const funcName = 'myid'
      const src = `;;; (${funcName} a b) -> number?
;;;  a : number?
;;;  b : number?
;;; returns a
;;; @example (${funcName} 1 2) -> 1
(define ${funcName} (lambda (a b) a))`
      const queryLoc = new Loc(6, 34, src.lastIndexOf('a)'))

      const { prog, diagnostics } = await compile(src, { queryLoc })
      expect(diagnostics).toStrictEqual([])
      if (prog === undefined) {
        expect.fail('expected compile to return a program')
        return
      }

      // Collect every op tag in a block, recursing into closure bodies.
      const tagsOf = (blk: L.Blk): string[] =>
        blk.flatMap((op) =>
          op.tag === 'cls' ? ['cls', ...tagsOf(op.body)] : [op.tag],
        )

      const define = prog[0] as L.Define
      const disp = prog[prog.length - 1] as L.Disp
      expect(tagsOf(define.expr)).toContain('rept')
      expect(tagsOf(disp.expr)).not.toContain('rept')
    })
  })
})

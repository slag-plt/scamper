import { describe, test, expect } from 'vitest'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import * as A from '../../src/scheme/ast'
import { Range, ScamperError } from '../../src/lpm'

// This suite pins down the source ranges the parser attaches to every AST
// node -- and, since the Var/PVar collapse, every identifier's own range.
// Each assertion ties a node's range to an exact substring of the source, so
// the tests double as executable documentation of what span each construct
// occupies.

///// Helpers //////////////////////////////////////////////////////////////////

function parse(src: string): A.Prog {
  const errors: ScamperError[] = []
  const prog = parseProgramFromSource(errors, src)
  expect(
    errors,
    `unexpected parse errors: ${errors.map((e) => e.message).join('; ')}`,
  ).toEqual([])
  return prog
}

function parseStmt(src: string): A.Stmt {
  const prog = parse(src)
  expect(prog.length).toBe(1)
  return prog[0]
}

function asStmt<T extends A.Stmt['tag']>(
  s: A.Stmt,
  tag: T,
): Extract<A.Stmt, { tag: T }> {
  expect(s.tag).toBe(tag)
  return s as Extract<A.Stmt, { tag: T }>
}

function asExp<T extends A.Exp['tag']>(
  e: A.Exp,
  tag: T,
): Extract<A.Exp, { tag: T }> {
  expect(e.tag).toBe(tag)
  return e as Extract<A.Exp, { tag: T }>
}

function asPat<T extends A.Pat['tag']>(
  p: A.Pat,
  tag: T,
): Extract<A.Pat, { tag: T }> {
  expect(p.tag).toBe(tag)
  return p as Extract<A.Pat, { tag: T }>
}

function parseExp(src: string): A.Exp {
  return asStmt(parseStmt(src), 'stmtexp').expr
}

// A character that cannot appear inside a Scheme identifier, hence a token
// boundary. `undefined` (string start/end) counts as a boundary too.
function isDelim(ch: string | undefined): boolean {
  return ch === undefined || /[\s()[\]'"]/.test(ch)
}

// Index of the `n`-th (0-based) occurrence of `text` in `s` that stands as a
// whole token -- i.e. is delimited on both sides. This keeps a search for a
// short identifier like `n` from matching the `n` buried inside `define`.
function nthTokenIndex(s: string, text: string, n: number): number {
  let from = 0
  let count = 0
  for (;;) {
    const idx = s.indexOf(text, from)
    if (idx < 0) return -1
    const before = idx === 0 ? undefined : s[idx - 1]
    const after = idx + text.length >= s.length ? undefined : s[idx + text.length]
    if (isDelim(before) && isDelim(after)) {
      if (count === n) return idx
      count++
    }
    from = idx + 1
  }
}

// The parser's Loc convention: 1-based line, 1-based col, 0-based idx.
function locAt(
  src: string,
  idx: number,
): { line: number; col: number; idx: number } {
  const before = src.slice(0, idx)
  return {
    line: before.split('\n').length,
    col: idx - (before.lastIndexOf('\n') + 1) + 1,
    idx,
  }
}

// Asserts `range` spans exactly the `occurrence`-th occurrence of `text` in
// `src`. Ranges are inclusive on the end (they point at the last character,
// not one past it), so a k-character token at offset i spans [i, i + k - 1].
function assertSpan(
  range: Range,
  src: string,
  text: string,
  occurrence = 0,
): void {
  const start = nthTokenIndex(src, text, occurrence)
  expect(
    start,
    `token ${JSON.stringify(text)} #${occurrence.toString()} not found in source`,
  ).toBeGreaterThanOrEqual(0)
  const end = start + text.length - 1
  expect({
    line: range.begin.line,
    col: range.begin.col,
    idx: range.begin.idx,
  }).toEqual(locAt(src, start))
  expect({
    line: range.end.line,
    col: range.end.col,
    idx: range.end.idx,
  }).toEqual(locAt(src, end))
}

///// Literals /////////////////////////////////////////////////////////////////

describe('literals', () => {
  test('integer', () => {
    const src = '42'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '42')
  })
  test('decimal', () => {
    const src = '3.14'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '3.14')
  })
  test('string (quotes included)', () => {
    const src = '"hello world"'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '"hello world"')
  })
  test('boolean', () => {
    const src = '#t'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '#t')
  })
  test('char', () => {
    const src = '#\\a'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '#\\a')
  })
  test('null', () => {
    const src = 'null'
    assertSpan(asExp(parseExp(src), 'lit').range, src, 'null')
  })
  test('empty list', () => {
    const src = '()'
    assertSpan(asExp(parseExp(src), 'lit').range, src, '()')
  })
})

///// Identifiers //////////////////////////////////////////////////////////////

describe('identifiers (variable references)', () => {
  test('bare identifier', () => {
    const src = 'foo'
    const id = asExp(parseExp(src), 'id')
    expect(id.name).toBe('foo')
    assertSpan(id.range, src, 'foo')
  })
  test('single-character identifier has an inclusive one-column range', () => {
    const src = 'x'
    const id = asExp(parseExp(src), 'id')
    assertSpan(id.range, src, 'x')
    expect(id.range.begin.idx).toBe(id.range.end.idx)
  })
})

///// Core expression forms ////////////////////////////////////////////////////

describe('application', () => {
  test('head and argument ranges', () => {
    const src = '(foo bar baz)'
    const app = asExp(parseExp(src), 'app')
    assertSpan(app.range, src, '(foo bar baz)')
    assertSpan(asExp(app.head, 'id').range, src, 'foo')
    expect(app.args.length).toBe(2)
    assertSpan(asExp(app.args[0], 'id').range, src, 'bar')
    assertSpan(asExp(app.args[1], 'id').range, src, 'baz')
  })
  test('nullary application', () => {
    const src = '(foo)'
    const app = asExp(parseExp(src), 'app')
    assertSpan(app.range, src, '(foo)')
    assertSpan(asExp(app.head, 'id').range, src, 'foo')
    expect(app.args.length).toBe(0)
  })
})

describe('lambda', () => {
  test('parameter and body ranges', () => {
    const src = '(lambda (x y) x)'
    const lam = asExp(parseExp(src), 'lam')
    assertSpan(lam.range, src, '(lambda (x y) x)')
    expect(lam.params.map((p) => p.name)).toEqual(['x', 'y'])
    assertSpan(lam.params[0].range, src, 'x', 0)
    assertSpan(lam.params[1].range, src, 'y', 0)
    expect(lam.restParam).toBeUndefined()
    assertSpan(asExp(lam.body, 'id').range, src, 'x', 1)
  })
  test('rest parameter', () => {
    const src = '(lambda (x . rest) rest)'
    const lam = asExp(parseExp(src), 'lam')
    expect(lam.params.map((p) => p.name)).toEqual(['x'])
    assertSpan(lam.params[0].range, src, 'x')
    const rest = lam.restParam
    expect(rest?.name).toBe('rest')
    if (rest === undefined) return
    assertSpan(rest.range, src, 'rest', 0)
    assertSpan(asExp(lam.body, 'id').range, src, 'rest', 1)
  })
})

describe('let and let*', () => {
  test('let binding identifiers, values, and body', () => {
    const src = '(let ([x 1] [y 2]) x)'
    const let_ = asExp(parseExp(src), 'let')
    assertSpan(let_.range, src, '(let ([x 1] [y 2]) x)')
    expect(let_.bindings.map((b) => b.id.name)).toEqual(['x', 'y'])
    assertSpan(let_.bindings[0].id.range, src, 'x', 0)
    assertSpan(let_.bindings[0].value.range, src, '1')
    assertSpan(let_.bindings[1].id.range, src, 'y', 0)
    assertSpan(let_.bindings[1].value.range, src, '2')
    assertSpan(asExp(let_.body, 'id').range, src, 'x', 1)
  })
  test('let* binding and body', () => {
    const src = '(let* ([x 1]) x)'
    const lets = asExp(parseExp(src), 'let*')
    assertSpan(lets.range, src, '(let* ([x 1]) x)')
    assertSpan(lets.bindings[0].id.range, src, 'x', 0)
    assertSpan(lets.bindings[0].value.range, src, '1')
    assertSpan(asExp(lets.body, 'id').range, src, 'x', 1)
  })
})

describe('begin', () => {
  test('sub-expression ranges', () => {
    const src = '(begin 1 2 3)'
    const begin = asExp(parseExp(src), 'begin')
    assertSpan(begin.range, src, '(begin 1 2 3)')
    expect(begin.exps.length).toBe(3)
    assertSpan(begin.exps[0].range, src, '1')
    assertSpan(begin.exps[1].range, src, '2')
    assertSpan(begin.exps[2].range, src, '3')
  })
})

describe('if', () => {
  test('guard, then, and else ranges', () => {
    const src = '(if #t 1 2)'
    const if_ = asExp(parseExp(src), 'if')
    assertSpan(if_.range, src, '(if #t 1 2)')
    assertSpan(if_.guard.range, src, '#t')
    assertSpan(if_.ifB.range, src, '1')
    assertSpan(if_.elseB.range, src, '2')
  })
})

describe('match and patterns', () => {
  test('literal and wildcard patterns', () => {
    const src = '(match x [1 "one"] [_ "other"])'
    const m = asExp(parseExp(src), 'match')
    assertSpan(m.range, src, '(match x [1 "one"] [_ "other"])')
    assertSpan(asExp(m.scrutinee, 'id').range, src, 'x')
    expect(m.branches.length).toBe(2)
    assertSpan(asPat(m.branches[0].pat, 'plit').range, src, '1')
    assertSpan(m.branches[0].body.range, src, '"one"')
    assertSpan(asPat(m.branches[1].pat, 'pwild').range, src, '_')
    assertSpan(m.branches[1].body.range, src, '"other"')
  })
  test('variable pattern', () => {
    const src = '(match x [y y])'
    const m = asExp(parseExp(src), 'match')
    const pv = asPat(m.branches[0].pat, 'id')
    expect(pv.name).toBe('y')
    assertSpan(pv.range, src, 'y', 0)
    assertSpan(asExp(m.branches[0].body, 'id').range, src, 'y', 1)
  })
  test('constructor pattern', () => {
    const src = '(match p [(cons hd tl) hd])'
    const m = asExp(parseExp(src), 'match')
    const pc = asPat(m.branches[0].pat, 'pctor')
    assertSpan(pc.range, src, '(cons hd tl)')
    expect(pc.name.name).toBe('cons')
    assertSpan(pc.name.range, src, 'cons')
    expect(pc.args.length).toBe(2)
    assertSpan(asPat(pc.args[0], 'id').range, src, 'hd', 0)
    assertSpan(asPat(pc.args[1], 'id').range, src, 'tl', 0)
    assertSpan(asExp(m.branches[0].body, 'id').range, src, 'hd', 1)
  })
})

describe('quote', () => {
  test('(quote ...) form', () => {
    const src = '(quote abc)'
    assertSpan(asExp(parseExp(src), 'quote').range, src, '(quote abc)')
  })
  test("' shorthand", () => {
    const src = "'abc"
    assertSpan(asExp(parseExp(src), 'quote').range, src, "'abc")
  })
})

describe('js-var', () => {
  test('range and name', () => {
    const src = '(js-var "Math.PI")'
    const jsv = asExp(parseExp(src), 'jsvar')
    expect(jsv.name).toBe('Math.PI')
    assertSpan(jsv.range, src, '(js-var "Math.PI")')
  })
})

describe('error', () => {
  test('message expression range', () => {
    const src = '(error "boom")'
    const err = asExp(parseExp(src), 'error')
    assertSpan(err.range, src, '(error "boom")')
    assertSpan(err.exp.range, src, '"boom"')
  })
})

describe('apply', () => {
  test('function and argument ranges', () => {
    const src = '(apply foo bar)'
    const ap = asExp(parseExp(src), 'apply')
    assertSpan(ap.range, src, '(apply foo bar)')
    assertSpan(asExp(ap.fn, 'id').range, src, 'foo')
    assertSpan(asExp(ap.args, 'id').range, src, 'bar')
  })
})

///// Sugared expression forms /////////////////////////////////////////////////

describe('and / or', () => {
  test('and sub-expression ranges', () => {
    const src = '(and #t #f)'
    const a = asExp(parseExp(src), 'and')
    assertSpan(a.range, src, '(and #t #f)')
    assertSpan(a.exps[0].range, src, '#t')
    assertSpan(a.exps[1].range, src, '#f')
  })
  test('or sub-expression ranges', () => {
    const src = '(or #t #f)'
    const o = asExp(parseExp(src), 'or')
    assertSpan(o.range, src, '(or #t #f)')
    assertSpan(o.exps[0].range, src, '#t')
    assertSpan(o.exps[1].range, src, '#f')
  })
})

describe('cond', () => {
  test('branch test and body ranges', () => {
    const src = '(cond [#t 1] [#f 2])'
    const c = asExp(parseExp(src), 'cond')
    assertSpan(c.range, src, '(cond [#t 1] [#f 2])')
    expect(c.branches.length).toBe(2)
    assertSpan(c.branches[0].test.range, src, '#t')
    assertSpan(c.branches[0].body.range, src, '1')
    assertSpan(c.branches[1].test.range, src, '#f')
    assertSpan(c.branches[1].body.range, src, '2')
  })
})

describe('section', () => {
  test('operator and hole ranges', () => {
    const src = '(section + _ 1)'
    const sec = asExp(parseExp(src), 'section')
    assertSpan(sec.range, src, '(section + _ 1)')
    expect(sec.exps.length).toBe(3)
    assertSpan(asExp(sec.exps[0], 'id').range, src, '+')
    assertSpan(asExp(sec.exps[1], 'id').range, src, '_')
    assertSpan(sec.exps[2].range, src, '1')
  })
})

///// Statements ///////////////////////////////////////////////////////////////

describe('statements', () => {
  test('import (builtin)', () => {
    const src = '(import image)'
    const imp = asStmt(parseStmt(src), 'import')
    expect(imp.module).toBe('image')
    expect(imp.kind).toBe('builtin')
    assertSpan(imp.range, src, '(import image)')
  })
  test('import (file)', () => {
    const src = '(import "lib/helpers.scm")'
    const imp = asStmt(parseStmt(src), 'import')
    expect(imp.module).toBe('lib/helpers.scm')
    expect(imp.kind).toBe('file')
    assertSpan(imp.range, src, '(import "lib/helpers.scm")')
  })
  test('define', () => {
    const src = '(define answer 42)'
    const def = asStmt(parseStmt(src), 'define')
    assertSpan(def.range, src, '(define answer 42)')
    expect(def.name.name).toBe('answer')
    assertSpan(def.name.range, src, 'answer')
    assertSpan(def.value.range, src, '42')
  })
  test('display', () => {
    const src = '(display foo)'
    const disp = asStmt(parseStmt(src), 'display')
    assertSpan(disp.range, src, '(display foo)')
    assertSpan(asExp(disp.value, 'id').range, src, 'foo')
  })
  test('bare expression statement matches its expression', () => {
    const src = '(foo bar)'
    const se = asStmt(parseStmt(src), 'stmtexp')
    assertSpan(se.range, src, '(foo bar)')
    assertSpan(se.expr.range, src, '(foo bar)')
  })
  test('struct', () => {
    const src = '(struct posn (x y))'
    const st = asStmt(parseStmt(src), 'struct')
    assertSpan(st.range, src, '(struct posn (x y))')
    expect(st.name.name).toBe('posn')
    assertSpan(st.name.range, src, 'posn')
    expect(st.fields.map((f) => f.name)).toEqual(['x', 'y'])
    assertSpan(st.fields[0].range, src, 'x')
    assertSpan(st.fields[1].range, src, 'y')
  })
})

///// Real-world programs //////////////////////////////////////////////////////

describe('real-world programs', () => {
  test('recursive factorial (multi-line, repeated identifiers)', () => {
    const src = [
      '(define factorial',
      '  (lambda (n)',
      '    (if (equal? n 0)',
      '        1',
      '        (* n (factorial (- n 1))))))',
    ].join('\n')
    const def = asStmt(parseStmt(src), 'define')
    // The whole (single-statement) source is the define.
    expect(def.range.begin.idx).toBe(0)
    expect(def.range.end.idx).toBe(src.length - 1)
    assertSpan(def.name.range, src, 'factorial', 0)

    const lam = asExp(def.value, 'lam')
    assertSpan(lam.params[0].range, src, 'n', 0)
    expect(lam.params[0].range.begin.line).toBe(2)

    const if_ = asExp(lam.body, 'if')
    const guard = asExp(if_.guard, 'app') // (equal? n 0)
    assertSpan(asExp(guard.head, 'id').range, src, 'equal?')
    assertSpan(asExp(guard.args[0], 'id').range, src, 'n', 1)
    assertSpan(guard.args[1].range, src, '0')
    assertSpan(if_.ifB.range, src, '1', 0)

    const elseB = asExp(if_.elseB, 'app') // (* n (factorial (- n 1)))
    assertSpan(asExp(elseB.head, 'id').range, src, '*')
    assertSpan(asExp(elseB.args[0], 'id').range, src, 'n', 2)

    const rec = asExp(elseB.args[1], 'app') // (factorial (- n 1))
    const recHead = asExp(rec.head, 'id')
    expect(recHead.name).toBe('factorial')
    // The recursive call is the 2nd occurrence of `factorial`, on line 5.
    assertSpan(recHead.range, src, 'factorial', 1)
    expect(recHead.range.begin.line).toBe(5)

    const sub = asExp(rec.args[0], 'app') // (- n 1)
    assertSpan(asExp(sub.head, 'id').range, src, '-')
    assertSpan(asExp(sub.args[0], 'id').range, src, 'n', 3)
    assertSpan(sub.args[1].range, src, '1', 1)
  })

  test('struct definition and accessor usage across statements', () => {
    const src = [
      '(struct posn (x y))',
      '(define origin (posn 0 0))',
      '(define ox (posn-x origin))',
    ].join('\n')
    const prog = parse(src)
    expect(prog.length).toBe(3)

    const st = asStmt(prog[0], 'struct')
    assertSpan(st.range, src, '(struct posn (x y))')
    assertSpan(st.name.range, src, 'posn', 0)

    const def1 = asStmt(prog[1], 'define')
    assertSpan(def1.range, src, '(define origin (posn 0 0))')
    expect(def1.range.begin.line).toBe(2)
    assertSpan(def1.name.range, src, 'origin', 0)
    const ctor = asExp(def1.value, 'app')
    assertSpan(asExp(ctor.head, 'id').range, src, 'posn', 1) // constructor use

    const def2 = asStmt(prog[2], 'define')
    expect(def2.range.begin.line).toBe(3)
    const acc = asExp(def2.value, 'app')
    assertSpan(asExp(acc.head, 'id').range, src, 'posn-x')
    assertSpan(asExp(acc.args[0], 'id').range, src, 'origin', 1)
  })

  test('define with a docstring keeps the doc comments out of the define range', () => {
    const src = [
      ';;; (double n) -> number?',
      ';;;   n : number?',
      ';;; Doubles n.',
      '(define double',
      '  (lambda (n)',
      '    (* n 2)))',
    ].join('\n')
    const def = asStmt(parseStmt(src), 'define')
    // The define node starts at `(define` on line 4, not at the first comment.
    assertSpan(def.range, src, '(define double\n  (lambda (n)\n    (* n 2)))')
    expect(def.range.begin.line).toBe(4)
    // `double` appears first in the docstring signature, then as the name.
    assertSpan(def.name.range, src, 'double', 1)

    const docs = def.docComments
    expect(docs?.length).toBe(3)
    if (docs === undefined) return
    assertSpan(docs[0].range, src, ';;; (double n) -> number?')
    assertSpan(docs[1].range, src, ';;;   n : number?')
    assertSpan(docs[2].range, src, ';;; Doubles n.')
  })

  test('multi-statement program: ranges stay absolute across lines', () => {
    const src = ['(define foo 1)', '(define bar 2)', '(display (+ foo bar))'].join(
      '\n',
    )
    const prog = parse(src)
    expect(prog.length).toBe(3)
    assertSpan(prog[0].range, src, '(define foo 1)')
    assertSpan(prog[1].range, src, '(define bar 2)')
    expect(prog[1].range.begin.line).toBe(2)
    assertSpan(prog[2].range, src, '(display (+ foo bar))')
    expect(prog[2].range.begin.line).toBe(3)

    const disp = asStmt(prog[2], 'display')
    const add = asExp(disp.value, 'app')
    assertSpan(asExp(add.head, 'id').range, src, '+')
    assertSpan(asExp(add.args[0], 'id').range, src, 'foo', 1)
    assertSpan(asExp(add.args[1], 'id').range, src, 'bar', 1)
    expect(add.args[0].range.begin.line).toBe(3)
  })

  test('list-processing function combining match and let', () => {
    const src = [
      '(define length',
      '  (lambda (lst)',
      '    (match lst',
      '      [null 0]',
      '      [(cons hd tl)',
      '       (let ([rest (length tl)])',
      '         (+ 1 rest))])))',
    ].join('\n')
    const def = asStmt(parseStmt(src), 'define')
    assertSpan(def.name.range, src, 'length', 0)

    const lam = asExp(def.value, 'lam')
    assertSpan(lam.params[0].range, src, 'lst', 0)

    const m = asExp(lam.body, 'match')
    assertSpan(asExp(m.scrutinee, 'id').range, src, 'lst', 1)
    expect(m.branches.length).toBe(2)

    // [null 0]
    assertSpan(asPat(m.branches[0].pat, 'plit').range, src, 'null')
    assertSpan(m.branches[0].body.range, src, '0')

    // [(cons hd tl) (let ([rest (length tl)]) (+ 1 rest))]
    const pc = asPat(m.branches[1].pat, 'pctor')
    assertSpan(pc.name.range, src, 'cons')
    assertSpan(asPat(pc.args[0], 'id').range, src, 'hd')
    assertSpan(asPat(pc.args[1], 'id').range, src, 'tl', 0)

    const let_ = asExp(m.branches[1].body, 'let')
    assertSpan(let_.bindings[0].id.range, src, 'rest', 0)
    const call = asExp(let_.bindings[0].value, 'app') // (length tl)
    assertSpan(asExp(call.head, 'id').range, src, 'length', 1)
    assertSpan(asExp(call.args[0], 'id').range, src, 'tl', 1)
    expect(call.range.begin.line).toBe(6)
  })
})

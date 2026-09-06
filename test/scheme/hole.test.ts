// The hole `??` (issue #561): an atomic special form that raises when, and
// only when, evaluation reaches it. It is core all the way down -- parser to
// bytecode -- so this covers the whole path in one place.
import { describe, expect, test } from 'vitest'
import * as A from '../../src/scheme/ast.js'
import { expandProgram } from '../../src/scheme/expansion.js'
import { lowerProgram } from '../../src/scheme/codegen.js'
import { reductionTrace, runProgram } from '../harness.js'
import { parse } from './parsing/test-utils'

/** Parse a single bare-expression statement and return its expression. */
function parseExp(src: string): A.Exp {
  const { prog, errors } = parse(src)
  expect(errors, `parse errors for ${JSON.stringify(src)}`).toEqual([])
  const stmt = prog[0]
  if (!A.isStmtExp(stmt)) throw new Error('expected a bare expression')
  return stmt.expr
}

describe('hole parsing', () => {
  test('?? is a hole, not an identifier', () => {
    expect(parseExp('??')).toMatchObject({ tag: 'hole' })
  })

  test('a hole is an ordinary sub-expression', () => {
    const e = parseExp('(+ 1 ??)')
    expect(e.tag).toBe('app')
    if (e.tag !== 'app') return
    expect(e.args.map((a) => a.tag)).toEqual(['lit', 'hole'])
  })

  test('(??) still parses, as an application of a hole', () => {
    // The former spelling, back when ?? was a nullary library function. It
    // keeps working because a head is evaluated before the call is made.
    const e = parseExp('(??)')
    expect(e.tag).toBe('app')
    if (e.tag !== 'app') return
    expect(e.head.tag).toBe('hole')
    expect(e.args).toEqual([])
  })

  test('?? cannot be a binder', () => {
    expect(parse('(define ?? 1)').errors.length).toBeGreaterThan(0)
    expect(parse('(lambda (??) 1)').errors.length).toBeGreaterThan(0)
    expect(parse('(let ([?? 1]) ??)').errors.length).toBeGreaterThan(0)
  })

  test('??? is still an ordinary identifier', () => {
    // Only the two-question-mark spelling is reserved.
    expect(parseExp('???')).toMatchObject({ tag: 'id', name: '???' })
  })

  test('a hole prints as itself', () => {
    expect(A.expToString(parseExp('(+ 1 ??)'))).toBe('(+ 1 ??)')
  })
})

describe('hole compilation', () => {
  test('a hole survives expansion', () => {
    const expanded = expandProgram(parse('(if #t 1 ??)').prog)
    const stmt = expanded[0]
    if (!A.isStmtExp(stmt) || stmt.expr.tag !== 'if') {
      throw new Error('expected an if expression')
    }
    expect(stmt.expr.elseB.tag).toBe('hole')
  })

  test('a hole inside #(...) survives expansion', () => {
    const expanded = expandProgram(parse('#(+ %1 ??)').prog)
    const stmt = expanded[0]
    if (!A.isStmtExp(stmt) || stmt.expr.tag !== 'lam') {
      throw new Error('expected a lambda')
    }
    expect(A.expToString(stmt.expr.body)).toBe('(+ %1 ??)')
  })

  test('a hole lowers to a single hole op carrying its range', () => {
    const stmt = lowerProgram(expandProgram(parse('(+ 1 ??)').prog))[0]
    if (stmt.tag !== 'disp') throw new Error('expected a display statement')
    const holes = stmt.expr.filter((op) => op.tag === 'hole')
    expect(holes.length).toBe(1)
    expect(holes[0].range.toString()).toBe('1:6-1:7')
  })
})

describe('hole evaluation', () => {
  test('reaching a hole raises, naming the hole itself', async () => {
    expect(await runProgram('(+ 1 ??)')).toEqual([
      'Runtime error [1:6-1:7]: Hole encountered in program!',
    ])
  })

  test('a hole is only reached when evaluation gets to it', async () => {
    // The point of the form: an unfinished branch does not break the branches
    // that are finished.
    expect(await runProgram('(if #t 1 ??)')).toEqual(['1'])
    expect(await runProgram('(if #f ?? 2)')).toEqual(['2'])
    expect(await runProgram('(and #f ??)')).toEqual(['#f'])
    expect(await runProgram('(or #t ??)')).toEqual(['#t'])
  })

  test('a hole in an unapplied function body is harmless', async () => {
    expect(
      await runProgram(`
(define unfinished (lambda (x) ??))
(procedure? unfinished)
`),
    ).toEqual(['#t'])
  })

  test('(??) raises the same error', async () => {
    expect(await runProgram('(??)')).toEqual([
      'Runtime error [1:2-1:3]: Hole encountered in program!',
    ])
  })

  test('a hole reached from inside a library function still names the hole', async () => {
    // HoleHandler falls back to the call site for a frame running library
    // code. A lambda the student wrote is user code wherever `map` applies it,
    // so the range stays the hole's own -- which is what makes the fallback
    // invisible rather than wrong.
    expect(await runProgram('(map (lambda (x) ??) (list 1 2))')).toEqual([
      'Runtime error [1:18-1:19]: Hole encountered in program!',
    ])
  })

  test('a hole stands in any expression position', async () => {
    expect(await runProgram('(match 1 [_ ??])')).toEqual([
      'Runtime error [1:13-1:14]: Hole encountered in program!',
    ])
    expect(await runProgram('(cond [#t ??])')).toEqual([
      'Runtime error [1:11-1:12]: Hole encountered in program!',
    ])
    expect(await runProgram('[1 ??]')).toEqual([
      'Runtime error [1:4-1:5]: Hole encountered in program!',
    ])
    expect(await runProgram('{"k" ??}')).toEqual([
      'Runtime error [1:6-1:7]: Hole encountered in program!',
    ])
  })

  test('with-handler catches a hole', async () => {
    expect(
      await runProgram('(with-handler (lambda (msg) msg) (lambda () ??))'),
    ).toEqual(['"Hole encountered in program!"'])
  })

  test('a hole aborts only its own statement', async () => {
    expect(
      await runProgram(`
??
(+ 1 2)
`),
    ).toEqual(['Runtime error [1:1-1:2]: Hole encountered in program!', '3'])
  })
})

describe('hole tracing', () => {
  // raise.ts reconstructs an expression from the ops still to run; its switch
  // is not exhaustiveness-checked, so a missing `hole` case would show up only
  // here.
  test('a not-yet-reached hole shows as ?? in a reduction step', async () => {
    expect(await reductionTrace('(+ (* 2 3) ??)')).toEqual([
      '(+ (* 2 3) ??)',
      '(+ 6 ??)',
      'Runtime error [1:12-1:13]: Hole encountered in program!',
    ])
  })

  test('a hole reached through a call reports where it was written', async () => {
    expect(
      await reductionTrace('(define f (lambda (x) ??))\n(f 1)'),
    ).toEqual([
      '(f 1)',
      '??',
      'Runtime error [1:23-1:24]: Hole encountered in program!',
    ])
  })

  test('a trace never reaches a hole in an untaken branch', async () => {
    expect(await reductionTrace('(if #t 1 ??)')).toEqual(['(if #t 1 ??)', '1'])
  })
})

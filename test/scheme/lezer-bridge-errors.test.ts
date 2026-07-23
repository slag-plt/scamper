import { describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'

// N.B., unlike lezer-bridge.test.ts/lezer-bridge-corpus.test.ts, these don't
// assert parity with reader.ts/parser.ts's specific error messages -- Lezer's
// error-recovery tree doesn't tell us *why* a span failed to parse the way
// the hand-written parser's per-form arity checks do, so the messages here
// are deliberately generic. What matters is: no crash, at least one error is
// reported, and parsing keeps going for the rest of the program.
function parse(src: string) {
  const errors: L.ScamperError[] = []
  const prog = parseProgramFromSource(errors, src)
  return { prog, errors }
}

describe('lezer-bridge error recovery', () => {
  test('reserved word used as a variable name', () => {
    const { prog, errors } = parse('(define and 5)')
    expect(errors.length).toBeGreaterThan(0)
    expect(prog.length).toBe(1)
  })

  test('incomplete lambda', () => {
    const { errors } = parse('(lambda)')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('if with a missing branch', () => {
    const { errors } = parse('(if 1 2)')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('unterminated display', () => {
    const { errors } = parse('(display')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('stray extra closing paren', () => {
    const { prog, errors } = parse('(display 1))')
    expect(errors.length).toBeGreaterThan(0)
    // N.B., the valid (display 1) statement before the stray ")" still
    // parses -- error recovery shouldn't poison the whole program.
    expect(prog.length).toBe(1)
    expect(prog[0].tag).toBe('display')
  })

  test("a malformed statement doesn't poison surrounding valid statements", () => {
    const { prog, errors } = parse(
      '(define x 1) (define and 5) (define y 2)',
    )
    expect(errors.length).toBeGreaterThan(0)
    expect(prog.length).toBe(3)
    expect(prog[0].tag).toBe('define')
    expect((prog[0] as { name: string }).name).toBe('x')
    expect(prog[2].tag).toBe('define')
    expect((prog[2] as { name: string }).name).toBe('y')
  })

  test("error localized inside a nested expression doesn't discard the outer statement", () => {
    const { prog, errors } = parse('(display (lambda))')
    expect(errors.length).toBeGreaterThan(0)
    expect(prog.length).toBe(1)
    expect(prog[0].tag).toBe('display')
  })

  test('malformed match branch pattern', () => {
    const { errors } = parse('(match x [and 1])')
    expect(errors.length).toBeGreaterThan(0)
  })
})

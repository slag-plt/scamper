import { describe, expect, test } from 'vitest'
import {
  enclosingCallAt,
  identifierAt,
  identifierOccurrences,
} from '../../src/scheme/token'

describe('identifierAt', () => {
  test('finds an identifier in application-head position', () => {
    expect(identifierAt('(car (list 1 2))', 1)).toEqual({
      name: 'car',
      from: 1,
      to: 4,
    })
  })

  test('finds an identifier when hovering its middle', () => {
    expect(identifierAt('(map f xs)', 2)?.name).toBe('map')
  })

  test('finds symbolic identifiers', () => {
    expect(identifierAt('(+ 1 2)', 1)?.name).toBe('+')
    expect(identifierAt('(null? xs)', 1)?.name).toBe('null?')
  })

  test('works on a syntactically incomplete buffer', () => {
    // Unclosed application -- tokenizeAndParse would yield no program here,
    // but the error-tolerant Lezer tree still locates the identifier.
    expect(identifierAt('(map f', 1)?.name).toBe('map')
  })

  test('returns undefined off any identifier', () => {
    expect(identifierAt('(car 42)', 0)).toBeUndefined() // the '(' bracket
    expect(identifierAt('(car 42)', 5)).toBeUndefined() // the number 42
    expect(identifierAt('  ', 1)).toBeUndefined() // whitespace
  })

  test('does not match reserved words (specialized grammar nodes)', () => {
    expect(identifierAt('(define x 1)', 1)).toBeUndefined() // 'define'
    expect(identifierAt('(lambda (x) x)', 1)).toBeUndefined() // 'lambda'
  })
})

describe('enclosingCallAt', () => {
  test('finds the callee and the active argument slot', () => {
    const src = '(map f lst)'
    expect(enclosingCallAt(src, src.indexOf('f'))).toEqual({
      name: 'map',
      activeParam: 0,
    })
    expect(enclosingCallAt(src, src.indexOf('lst'))).toEqual({
      name: 'map',
      activeParam: 1,
    })
  })

  test('reports the next slot once an argument is complete', () => {
    // Cursor just before the close paren, after "f " -> second slot.
    expect(enclosingCallAt('(cons f )', 8)).toEqual({
      name: 'cons',
      activeParam: 1,
    })
  })

  test('uses the innermost call for nested applications', () => {
    const src = '(map (f x) lst)'
    expect(enclosingCallAt(src, src.indexOf('x'))).toEqual({
      name: 'f',
      activeParam: 0,
    })
  })

  test('works on an unclosed call', () => {
    expect(enclosingCallAt('(map f', 5)?.name).toBe('map')
  })

  test('returns undefined outside any call', () => {
    expect(enclosingCallAt('42', 1)).toBeUndefined()
    expect(enclosingCallAt('x', 0)).toBeUndefined()
  })
})

describe('identifierOccurrences', () => {
  test('finds every occurrence of a name', () => {
    const occ = identifierOccurrences('(+ x (f x x))', 'x')
    expect(occ.length).toBe(3)
    expect(occ.every((o) => o.name === 'x')).toBe(true)
  })

  test('matches whole tokens only, not substrings', () => {
    // `xs` is a different token from `x`.
    expect(identifierOccurrences('(xs x)', 'x').length).toBe(1)
  })
})

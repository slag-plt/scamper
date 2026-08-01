import { describe, expect, test } from 'vitest'
import { identifierAt } from '../../src/scheme/token'

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

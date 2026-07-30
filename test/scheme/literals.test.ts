import { describe, expect, test } from 'vitest'
import {
  looksLikeIdentifier,
  parseCharLiteral,
  parseNumberLiteral,
  parseStringLiteral,
} from '../../src/scheme/literals.js'
import * as L from '../../src/lpm/index.js'
import { anyRange } from './util.js'

describe('parseNumberLiteral', () => {
  test('parses positive integers', () => {
    expect(parseNumberLiteral('42')).toBe(42)
  })

  test('parses negative integers', () => {
    expect(parseNumberLiteral('-17')).toBe(-17)
  })

  test('parses floats', () => {
    expect(parseNumberLiteral('3.14')).toBe(3.14)
  })

  test('parses exponent notation', () => {
    expect(parseNumberLiteral('2e3')).toBe(2000)
  })
})

describe('parseStringLiteral', () => {
  test('throws an ICE for empty text (no quote at all)', () => {
    expect(() => parseStringLiteral('', anyRange)).toThrow(L.ICE)
  })

  test('throws when text does not begin with a quote', () => {
    expect(() => parseStringLiteral('abc"', anyRange)).toThrow(
      'String literal must begin with a quote',
    )
  })

  test('parses a plain string with no escapes', () => {
    expect(parseStringLiteral('"hello world"', anyRange)).toBe('hello world')
  })

  test('resolves every named escape sequence', () => {
    const namedEscapes: [string, string][] = [
      ['a', String.fromCharCode(7)], // alarm
      ['b', String.fromCharCode(8)], // backspace
      ['t', String.fromCharCode(9)], // tab
      ['n', String.fromCharCode(10)], // newline
      ['v', String.fromCharCode(11)], // vertical tab
      ['f', String.fromCharCode(12)], // form feed
      ['r', String.fromCharCode(13)], // carriage return
      ['e', String.fromCharCode(27)], // escape
      ['"', '"'],
      ["'", "'"],
      ['\\', '\\'],
    ]
    for (const [esc, expected] of namedEscapes) {
      expect(parseStringLiteral(`"x\\${esc}y"`, anyRange)).toBe(
        `x${expected}y`,
      )
    }
  })

  test('rejects octal, hex, and unicode escape codes with distinct messages', () => {
    const rejected: [string, string][] = [
      ['0', 'Octal escape codes not supported'],
      ['x', 'Hex escape codes not supported'],
      ['u', 'Unicode escape codes not supported'],
      ['U', 'Unicode escape codes not supported'],
    ]
    for (const [esc, message] of rejected) {
      expect(() => parseStringLiteral(`"\\${esc}"`, anyRange)).toThrow(
        message,
      )
    }
  })

  test('throws when a backslash occurs at the end of the string', () => {
    expect(() => parseStringLiteral('"abc\\', anyRange)).toThrow(
      'Escape character "\\" cannot occur at the end of a string.',
    )
  })

  test('skips a newline immediately following a backslash (line continuation)', () => {
    expect(parseStringLiteral('"a\\\nb"', anyRange)).toBe('ab')
  })

  test('falls back to the identity escape for any other character', () => {
    expect(parseStringLiteral('"\\q"', anyRange)).toBe('q')
  })
})

describe('parseCharLiteral', () => {
  test('parses a plain single-character literal', () => {
    expect(parseCharLiteral('#\\a', anyRange)).toEqual(L.mkChar('a'))
  })

  test('parses a named character literal', () => {
    expect(parseCharLiteral('#\\space', anyRange)).toEqual(L.mkChar(' '))
  })

  test('throws on an invalid character literal', () => {
    expect(() => parseCharLiteral('#\\bogus', anyRange)).toThrow(
      'Invalid character literal: #\\bogus',
    )
  })
})

describe('looksLikeIdentifier', () => {
  test('true for a plain identifier', () => {
    expect(looksLikeIdentifier('foo')).toBe(true)
  })

  test('false for text that looks like a number, boolean, null, string, or char literal', () => {
    const disqualifying = ['42', '-3.14', '#t', '#f', 'null', '"hi"', '#\\a']
    for (const text of disqualifying) {
      expect(looksLikeIdentifier(text)).toBe(false)
    }
  })

  test('true for punctuation-heavy identifiers with hyphens or question marks', () => {
    const identifiers = ['is-even?', 'list->vector', 'foo-bar-baz']
    for (const text of identifiers) {
      expect(looksLikeIdentifier(text)).toBe(true)
    }
  })
})

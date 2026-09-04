import { describe, expect, test } from 'vitest'
import { ICE, ScamperError, Value } from '../../src/lpm'
import * as U from '../../src/lpm/util'
import { parseStringLiteral } from '../../src/scheme/literals'
import { anyRange } from '../scheme/util'

describe('listNth', () => {
  test('returns the element at a valid index', () => {
    expect(U.listNth(0, U.mkList(10, 20, 30))).toBe(10)
    expect(U.listNth(2, U.mkList(10, 20, 30))).toBe(30)
  })

  test('throws on a negative index', () => {
    expect(() => U.listNth(-1, U.mkList(1, 2, 3))).toThrow(ScamperError)
  })

  test('throws when the index is out of bounds', () => {
    expect(() => U.listNth(5, U.mkList(1, 2, 3))).toThrow(ScamperError)
  })

  test('throws when indexing into an empty list', () => {
    expect(() => U.listNth(0, null)).toThrow(ScamperError)
  })
})

describe('equals', () => {
  test('equal and unequal primitives via the strict-equality fast path', () => {
    expect(U.equals(5, 5)).toBe(true)
    expect(U.equals(5, 6)).toBe(false)
    expect(U.equals('hi', 'hi')).toBe(true)
  })

  test('equal and unequal arrays, including different-length arrays', () => {
    expect(U.equals([1, 2, 3], [1, 2, 3])).toBe(true)
    expect(U.equals([1, 2], [1, 99])).toBe(false)
    expect(U.equals([1, 2], [1, 2, 3])).toBe(false)
  })

  test('equal chars vs unequal chars', () => {
    expect(U.equals(U.mkChar('a'), U.mkChar('a'))).toBe(true)
    expect(U.equals(U.mkChar('a'), U.mkChar('b'))).toBe(false)
  })

  test('structs of the same kind with equal fields are equal', () => {
    const a = U.mkStruct('point', ['x', 'y'], [1, 2])
    const b = U.mkStruct('point', ['x', 'y'], [1, 2])
    expect(U.equals(a, b)).toBe(true)
  })

  test('structs of different kinds are unequal even with identical field values', () => {
    const a = U.mkStruct('point', ['x', 'y'], [1, 2])
    const b = U.mkStruct('vector2', ['x', 'y'], [1, 2])
    expect(U.equals(a, b)).toBe(false)
  })

  test('structs of the same kind with an unequal field are unequal', () => {
    const a = U.mkStruct('point', ['x', 'y'], [1, 2])
    const b = U.mkStruct('point', ['x', 'y'], [1, 99])
    expect(U.equals(a, b)).toBe(false)
  })

  test('nested structural equality: a struct containing a list containing another struct', () => {
    const innerA = U.mkStruct('point', ['x', 'y'], [1, 2])
    const innerB = U.mkStruct('point', ['x', 'y'], [1, 2])
    const outerA = U.mkStruct('box', ['items'], [U.mkList(innerA)])
    const outerB = U.mkStruct('box', ['items'], [U.mkList(innerB)])
    expect(U.equals(outerA, outerB)).toBe(true)
  })

  test('equal and unequal pairs (compared as structs)', () => {
    expect(U.equals(U.mkPair(1, 2), U.mkPair(1, 2))).toBe(true)
    expect(U.equals(U.mkPair(1, 2), U.mkPair(1, 99))).toBe(false)
  })

  test('equal lists, unequal lists, and the empty list', () => {
    expect(U.equals(U.mkList(1, 2, 3), U.mkList(1, 2, 3))).toBe(true)
    expect(U.equals(U.mkList(1, 2), U.mkList(1, 2, 3))).toBe(false)
    expect(U.equals(U.mkList(), U.mkList())).toBe(true) // both null
  })

  test('nested vectors compare element-wise', () => {
    expect(U.equals([[1, 2], [3]], [[1, 2], [3]])).toBe(true)
    expect(U.equals([[1, 2], [3]], [[1, 2], [4]])).toBe(false)
  })

  test('symbols compare by identity, not by value', () => {
    // A map (plain object) compares structurally: same keys bound to equal
    // values, regardless of key order.
    expect(U.equals({ a: 1 }, { a: 1 })).toBe(true)
    expect(U.equals({ a: 1, b: 2 }, { b: 2, a: 1 })).toBe(true)
    expect(U.equals({ a: 1 }, { a: 2 })).toBe(false)
    expect(U.equals({ a: 1 }, { a: 1, b: 2 })).toBe(false)
    expect(U.equals({ a: [1, 2] }, { a: [1, 2] })).toBe(true)
  })

  test('falls through to false when comparing two completely different value kinds', () => {
    const s = U.mkStruct('point', ['x', 'y'], [1, 2])
    expect(U.equals(s, 'point')).toBe(false)
    // A struct is a tagged object, so it is never equal to a plain map.
    expect(U.equals(s, { x: 1, y: 2 })).toBe(false)
  })
})

describe('typeOf', () => {
  function namedFn(x: number): number {
    return x * 2
  }

  const cases: [string, Value, string][] = [
    ['number', 42, 'number'],
    ['boolean', true, 'boolean'],
    ['string', 'hi', 'string'],
    ['null', null, 'null'],
    ['void', undefined, 'void'],
    ['vector', [1, 2, 3], 'vector'],
    ['JS function', namedFn, '[Function: namedFn]'],
    [
      'closure',
      U.mkClosure([], [], [], () => null, 'add-one'),
      '[Function: add-one]',
    ],
    ['char', U.mkChar('a'), 'char'],
    ['map', { a: 1 }, 'object'],
    ['pair', U.mkPair(1, 2), 'pair'],
    ['list', U.mkList(1, 2), 'list'],
    ['struct', U.mkStruct('point', ['x', 'y'], [1, 2]), '[Struct: point]'],
  ]

  test.for(cases)('%s', ([, v, expected]) => {
    expect(U.typeOf(v)).toBe(expected)
  })
})

describe('toString', () => {
  function namedFn(x: number): number {
    return x * 3
  }

  const cases: [string, Value, string][] = [
    ['boolean true', true, '#t'],
    ['boolean false', false, '#f'],
    ['number', 42, '42'],
    ['string', 'hi', '"hi"'],
    ['void', undefined, 'void'],
    ['null', null, 'null'],
    ['empty map', {}, '{}'],
    ['map', { a: 1, b: 'hi' }, '{ "a" : 1, "b" : "hi" }'],
    ['empty vector', [], '(vector)'],
    ['vector', [1, 2], '(vector 1 2)'],
    [
      'closure',
      U.mkClosure([], [], [], () => null, 'add-one'),
      '[Function: add-one]',
    ],
    ['JS function', namedFn, '[Function: namedFn]'],
    ['char, unnamed', U.mkChar('a'), '#\\a'],
    ['char, named', U.mkChar(' '), '#\\space'],
    ['list', U.mkList(1, 2), '(list 1 2)'],
    ['pair', U.mkPair(1, 2), '(pair 1 2)'],
    ['struct with fields', U.mkStruct('point', ['x', 'y'], [1, 2]), '(point 1 2)'],
    ['struct with no fields', U.mkStruct('unit', [], []), '(unit)'],
  ]

  test.for(cases)('%s', ([, v, expected]) => {
    expect(U.toString(v)).toBe(expected)
  })

  test('HTMLElement, ScamperError, ICE, and generic Error values delegate to their own toString', () => {
    const el = document.createElement('div')
    expect(U.toString(el)).toBe('[HTMLElement]')

    const scamperErr = new ScamperError('Runtime', 'boom')
    expect(U.toString(scamperErr)).toBe(scamperErr.toString())

    const ice = new ICE('someFn', 'unreachable')
    expect(U.toString(ice)).toBe(ice.toString())

    const jsErr = new Error('oops')
    expect(U.toString(jsErr)).toBe(jsErr.toString())
  })

  test('falls back to a Blob representation for an unrecognized value', () => {
    // A plain object is a map, so the fallback needs something that is neither
    // a map nor any other known kind -- here, a class instance.
    const v = new (class {
      foo = 1
    })()
    expect(U.toString(v)).toBe(`[Blob: ${JSON.stringify(v)}]`)
  })
})

describe('escapeStringLiteral', () => {
  // Every escape `parseStringLiteral` reads, as (character, escape) pairs.
  const escapes: [string, string, string][] = [
    ['backslash', '\\', '\\\\'],
    ['double quote', '"', '\\"'],
    ['alarm', '\x07', '\\a'],
    ['backspace', '\b', '\\b'],
    ['tab', '\t', '\\t'],
    ['newline', '\n', '\\n'],
    ['vertical tab', '\v', '\\v'],
    ['form feed', '\f', '\\f'],
    ['carriage return', '\r', '\\r'],
    ['escape', '\x1b', '\\e'],
  ]

  test.for(escapes)('escapes a string containing one %s', ([, c, esc]) => {
    expect(U.escapeStringLiteral(`a${c}b`)).toBe(`a${esc}b`)
  })

  test('leaves the empty string alone', () => {
    expect(U.escapeStringLiteral('')).toBe('')
  })

  test('leaves an ordinary string alone', () => {
    expect(U.escapeStringLiteral('hello world')).toBe('hello world')
  })

  test('escapes a string of nothing but escapable characters', () => {
    const chars = escapes.map(([, c]) => c).join('')
    const expected = escapes.map(([, , esc]) => esc).join('')
    expect(U.escapeStringLiteral(chars)).toBe(expected)
  })

  test('escapes several escapable characters in one string', () => {
    expect(U.escapeStringLiteral('a\nb\tc"d\\e')).toBe('a\\nb\\tc\\"d\\\\e')
  })

  test('never renders a line break, so a literal stays on one line', () => {
    expect(U.escapeStringLiteral('a\nb\r\nc')).not.toMatch(/[\n\r]/)
  })
})

describe('escapeStringLiteral round-trips through parseStringLiteral', () => {
  const roundTrip = (s: string): string =>
    parseStringLiteral(`"${U.escapeStringLiteral(s)}"`, anyRange)

  const escapable = ['\\', '"', '\x07', '\b', '\t', '\n', '\v', '\f', '\r', '\x1b']

  const cases: [string, string][] = [
    ['the empty string', ''],
    ['a plain string', 'hello world'],
    ['a string of only escapable characters', escapable.join('')],
    ['a string mixing escapes and text', 'a\nb\tc"d\\e'],
    ['a quote-heavy string', '"" \\" ""'],
  ]

  test.for(cases)('%s', ([, s]) => {
    expect(roundTrip(s)).toBe(s)
  })

  // Property: escaping then parsing is the identity, checked over each
  // escapable character alone and over every ordered pair of them.
  test('holds for each escapable character and every pair of them', () => {
    for (const c of escapable) {
      expect(roundTrip(`x${c}y`)).toBe(`x${c}y`)
      for (const d of escapable) {
        expect(roundTrip(`x${c}y${d}z`)).toBe(`x${c}y${d}z`)
      }
    }
  })
})

import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { callFunction } from '../sem.js'
import { emptyLibrary, Library, registerValue, ScamperError, Value } from '../lang.js'
import * as Display from '../display.js'

const Prelude: Library = emptyLibrary()

const query1C = (name: string) => contract(name, [C.any])
const query2C = (name: string) => contract(name, [C.any, C.any])

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

function equalQ (x: any, y: any): boolean {
  checkContract(arguments, query2C('equal?'))
  return Value.equal(x, y)
}
registerValue('equal?', equalQ, Prelude)

// Numbers (6.2)

function numberQ (x: any): boolean {
  checkContract(arguments, query1C('number?'))
  return typeof x === 'number'
}
registerValue('number?', numberQ, Prelude)

function realQ (x: any): boolean {
  checkContract(arguments, query1C('real?'))
  return typeof x === 'number' && !Number.isInteger(x)
}
registerValue('real?', realQ, Prelude)

function integerQ (x: any): boolean {
  checkContract(arguments, query1C('integer?'))
  return typeof x === 'number' && Number.isInteger(x)
}
registerValue('integer?', integerQ, Prelude)

// N.B., we don't implement the following functions:
//   (complex? obj)
//   (rational? obj)
//   (exact? z)
//   (inexact? z)
//   (exact->integer? z)
//   (finite? z)
//   (infinite? z)

// Because we only implement the subset of numbers corresponding to the
// Javascript numeric stack: number -> real -> integer

function nanQ (x: any): boolean {
  checkContract(arguments, query1C('nan?'))
  return Number.isNaN(x)
}
registerValue('nan?', nanQ, Prelude)

function lt (x: number, y: number): boolean {
  checkContract(arguments, contract('<', [C.number, C.number]))
  return x < y
}
registerValue('<', lt, Prelude)

function leq (x: number, y: number): boolean {
  checkContract(arguments, contract('<=', [C.number, C.number]))
  return x <= y
}
registerValue('<=', leq, Prelude)

function gt (x: number, y: number): boolean {
  checkContract(arguments, contract('>', [C.number, C.number]))
  return x > y
}
registerValue('>', gt, Prelude)

function geq (x: number, y: number): boolean {
  checkContract(arguments, contract('>=', [C.number, C.number]))
  return x >= y
}
registerValue('>=', geq, Prelude)

function eq (x: number, y: number): boolean {
  checkContract(arguments, contract('=', [C.number, C.number]))
  return x === y
}
registerValue('=', eq, Prelude)

function zeroQ (x: number): boolean {
  checkContract(arguments, contract('zero?', [C.number]))
  return x === 0
}
registerValue('zero?', zeroQ, Prelude)

function positiveQ (x: number): boolean {
  checkContract(arguments, contract('positive?', [C.number]))
  return x > 0
}
registerValue('positive?', positiveQ, Prelude)

function negativeQ (x: number): boolean {
  checkContract(arguments, contract('negative?', [C.number]))
  return x < 0
}
registerValue('negative?', negativeQ, Prelude)

function oddQ (x: number): boolean {
  checkContract(arguments, contract('odd?', [C.integer]))
  return (x & 1) === 1
}
registerValue('odd?', oddQ, Prelude)

function evenQ (x: number): boolean {
  checkContract(arguments, contract('even?', [C.integer]))
  return (x & 1) !== 1
}
registerValue('even?', evenQ, Prelude)

function max (...xs: number[]): number {
  checkContract(arguments, contract('max', [], C.number))
  return Math.max(...xs)
}
registerValue('max', max, Prelude)

function min (...xs: number[]): number {
  checkContract(arguments, contract('min', [], C.number))
  return Math.min(...xs)
}
registerValue('min', min, Prelude)

function plus (...xs: number[]): number {
  checkContract(arguments, contract('+', [], C.number))
  return xs.reduce((a, b) => a + b, 0)
}
registerValue('+', plus, Prelude)

function minus (...xs: number[]): number {
  checkContract(arguments, contract('-', [C.number], C.number))
  return xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
}
registerValue('-', minus, Prelude)

function times (...xs: number[]): number {
  checkContract(arguments, contract('*', [], C.number))
  return xs.reduce((a, b) => a * b, 1)
}
registerValue('*', times, Prelude)

function div (...xs: number[]): number {
  checkContract(arguments, contract('/', [C.number], C.number))
  return xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
}
registerValue('/', div, Prelude)

function abs (x: number): number {
  checkContract(arguments, contract('abs', [C.number]))
  return Math.abs(x)
}
registerValue('abs', abs, Prelude)

// N.B., not implementing the composite division functions:
//   (floor / n1 n2)
//   (floor-quotient n1 n2)
//   (floor-remainder n1 n2)
//   (truncate/ n1 n2)
//   (truncate-quotient n1 n2)
//   (truncate-remainder n1 n2)
// To avoid clutter in the documentation.

function quotient (x: number, y: number): number {
  checkContract(arguments, contract('quotient', [C.integer, C.integer]))
  return Math.trunc(x / y)
}
registerValue('quotient', quotient, Prelude)

function remainder (x: number, y: number): number {
  checkContract(arguments, contract('remainder', [C.integer, C.integer]))
  return x % y
}
registerValue('remainder', remainder, Prelude)

function modulo (x: number, y: number): number {
  checkContract(arguments, contract('modulo', [C.integer, C.integer]))
  return ((x % y) + y) % y
}
registerValue('modulo', modulo, Prelude)

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)

// N.B., we don't implement:
//   (numerator q)
//   (denominator q)
// Since we don't implement rationals.

function floor (x: number): number {
  checkContract(arguments, contract('floor', [C.number]))
  return Math.floor(x)
}
registerValue('floor', floor, Prelude)

function ceiling (x: number): number {
  checkContract(arguments, contract('ceiling', [C.number]))
  return Math.ceil(x)
}
registerValue('ceiling', ceiling, Prelude)

function truncate (x: number): number {
  checkContract(arguments, contract('truncate', [C.number]))
  return Math.trunc(x)
}
registerValue('truncate', truncate, Prelude)

function round (x: number): number {
  checkContract(arguments, contract('round', [C.number]))
  return Math.round(x)
}
registerValue('round', round, Prelude)

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

function square (x: number): number {
  checkContract(arguments, contract('square', [C.number]))
  return x * x
}
registerValue('square', square, Prelude)

function sqrt (x: number): number {
  checkContract(arguments, contract('sqrt', [C.number]))
  return Math.sqrt(x)
}
registerValue('sqrt', sqrt, Prelude)

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

function expt (x: number, y: number): number {
  checkContract(arguments, contract('expt', [C.number, C.number]))
  return Math.pow(x, y)
}
registerValue('expt', expt, Prelude)

// N.B., we don't implement:
//   (make-rectangular x1 x2)   ...probably not!
//   (make-polar x3 x4)         ...probably not!
//   (real-part z)              ...probably not!
//   (imag-part z)              ...probably not!
//   (magnitude z)              ...probably not!
//   (angle z)                  ...probably not!
// Because we don't implement complex numbers.

function numberToString (x: number): string {
  checkContract(arguments, contract('number->string', [C.number]))
  return x.toString()
}
registerValue('number->string', numberToString, Prelude)

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

function stringToNumber (s: string): number {
  checkContract(arguments, contract('string->number', [C.string]))
  if (/^[+-]?\d+$/.test(s)) {
    return parseInt(s)
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return parseFloat(s)
  } else {
    throw new Error(`Runtime error: string->number: invalid string: ${s}`)
  }
}
registerValue('string->number', stringToNumber, Prelude)

// Additional functions from racket/base

function exp (x: number): number {
  checkContract(arguments, contract('exp', [C.number]))
  return Math.exp(x)
}
registerValue('exp', exp, Prelude)

function log (x: number): number {
  checkContract(arguments, contract('log', [C.number]))
  return Math.log(x)
}
registerValue('log', log, Prelude)

function sin (x: number): number {
  checkContract(arguments, contract('sin', [C.number]))
  return Math.sin(x)
}
registerValue('sin', sin, Prelude)

function cos (x: number): number {
  checkContract(arguments, contract('cos', [C.number]))
  return Math.cos(x)
}
registerValue('cos', cos, Prelude)

function tan (x: number): number {
  checkContract(arguments, contract('tan', [C.number]))
  return Math.tan(x)
}
registerValue('tan', tan, Prelude)

function asin (x: number): number {
  checkContract(arguments, contract('asin', [C.number]))
  return Math.asin(x)
}
registerValue('asin', asin, Prelude)

function acos (x: number): number {
  checkContract(arguments, contract('acos', [C.number]))
  return Math.acos(x)
}
registerValue('acos', acos, Prelude)

function atan (x: number): number {
  checkContract(arguments, contract('atan', [C.number]))
  return Math.atan(x)
}
registerValue('atan', atan, Prelude)

function equalsEps (eps: number): Value.T {
  checkContract(arguments, contract('=-eps', [C.number]))
  const name = `=-(${eps})`
  const ret = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-(${eps})`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  }
  Value.nameFn(name, ret)
  return ret
}
registerValue('=-eps', equalsEps, Prelude)

// Booleans (6.3)

function not (x: boolean): boolean {
  checkContract(arguments, contract('not', [C.boolean]))
  return !x
}
registerValue('not', not, Prelude)

function booleanQ (x: any): boolean {
  checkContract(arguments, contract('boolean?', [C.any]))
  return typeof x === 'boolean'
}
registerValue('boolean?', booleanQ, Prelude)

// From racket/base

function nand (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nand', [], C.boolean))
  return !xs.reduce((a, b) => a && b, true)
}
registerValue('nand', nand, Prelude)

function nor (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nor', [], C.boolean))
  return !xs.reduce((a, b) => a || b, false)
}
registerValue('nor', nor, Prelude)

function implies (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('implies', [C.boolean, C.boolean]))
  return !x || y
}
registerValue('implies', implies, Prelude)

function xor (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('xor', [C.boolean, C.boolean]))
  return (x && !y) || (!x && y)
}
registerValue('xor', xor, Prelude)

// Pairs and Lists (6.4)

// TODO: really: the pair/list values we manipulated are _quoted_ pairs/lists.
// The unquoted pair/list form is always evaluated by Scheme immediately, so
// they only exist temporarily. Since our old infrastructure tried to
// distinguish between pairs and lists, we have some architectual changes to
// them to support this behavior.
//
// We should introduce "quoted" helpers for pairs, lists, and vectors so that
// the standard library can use them throughout.

function pairQ (x: any): boolean {
  checkContract(arguments, contract('pair?', [C.any]))
  return Value.isPair(x)
}
registerValue('pair?', pairQ, Prelude)

function cons (x: any, y: any): Value.T {
  checkContract(arguments, contract('cons', [C.any, C.any]))
  return Value.mkPair(x, y)
}
registerValue('cons', cons, Prelude)

function pair (x: any, y: any): Value.T {
  checkContract(arguments, contract('pair', [C.any, C.any]))
  return Value.mkPair(x, y)
}
registerValue('pair', pair, Prelude)

function car (x: Value.T): Value.T {
  checkContract(arguments, contract('car', [C.pair]))
  return (x as any).fst
}
registerValue('car', car, Prelude)

function cdr (x: Value.T): Value.T {
  checkContract(arguments, contract('cdr', [C.pair]))
  return (x as any).snd
}
registerValue('cdr', cdr, Prelude)

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

function nullQ (x: any): boolean {
  checkContract(arguments, contract('null?', [C.any]))
  return x === null
}
registerValue('null?', nullQ, Prelude)

function listQ (x: any): boolean {
  checkContract(arguments, contract('list?', [C.any]))
  return x === null || (Value.isPair(x) && (x as any).isList)
}
registerValue('list?', listQ, Prelude)

function list (...xs: Value.T[]): Value.List {
  checkContract(arguments, contract('list', [], C.any))
  let ret: Value.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = Value.mkPair(xs[i], ret)
  }
  return ret
}
registerValue('list', list, Prelude)


function makeList (n: number, fill: Value.T): Value.List {
  checkContract(arguments, contract('make-list', [C.integer, C.any]))
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = Value.mkPair(fill, ret)
  }
  return ret
}
registerValue('make-list', makeList, Prelude)

function length (l: Value.List): number {
  checkContract(arguments, contract('length', [C.list]))
  let len = 0
  while (l !== null) {
    len += 1
    l = (l.snd as Value.List)
  }
  return len
}
registerValue('length', length, Prelude)

function appendOne_ (l1: Value.List, l2: Value.List): Value.List {
  if (l1 === null) {
    return l2
  } else {
    return Value.mkPair(l1.fst, appendOne_(l1.snd as Value.List, l2))
  }
}

function append (l: Value.List, ...ls: Value.List[]): Value.List {
  checkContract(arguments, contract('append', [C.list], C.list))
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret 
}
registerValue('append', append, Prelude)

function reverse (l: Value.List): Value.List {
  checkContract(arguments, contract('reverse', [C.list]))
  const queue = []
  while (l !== null) {
    queue.push(l)
    l = l.snd as Value.List
  }
  queue.reverse()
  let ret = null
  while (queue.length > 0) {
    const next = queue.pop() as Value.Pair
    ret = Value.mkPair(next.fst, ret)
  }
  return ret
}
registerValue('reverse', reverse, Prelude)

function listTail (l: Value.List, k: number): Value.List {
  checkContract(arguments, contract('list-tail', [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.snd as Value.List
    k -= 1
  }
  return l
}
registerValue('list-tail', listTail, Prelude)

function listTake (l: Value.List, k: number): Value.List {
  checkContract(arguments, contract('list-take', [C.list, C.nonneg]))
  let elts = []
  // N.B., push in reverse order so we built the list right-to-left
  while (l !== null && k > 0) {
    elts.push(l.fst)
    l = l.snd as Value.List
    k -= 1
  }
  let ret: Value.List = null
  for (let i = elts.length - 1; i >= 0; i--) {
    ret = Value.mkPair(elts[i], ret)
  }
  return ret
}
registerValue('list-take', listTake, Prelude)

function listDrop (l: Value.List, k: number): Value.List {
  checkContract(arguments, contract('list-drop', [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.snd as Value.List
    k -= 1
  }
  return l
}
registerValue('list-drop', listDrop, Prelude)

function listRef (l: Value.List, n: number): Value.T {
  checkContract(arguments, contract('list-ref', [C.list, C.nonneg]))
  let i = n
  while (l !== null && i > 0) {
    l = l.snd as Value.List
    i -= 1
  }
  if (l === null) {
    throw new ScamperError('Runtime', `list-ref: index ${n} out of bounds of list`)
  } else {
    return l.fst
  }
}
registerValue('list-ref', listRef, Prelude)

// N.B., list-set! is unimplemented since it is effectful.

// N.B., association list operations implemented below.
//   (memq obj list)
//   (memv obj list)
//   (member obj list compare)
//   (assq obj alist)
//   (assv obj alist)
//   (assoc obj alist)
//   (assoc obj alist compare)

// N.B., no need for list-copy since we have immutable lists.
//   (list-copy obj)

// Other list functions

function indexOf (l: Value.List, v: Value.T): number {
  checkContract(arguments, contract('index-of', [C.list, C.any]))
  let i = 0
  while (l !== null) {
    if (Value.equal(l.fst, v)) {
      return i
    }
    l = l.snd as Value.List
    i += 1
  }
  return -1
}
registerValue('index-of', indexOf, Prelude)

function assocKey (v: Value.T, l: Value.List): boolean {
  checkContract(arguments, contract('assoc-key?', [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (Value.equal((l.fst as Value.Pair).fst, v)) {
      return true
    }
    l = l.snd as Value.List
  }
  return false
}
registerValue('assoc-key?', assocKey, Prelude)

function assocRef (v: Value.T, l: Value.List): Value.T {
  checkContract(arguments, contract('assoc-ref', [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (Value.equal((l.fst as Value.Pair).fst, v)) {
      return (l.fst as Value.Pair).snd
    }
    l = l.snd as Value.List
  }
  throw new ScamperError('Runtime', `assoc-ref: key ${v} not found in association list`)
}
registerValue('assoc-ref', assocRef, Prelude)

function assocSet (k: Value.T, v: Value.T, l: Value.List): Value.List {
  checkContract(arguments, contract('assoc-set', [C.any, C.any, C.listof(C.pair)]))
  const front = []
  // TODO: implement me—this isn't the right implementation!
  while (l !== null) {
    const entry = l.fst as Value.Pair
    if (Value.equal(entry.fst, k)) {
      front.push(Value.mkPair(k, v))
      let ret = l.snd as Value.List
      for (let i = front.length - 1; i >= 0; i--) {
        ret = Value.mkPair(front[i], ret)
      }
      return ret
    } else {
      front.push(l.fst)
      l = l.snd as Value.List
    }
  }
  return Value.vectorToList(front.concat([Value.mkPair(k, v)]))
}
registerValue('assoc-set', assocSet, Prelude)

// Symbols (6.5)

// TODO: implement:
//   (symbol? obj)
//   (symbol=? sym1 ... symk)
//   (symbol->string sym)
//   (string->symbol str)
//
// ...but we don't implement symbols, will we?

// Characters (6.6)

function charQ (x: any): boolean {
  checkContract(arguments, contract('char?', [C.any]))
  return Value.isChar(x)
}
registerValue('char?', charQ, Prelude)

function pairwiseSatisfies<T> (f: (a: T, b: T) => boolean, xs: T[]): boolean {
  if (xs.length <= 1) {
    return true
  } else {
    for (let i = 0; i < xs.length - 1; i++) {
      if (!f(xs[i], xs[i + 1])) {
        return false
      }
    }
    return true
  }
}

function mkCharCompareFn (name: string, f: (a: string, b: string) => boolean): void {
  const fn = function (...args: Value.T[]) {
    checkContract(arguments, contract(name, [], C.char))
    return pairwiseSatisfies((a, b) => f((a as Value.Char).value, (b as Value.Char).value), args)
  }
  Value.nameFn(name, fn)
  registerValue(name, fn, Prelude)
}

mkCharCompareFn('char=?', (a, b) => a === b)
mkCharCompareFn('char<?', (a, b) => a.codePointAt(0)! < b.codePointAt(0)!)
mkCharCompareFn('char>?', (a, b) => a.codePointAt(0)! > b.codePointAt(0)!)
mkCharCompareFn('char<=?', (a, b) => a.codePointAt(0)! <= b.codePointAt(0)!)
mkCharCompareFn('char>=?', (a, b) => a.codePointAt(0)! >= b.codePointAt(0)!)
mkCharCompareFn('char-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
mkCharCompareFn('char-ci<?', (a, b) => a.toLowerCase().codePointAt(0)! < b.toLowerCase().codePointAt(0)!)
mkCharCompareFn('char-ci>?', (a, b) => a.toLowerCase().codePointAt(0)! > b.toLowerCase().codePointAt(0)!)
mkCharCompareFn('char-ci<=?', (a, b) => a.toLowerCase().codePointAt(0)! <= b.toLowerCase().codePointAt(0)!)
mkCharCompareFn('char-ci>=?', (a, b) => a.toLowerCase().codePointAt(0)! >= b.toLowerCase().codePointAt(0)!)

function mkCharPredicatePrim (name: string, f: (a: string) => boolean): void {
  const fn = function (x: Value.Char) {
    checkContract(arguments, contract(name, [], C.char))
    return f(x.value)
  }
  Value.nameFn(name, fn)
  registerValue(name, fn, Prelude)
}

mkCharPredicatePrim('char-alphabetic?', (a) => /\p{L}/gu.test(a))
mkCharPredicatePrim('char-numeric?', (a) => /\p{N}/gu.test(a))
mkCharPredicatePrim('char-whitespace?', (a) => /\p{Z}/gu.test(a))
mkCharPredicatePrim('char-upper-case?', (a) => /\p{Lu}/gu.test(a))
mkCharPredicatePrim('char-lower-case?', (a) => /\p{Ll}/gu.test(a))

function digitalValue (c: Value.Char): number {
  checkContract(arguments, contract('digit-value', [], C.char))
  const n = parseInt(c.value, 10)
  if (isNaN(n)) {
    throw new ScamperError('Runtime', `digit-value: ${c.value} is not a decimal digit`)
  } else {
    return n
  }
}
registerValue('digit-value', digitalValue, Prelude)

function charToInteger (c: Value.Char): number {
  checkContract(arguments, contract('char->integer', [], C.char))
  return c.value.codePointAt(0)!
}
registerValue('char->integer', charToInteger, Prelude)

function integerToChar (n: number): Value.Char {
  checkContract(arguments, contract('integer->char', [C.integer]))
  return Value.mkChar(String.fromCodePoint(n))
}
registerValue('integer->char', integerToChar, Prelude)

function charUpcase (c: Value.Char): Value.Char {
  checkContract(arguments, contract('char-upcase?', [], C.char))
  return Value.mkChar(c.value.toUpperCase())
}
registerValue('char-upcase', charUpcase, Prelude)

function charDowncase (c: Value.Char): Value.Char {
  checkContract(arguments, contract('char-downcase?', [], C.char))
  return Value.mkChar(c.value.toLowerCase())
}
registerValue('char-downcase', charDowncase, Prelude)

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
function charFoldcase (c: Value.Char): Value.Char {
  checkContract(arguments, contract('char-foldcase?', [], C.char))
  return Value.mkChar(c.value.toLowerCase())
}
registerValue('char-foldcase', charFoldcase, Prelude)

// Strings (6.7)

function stringQ (x: any): boolean {
  checkContract(arguments, contract('string?', [C.any]))
  return typeof x === 'string'
}
registerValue('string?', stringQ, Prelude)

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
function makeString (k: number, c: Value.Char): string {
  checkContract(arguments, contract('make-string', [C.integer, C.char]))
  return c.value.repeat(k)
}
registerValue('make-string', makeString, Prelude)

function string (c: Value.Char, ...cs: Value.Char[]): string {
  checkContract(arguments, contract('string', [C.char], C.char))
  return [c, ...cs].map((e) => e.value).join('')
}
registerValue('string', string, Prelude)

function stringLength (s: string): number {
  checkContract(arguments, contract('string-length', [C.string]))
  return s.length
}
registerValue('string-length', stringLength, Prelude)

function stringRef (s: string, i: number): Value.Char {
  checkContract(arguments, contract('string-ref', [C.string, C.integer]))
  return Value.mkChar(s[i])
}
registerValue('string-ref', stringRef, Prelude)

// N.B., string-set! is unimplemented since it is effectful.

function mkStringCompareFn (name: string, f: (a: string, b: string) => boolean): void {
  const fn = function (...args: string[]) {
    checkContract(arguments, contract(name, [], C.string))
    return pairwiseSatisfies((a, b) => f(a, b), args)
  }
  Value.nameFn(name, fn)
  registerValue(name, fn, Prelude)
}

mkStringCompareFn('string=?', (a, b) => a === b)
mkStringCompareFn('string<?', (a, b) => a < b)
mkStringCompareFn('string>?', (a, b) => a > b)
mkStringCompareFn('string<=?', (a, b) => a <= b)
mkStringCompareFn('string>=?', (a, b) => a >= b)
mkStringCompareFn('string-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
mkStringCompareFn('string-ci<?', (a, b) => a.toLowerCase() < b.toLowerCase())
mkStringCompareFn('string-ci>?', (a, b) => a.toLowerCase() > b.toLowerCase())
mkStringCompareFn('string-ci<=?', (a, b) => a.toLowerCase() <= b.toLowerCase())
mkStringCompareFn('string-ci>=?', (a, b) => a.toLowerCase() >= b.toLowerCase())

function stringUpcase (s: string): string {
  checkContract(arguments, contract('string-upcase', [C.string])) 
  return s.toUpperCase()
}
registerValue('string-upcase', stringUpcase, Prelude)

function stringDowncase (s: string): string {
  checkContract(arguments, contract('string-downcase', [C.string])) 
  return s.toLowerCase()
}
registerValue('string-downcase', stringDowncase, Prelude)

function stringFoldcase (s: string): string {
  checkContract(arguments, contract('string-foldcase', [C.string])) 
  return s.toLowerCase()
}
registerValue('string-foldcase', stringFoldcase, Prelude)

function substring (s: string, start: number, end: number): string {
  checkContract(arguments, contract('substring', [C.string, C.integer, C.integer])) 
  return s.substring(start, end)
}
registerValue('substring', substring, Prelude)

function stringAppend (...args: string[]): string {
  checkContract(arguments, contract('string-append', [], C.string))
  return args.join('')
}
registerValue('string-append', stringAppend, Prelude)

// TODO: stringToList has a 3-argument version, too, that specifies
// a substring of s to turn into a list.
function stringToList (s: string): Value.List {
  checkContract(arguments, contract('string->list', [C.string]))
  let ret = null
  for (let i = s.length - 1; i >= 0; i--) {
    ret = Value.mkPair(Value.mkChar(s[i]), ret)
  }
  return ret
}
registerValue('string->list', stringToList, Prelude)

function listToString (l: Value.List): string {
  checkContract(arguments, contract('list->string', [C.list]))
  let ret = ''
  while (l !== null) {
    if (!Value.isChar(l.fst)) {
      throw new ScamperError('Runtime', `list->string: list contains non-character element: ${Value.typeOf(l.fst)}`)
    }
    ret += (l.fst as Value.Char).value
    l = l.snd as Value.List
  } 
  return ret
}
registerValue('list->string', listToString, Prelude)

function stringToVector (s: string): Value.Char[] {
  checkContract(arguments, contract('string->vector', [C.string]))
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(Value.mkChar(s[i]))
  }
  return ret
}
registerValue('string->vector', stringToVector, Prelude)

function vectorToString (v: Value.Char[]): string {
  checkContract(arguments, contract('vector->string', [C.vector]))
  let ret = ''
  for (let i = 0; i < v.length; i++) {
    ret += v[i].value
  }
  return ret
}
registerValue('vector->string', vectorToString, Prelude)

// N.B., the following functions:
//
//   (string-copy string)
//   (string-copy string start)
//   (string-copy string start end)
//
// and string-copy! and string-fill! are unimplemented since they don't make
// sense in an immutable context.

// Additional functions from racket/string.

function stringContains (s: string, sub: string): boolean {
  checkContract(arguments, contract('string-contains', [C.string, C.string]))  
  return s.includes(sub)
}
registerValue('string-contains', stringContains, Prelude)

function stringSplit (s: string, sep: string): Value.List {
  checkContract(arguments, contract('string-split', [C.string, C.string]))
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = Value.mkPair(splits[i], ret)
  }
  return ret
}
registerValue('string-split', stringSplit, Prelude)


function stringSplitVector (s: string, sep: string): string[] {
  checkContract(arguments, contract('string-split-vector', [C.string, C.string])) 
  return s.split(sep)
}
registerValue('string-split-vector', stringSplitVector, Prelude)

// TODO: what should the type of a reactive-file object be? A struct? Or a JS object?
// TODO: need to add a custom renderer for reactive file blobs

export interface ReactiveFile extends Value.Struct {
  [Value.structKind]: 'reactive-file',
  callback: Value.ScamperFn
}

function withFile (callback: Value.ScamperFn): ReactiveFile {
  checkContract(arguments, contract('with-file', [C.func]))  
  return {
    [Value.scamperTag]: 'struct',
    [Value.structKind]: 'reactive-file',
    callback
  }
}
registerValue('with-file', withFile, Prelude)

function renderReactiveFile (v: any): HTMLElement {
  const rf = v as ReactiveFile
  const ret = document.createElement('code')
  const inp = document.createElement('input')
  const outp = document.createElement('code')
  inp.type = 'file'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
        outp.innerHTML = ''
        try {
          const v = callFunction(rf.callback, [e.target.result as string])
          outp.appendChild(Display.renderToHTML(v))
        } catch (e) {
          outp.appendChild(Display.renderToHTML(e as ScamperError))
        }
      } else {
        outp.innerText = ''
      }
    }
    if (inp.files !== null && inp.files.length > 0) {
      outp.innerText = 'Loading...'
      reader.readAsText(inp.files[0])
    }
  }, false)

  ret.appendChild(inp)
  ret.appendChild(document.createElement('br'))
  ret.appendChild(outp)
  return ret
}
Display.addCustomWebRenderer(
  (v) => Value.isStructKind(v, 'reactive-file'), renderReactiveFile)

// Vectors (6.8)

function vectorQ (x: any): boolean {
  checkContract(arguments, contract('vector?', [C.any]))
  return Value.isArray(x)
}
registerValue('vector?', vectorQ, Prelude)

function vector (...xs: Value.T[]): Value.T[] {
  checkContract(arguments, contract('vector', [], C.any))
  return xs
}
registerValue('vector', vector, Prelude)

function makeVector (n: number, fill: Value.T): Value.T[] {
  checkContract(arguments, contract('make-vector', [C.integer, C.any]))
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}
registerValue('make-vector', makeVector, Prelude)

function vectorLength (v: Value.T[]): number {
  checkContract(arguments, contract('vector-length', [C.vector])) 
  return v.length
}
registerValue('vector-length', vectorLength, Prelude)

function vectorRef (v: Value.T[], i: number): Value.T {
  checkContract(arguments, contract('vector-ref', [C.vector, C.integer]))
  return v[i]
}
registerValue('vector-ref', vectorRef, Prelude)

function vectorSet (v: Value.T[], i: number, x: Value.T): void {
  checkContract(arguments, contract('vector-set!', [C.vector, C.integer, C.any]))
  v[i] = x
}
registerValue('vector-set!', vectorSet, Prelude)

function vectorFill (v: Value.T[], x: Value.T): void {
  checkContract(arguments, contract('vector-fill!', [C.vector, C.any]))
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}
registerValue('vector-fill!', vectorFill, Prelude)

function vectorToList (v: Value.T[]): Value.List {
  checkContract(arguments, contract('vector->list', [C.vector]))
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = Value.mkPair(v[i], ret)
  }
  return ret
}
registerValue('vector->list', vectorToList, Prelude)

function listToVector (l: Value.List): Value.T[] {
  checkContract(arguments, contract('list->vector', [C.list]))
  const ret = []
  while (l !== null) {
    ret.push(l.fst)
    l = l.snd as Value.List
  }
  return ret
}
registerValue('list->vector', listToVector, Prelude)

function vectorRange (...args: number[]): number[] {
  checkContract(arguments, contract('vector-range', [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new ScamperError('Runtime', '1, 2, or 3 numbers must be passed to function')
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new ScamperError('Runtime', '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return arr
  }
}
registerValue('vector-range', vectorRange, Prelude)

function vectorAppend (...vecs: Value.T[][]): Value.T[] {
  checkContract(arguments, contract('vector-append', [], C.vector))
  const arr = []
  for (let i = 0; i < vecs.length; i++) {
    for (let j = 0; j < vecs[i].length; j++) {
      arr.push(vecs[i][j])
    }
  }
  return arr
}
registerValue('vector-append', vectorAppend, Prelude)

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

function procedureQ (x: any): boolean {
  checkContract(arguments, contract('procedure?', [C.any]))
  return Value.isClosure(x) || Value.isJsFunction(x)
}
registerValue('procedure?', procedureQ, Prelude)

function apply (f: Value.Closure | Function, args: Value.List): Value.T {
  checkContract(arguments, contract('apply', [C.func, C.list]))

  return callFunction(f, ...Value.listToVector(args))
}
registerValue('apply', apply, Prelude)

function stringMap (f: Value.Closure | Function, s: string): string {
  checkContract(arguments, contract('string-map', [C.func, C.string]))
  let chs = []
  for (let i = 0; i < s.length; i++) {
    chs.push(Value.mkChar(s[i]))
  }
  return chs.map((c) => callFunction(f, c).value).join('')
}
registerValue('string-map', stringMap, Prelude)

/**
 * @param arr - a rectangular array of arrays, i.e., each array has the same
 * length
 * @returns the transposition of this array of arrays where rows become columns
 * and columns become rows.
 */
function transpose <T> (arr: T[][]): T[][] {
  if (arr.length === 0) { return [] }
  const numArrays = arr.length
  // N.B., assumed that all arrays have the same length
  const numArgs = arr[0].length
  const result: T[][] = []
  for (let i = 0; i < numArgs; i++) {
    result.push([])
  }
  for (let i = 0; i < numArgs; i++) {
    for (let j = 0; j < numArrays; j++) {
      result[i].push(arr[j][i])
    }
  }
  return result
}

function mapOne (f: Value.Closure | Function, l: Value.List): Value.List {
  const values = []
  while (l !== null) {
    values.push(callFunction(f, l.fst))
    l = l.snd as Value.Pair
  }
  return Value.vectorToList(values)
}

function map (f: Value.Closure | Function, ...lsts: Value.List[]): Value.List {
  checkContract(arguments, contract('map', [C.func], C.list))
  if (lsts.length === 0) {
    return null
  } else if (lsts.length === 1) {
    return mapOne(f, lsts[0])
  } else {
    const lists = lsts.map(Value.listToVector)
    if (!(lists.map(l => l.length).every(n => n === lists[0].length))) {
      throw new ScamperError('Runtime', 'the lists passed to the function call do not have the same length')
    }
    const xs = transpose(lists)
    return Value.vectorToList(xs.map(vs => callFunction(f, ...vs)))
  }
}
registerValue('map', map, Prelude)

// Additional list pipeline functions from racket/base

function filter (f: Value.Closure | Function, lst: Value.List): Value.List {
  checkContract(arguments, contract('filter', [C.func, C.list]))
  const values = []
  while (lst !== null) {
    if (callFunction(f, lst.fst)) {
      values.push(lst.fst)
    }
    lst = lst.snd as Value.Pair
  }
  return Value.vectorToList(values) 
}
registerValue('filter', filter, Prelude)

function fold (f: Value.Closure | Function, init: Value.T, lst: Value.List): Value.T {
  checkContract(arguments, contract('fold', [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as Value.Pair
  }
  return acc
}
registerValue('fold', fold, Prelude)

function reduce (f: Value.Closure | Function, lst: Value.List): Value.T {
  checkContract(arguments, contract('reduce', [C.func, C.nonemptyList]))
  let acc = (lst as Value.Pair).fst
  lst = (lst as Value.Pair).snd as Value.Pair
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as Value.Pair
  }
  return acc
}
registerValue('reduce', reduce, Prelude)

function foldLeft (f: Value.Closure | Function, init: Value.T, lst: Value.List): Value.T {
  checkContract(arguments, contract('fold-left', [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as Value.Pair
  }
  return acc
}
registerValue('fold-left', foldLeft, Prelude)

function foldRight (f: Value.Closure | Function, init: Value.T, lst: Value.List): Value.T {
  checkContract(arguments, contract('fold-right', [C.func, C.any, C.list]))
  const values = Value.listToVector(lst)
  let acc = init
  for (let i = values.length - 1; i >= 0; i--) {
    acc = callFunction(f, values[i], acc)
  }
  return acc
}
registerValue('fold-right', foldRight, Prelude)

function reduceRight (f: Value.Closure | Function, lst: Value.List): Value.T {
  checkContract(arguments, contract('reduce-right', [C.func, C.nonemptyList]))
  const values = Value.listToVector(lst)
  let acc = values.pop()
  for (let i = values.length - 1; i >= 0; i--) {
    acc = callFunction(f, values[i], acc)
  }
  return acc
}
registerValue('reduce-right', reduceRight, Prelude)

function vectorMap (f: Value.Closure | Function, ...vecs: Value.T[][]): Value.T[] {
  checkContract(arguments, contract('vector-map', [C.func], C.vector))
  if (vecs.length === 0) {
    return []
  } else if (vecs.length === 1) {
    return vecs[0].map((v) => callFunction(f, v))
  } else {
    if (!(vecs.map(l => l.length).every(n => n === vecs[0].length))) {
      throw new ScamperError('Runtime', 'the vectors passed to the function call do not have the same length')
    }
    const xs = transpose(vecs)
    return xs.map(vs => callFunction(f, ...vs))
  }
}
registerValue('vector-map', vectorMap, Prelude)

function vectorMapBang (f: Value.Closure | Function, vec: Value.T[]): void {
  checkContract(arguments, contract('vector-map!', [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    vec[i] = callFunction(f, vec[i])
  }
}
registerValue('vector-map!', vectorMapBang, Prelude)

function vectorForEach (f: Value.Closure | Function, vec: Value.T[]): void {
  checkContract(arguments, contract('vector-for-each', [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    callFunction(f, vec[i])
  }
}
registerValue('vector-for-each', vectorForEach, Prelude)

function forRange (start: number, end: number, f: Value.Closure | Function): void {
  checkContract(arguments, contract('for-range', [C.integer, C.integer, C.func]))
  if (start < end) {
    for (let i = start; i < end; i++) {
      callFunction(f, i)
    }
  } else {
    for (let i = start; i > end; i--) {
      callFunction(f, i)
    }
  }
}
registerValue('for-range', forRange, Prelude)

// TODO: implement:
//   (for-each fn l1 ... lk)
//   (string-for-each fn str1 ... strk)

// N.B., (vector-for-each fn v1 ... vk) not implemented since vectors are not implemented.

// TODO: implement:
//   (call-with-current-continuation proc)
//   (call/cc proc)
//   (values obj ...)
//   (call-with-values producer consumer)
//   (dynamic-wind before thunk after)

// Additional control features

function vectorFilter (f: Value.Closure | Function, lst: Value.T[]): Value.T[] {
  checkContract(arguments, contract('vector-filter', [C.func, C.vector]))
  const ret = []
  for (let i = 0; i < lst.length; i++) {
    if (callFunction(f, lst[i])) {
      ret.push(lst[i])
    }
  }
  return ret
}
registerValue('vector-filter', vectorFilter, Prelude)

// TODO: implement fold/reduce variants for vectors

function voidQ (x: any): boolean {
  checkContract(arguments, contract('void?', [C.any]))
  return x === undefined
}
registerValue('void?', voidQ, Prelude)

function error (msg: string): never {
  checkContract(arguments, contract('error', [C.string]))
  throw new ScamperError ('Runtime', msg)
}
registerValue('error', error, Prelude)

function qq (): never {
  checkContract(arguments, contract('??', []))
  throw new ScamperError ('Runtime', 'Hole encountered in program!')
}
registerValue('??', qq, Prelude)

function compose (...fss: (Value.Closure | Function)[]): Value.Closure | Function {
  checkContract(arguments, contract('compose', [C.func], C.func))
  let first = fss[fss.length - 1]
  return (x: Value.T) => {
    let ret = callFunction(first, x)
    for (let i = fss.length - 2; i >= 0; i--) {
      ret = callFunction(fss[i], ret)
    }
    return ret
  }
}
registerValue('compose', compose, Prelude)
registerValue('o', compose, Prelude)

function pipe (init: Value.T, ...fs: (Value.Closure | Function)[]): Value.T {
  checkContract(arguments, contract('|>', [C.any, C.func], C.func))
  let acc = init
  for (let i = 0; i < fs.length; i++) {
    acc = callFunction(fs[i], acc)
  }
  return acc
}
registerValue('|>', pipe, Prelude)

function range (...args: number[]): Value.List {
  checkContract(arguments, contract('range', [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new ScamperError('Runtime', '1, 2, or 3 numbers must be passed to function')
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new ScamperError('Runtime', '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return Value.vectorToList(arr)
  }
}
registerValue('range', range, Prelude)

function random (n: number): number {
  checkContract(arguments, contract('random', [C.integer])) 
  return Math.floor(Math.random() * n)
}
registerValue('random', random, Prelude)

function withHandler (handler: Value.Closure | Function, fn: Value.Closure | Function, ...args: Value.T[]): Value.T {
  checkContract(arguments, contract('with-handler', [C.func, C.func], C.any))
  try {
    return callFunction(fn, ...args)
  } catch (e) {
    return callFunction(handler, (e as ScamperError).message)
  }
}
registerValue('with-handler', withHandler, Prelude)

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

// Additional Scamper-specific functions

function ignore (_v: Value.T): HTMLElement {
  checkContract(arguments, contract('ignore', [C.any]))
  const ret = document.createElement('div')
  ret.style.display = 'non'
  return ret
}
registerValue('ignore', ignore, Prelude)

// Additional constants

const elseConst = true
const nullConst = null
const piConst = Math.PI
const voidConst = undefined

registerValue('else', elseConst, Prelude)
registerValue('null', nullConst, Prelude)
registerValue('pi', piConst, Prelude)
registerValue('π', piConst, Prelude)
registerValue('void', voidConst, Prelude)

export default Prelude
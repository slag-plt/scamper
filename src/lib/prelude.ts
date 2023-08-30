import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { Value } from '../value.js'
import * as V from '../value.js'
import { callFunction } from '../sem.js'
import { ScamperError } from '../lang.js'

function registerFn (name: string, fn: Function, map: [string, Value][]) {
  V.nameFn(name, fn)
  map.push([name, fn])
}

const Prelude: [string, Value][] = []

const query1C = (name: string) => contract(name, [C.any])
const query2C = (name: string) => contract(name, [C.any])

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

function equalQ (x: any, y: any): boolean {
  checkContract(arguments, query2C('equal?'))
  return V.valuesEqual(x, y)
}
registerFn('equal?', equalQ, Prelude)

// Numbers (6.2)

function numberQ (x: any): boolean {
  checkContract(arguments, query1C('number?'))
  return typeof x === 'number'
}
registerFn('number?', numberQ, Prelude)

function realQ (x: any): boolean {
  checkContract(arguments, query1C('real?'))
  return typeof x === 'number' && !Number.isInteger(x)
}
registerFn('real?', realQ, Prelude)

function integerQ (x: any): boolean {
  checkContract(arguments, query1C('integer?'))
  return typeof x === 'number' && Number.isInteger(x)
}
registerFn('integer?', integerQ, Prelude)

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
registerFn('nan?', nanQ, Prelude)

function lt (x: number, y: number): boolean {
  checkContract(arguments, contract('<', [C.number, C.number]))
  return x < y
}
registerFn('<', lt, Prelude)

function leq (x: number, y: number): boolean {
  checkContract(arguments, contract('<=', [C.number, C.number]))
  return x <= y
}
registerFn('<=', leq, Prelude)

function gt (x: number, y: number): boolean {
  checkContract(arguments, contract('>', [C.number, C.number]))
  return x > y
}
registerFn('>', gt, Prelude)

function geq (x: number, y: number): boolean {
  checkContract(arguments, contract('>=', [C.number, C.number]))
  return x >= y
}
registerFn('>=', geq, Prelude)

function eq (x: number, y: number): boolean {
  checkContract(arguments, contract('=', [C.number, C.number]))
  return x === y
}
registerFn('=', eq, Prelude)

function zeroQ (x: number): boolean {
  checkContract(arguments, contract('zero?', [C.number]))
  return x === 0
}
registerFn('zero?', zeroQ, Prelude)

function positiveQ (x: number): boolean {
  checkContract(arguments, contract('positive?', [C.number]))
  return x > 0
}
registerFn('positive?', positiveQ, Prelude)

function negativeQ (x: number): boolean {
  checkContract(arguments, contract('negative?', [C.number]))
  return x < 0
}
registerFn('negative?', negativeQ, Prelude)

function oddQ (x: number): boolean {
  checkContract(arguments, contract('odd?', [C.integer]))
  return (x & 1) === 1
}
registerFn('odd?', oddQ, Prelude)

function evenQ (x: number): boolean {
  checkContract(arguments, contract('even?', [C.integer]))
  return (x & 1) !== 1
}
registerFn('even?', evenQ, Prelude)

function max (...xs: number[]): number {
  checkContract(arguments, contract('max', [], C.number))
  return Math.max(...xs)
}
registerFn('max', max, Prelude)

function min (...xs: number[]): number {
  checkContract(arguments, contract('min', [], C.number))
  return Math.min(...xs)
}
registerFn('min', min, Prelude)

function plus (...xs: number[]): number {
  checkContract(arguments, contract('+', [], C.number))
  return xs.reduce((a, b) => a + b, 0)
}
registerFn('+', plus, Prelude)

function minus (...xs: number[]): number {
  checkContract(arguments, contract('-', [C.number], C.number))
  return xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
}
registerFn('-', minus, Prelude)

function times (...xs: number[]): number {
  checkContract(arguments, contract('*', [], C.number))
  return xs.reduce((a, b) => a * b, 1)
}
registerFn('*', times, Prelude)

function div (...xs: number[]): number {
  checkContract(arguments, contract('/', [C.number], C.number))
  return xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
}
registerFn('/', div, Prelude)

function abs (x: number): number {
  checkContract(arguments, contract('abs', [C.number]))
  return Math.abs(x)
}
registerFn('abs', abs, Prelude)

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
registerFn('quotient', quotient, Prelude)

function remainder (x: number, y: number): number {
  checkContract(arguments, contract('remainder', [C.integer, C.integer]))
  return x % y
}
registerFn('remainder', remainder, Prelude)

function modulo (x: number, y: number): number {
  checkContract(arguments, contract('modulo', [C.integer, C.integer]))
  return ((x % y) + y) % y
}
registerFn('modulo', modulo, Prelude)

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
registerFn('floor', floor, Prelude)

function ceiling (x: number): number {
  checkContract(arguments, contract('ceiling', [C.number]))
  return Math.ceil(x)
}
registerFn('ceiling', ceiling, Prelude)

function truncate (x: number): number {
  checkContract(arguments, contract('truncate', [C.number]))
  return Math.trunc(x)
}
registerFn('truncate', truncate, Prelude)

function round (x: number): number {
  checkContract(arguments, contract('round', [C.number]))
  return Math.round(x)
}
registerFn('round', round, Prelude)

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

function square (x: number): number {
  checkContract(arguments, contract('square', [C.number]))
  return x * x
}
registerFn('square', square, Prelude)

function sqrt (x: number): number {
  checkContract(arguments, contract('sqrt', [C.number]))
  return Math.sqrt(x)
}
registerFn('sqrt', sqrt, Prelude)

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

function expt (x: number, y: number): number {
  checkContract(arguments, contract('expt', [C.number, C.number]))
  return Math.pow(x, y)
}
registerFn('expt', expt, Prelude)

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
registerFn('number->string', numberToString, Prelude)

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
registerFn('string->number', stringToNumber, Prelude)

// Additional functions from racket/base

function exp (x: number): number {
  checkContract(arguments, contract('exp', [C.number]))
  return Math.exp(x)
}
registerFn('exp', exp, Prelude)

function log (x: number): number {
  checkContract(arguments, contract('log', [C.number]))
  return Math.log(x)
}
registerFn('log', log, Prelude)

function sin (x: number): number {
  checkContract(arguments, contract('sin', [C.number]))
  return Math.sin(x)
}
registerFn('sin', sin, Prelude)

function cos (x: number): number {
  checkContract(arguments, contract('cos', [C.number]))
  return Math.cos(x)
}
registerFn('cos', cos, Prelude)

function tan (x: number): number {
  checkContract(arguments, contract('tan', [C.number]))
  return Math.tan(x)
}
registerFn('tan', tan, Prelude)

function asin (x: number): number {
  checkContract(arguments, contract('asin', [C.number]))
  return Math.asin(x)
}
registerFn('asin', asin, Prelude)

function acos (x: number): number {
  checkContract(arguments, contract('acos', [C.number]))
  return Math.acos(x)
}
registerFn('acos', acos, Prelude)

function atan (x: number): number {
  checkContract(arguments, contract('atan', [C.number]))
  return Math.atan(x)
}
registerFn('atan', atan, Prelude)

function equalsEps (eps: number): Value {
  checkContract(arguments, contract('=-eps', [C.number]))
  const name = `=-(${eps})`
  const ret = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-(${eps})`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  }
  V.nameFn(name, ret)
  return ret
}
registerFn('=-eps', equalsEps, Prelude)

// Booleans (6.3)

function not (x: boolean): boolean {
  checkContract(arguments, contract('not', [C.boolean]))
  return !x
}
registerFn('not', not, Prelude)

function booleanQ (x: any): boolean {
  checkContract(arguments, contract('boolean?', [C.any]))
  return typeof x === 'boolean'
}
registerFn('boolean?', booleanQ, Prelude)

// From racket/base

function nand (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nand', [], C.boolean))
  return !xs.reduce((a, b) => a && b, true)
}
registerFn('nand', nand, Prelude)

function nor (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nor', [], C.boolean))
  return !xs.reduce((a, b) => a || b, false)
}
registerFn('nor', nor, Prelude)

function implies (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('implies', [C.boolean, C.boolean]))
  return !x || y
}
registerFn('implies', implies, Prelude)

function xor (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('xor', [C.boolean, C.boolean]))
  return (x && !y) || (!x && y)
}
registerFn('xor', xor, Prelude)

// Pairs and Lists (6.4)

function pairQ (x: any): boolean {
  checkContract(arguments, contract('pair?', [C.any]))
  return V.isPair(x)
}
registerFn('pair?', pairQ, Prelude)

function cons (x: any, y: any): Value {
  checkContract(arguments, contract('cons', [C.any, C.any]))
  return V.mkPair(x, y)
}
registerFn('cons', cons, Prelude)

function pair (x: any, y: any): Value {
  checkContract(arguments, contract('pair', [C.any, C.any]))
  return V.mkPair(x, y)
}
registerFn('pair', pair, Prelude)

function car (x: Value): Value {
  checkContract(arguments, contract('car', [C.pair]))
  return (x as any).fst
}
registerFn('car', car, Prelude)

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

function nullQ (x: any): boolean {
  checkContract(arguments, contract('null?', [C.any]))
  return x === null
}
registerFn('null?', nullQ, Prelude)

function listQ (x: any): boolean {
  checkContract(arguments, contract('list?', [C.any]))
  return x === null || (V.isPair(x) && (x as any).isList)
}
registerFn('list?', listQ, Prelude)

function list (...xs: Value[]): V.List {
  checkContract(arguments, contract('list', [], C.any))
  let ret: V.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = V.mkPair(xs[i], ret)
  }
  return ret
}
registerFn('list', list, Prelude)


function makeList (n: number, fill: Value): V.List {
  checkContract(arguments, contract('make-list', [C.integer, C.any]))
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = V.mkPair(fill, ret)
  }
  return ret
}
registerFn('make-list', makeList, Prelude)

function length (l: V.List): number {
  checkContract(arguments, contract('length', [C.list]))
  let len = 0
  while (l !== null) {
    len += 1
    l = (l.snd as V.List)
  }
  return len
}
registerFn('length', length, Prelude)

function appendOne_ (l1: V.List, l2: V.List): V.List {
  if (l1 === null) {
    return l2
  } else {
    return V.mkPair(l1.fst, appendOne_(l1.snd as V.List, l2))
  }
}

function append (l: V.List, ...ls: V.List[]): V.List {
  checkContract(arguments, contract('append', [C.list], C.list))
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret 
}
registerFn('append', append, Prelude)

function reverse (l: V.List): V.List {
  checkContract(arguments, contract('reverse', [C.list]))
  const queue = []
  while (l !== null) {
    queue.push(l)
    l = l.snd as V.List
  }
  queue.reverse()
  let ret = null
  while (queue.length > 0) {
    const next = queue.pop() as V.Pair
    ret = V.mkPair(next.fst, ret)
  }
  return ret
}
registerFn('reverse', reverse, Prelude)

function listTail (l: V.List, k: number): V.List {
  throw new Error ('Prelude.tail unimplemented')
}
registerFn('list-tail', listTail, Prelude)

function listTake (l: V.List, k: number): V.List {
  throw new Error ('Prelude.take unimplemented')
}
registerFn('list-take', listTake, Prelude)

function listRef (l: V.List, n: number): Value {
  throw new Error ('Prelude.ref unimplemented')
}
registerFn('list-ref', listRef, Prelude)

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

function indexOf (l: V.List, v: Value): number {
  throw new Error ('Prelude.indexOf unimplemented') 
}
registerFn('index-of', indexOf, Prelude)

function assocKey (v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}
registerFn('assoc-key?', assocKey, Prelude)

function assocRef (v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}
registerFn('assoc-ref', assocRef, Prelude)

function assocSet (k: Value, v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}
registerFn('assoc-set', assocSet, Prelude)

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
  return V.isChar(x)
}
registerFn('char?', charQ, Prelude)

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
  const fn = function (...args: Value[]) {
    checkContract(arguments, contract(name, [], C.char))
    pairwiseSatisfies((a, b) => f((a as V.Char).value, (b as V.Char).value), args)
  }
  V.nameFn(name, fn)
  registerFn(name, fn, Prelude)
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
  const fn = function (x: V.Char) {
    checkContract(arguments, contract(name, [], C.char))
    return f(x.value)
  }
  V.nameFn(name, fn)
  registerFn(name, fn, Prelude)
}

mkCharPredicatePrim('char-alphabetic?', (a) => /\p{L}/gu.test(a))
mkCharPredicatePrim('char-numeric?', (a) => /\p{N}/gu.test(a))
mkCharPredicatePrim('char-whitespace?', (a) => /\p{Z}/gu.test(a))
mkCharPredicatePrim('char-upper-case?', (a) => /\p{Lu}/gu.test(a))
mkCharPredicatePrim('char-lower-case?', (a) => /\p{Ll}/gu.test(a))

function digitalValue (c: V.Char): number {
  checkContract(arguments, contract('digit-value', [], C.char))
  const n = parseInt(c.value, 10)
  if (isNaN(n)) {
    throw new ScamperError('Runtime', `digit-value: ${c.value} is not a decimal digit`)
  } else {
    return n
  }
}
registerFn('digit-value', digitalValue, Prelude)

function charToInteger (c: V.Char): number {
  checkContract(arguments, contract('char->integer', [], C.char))
  return c.value.codePointAt(0)!
}
registerFn('char->integer', charToInteger, Prelude)

function integerToChar (n: number): V.Char {
  checkContract(arguments, contract('integer->char', [C.integer]))
  return V.mkChar(String.fromCodePoint(n))
}
registerFn('integer->char', integerToChar, Prelude)

function charUpcase (c: V.Char): V.Char {
  checkContract(arguments, contract('char-upcase?', [], C.char))
  return V.mkChar(c.value.toUpperCase())
}
registerFn('char-upcase', charUpcase, Prelude)

function charDowncase (c: V.Char): V.Char {
  checkContract(arguments, contract('char-downcase?', [], C.char))
  return V.mkChar(c.value.toLowerCase())
}
registerFn('char-downcase', charDowncase, Prelude)

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
function charFoldcase (c: V.Char): V.Char {
  checkContract(arguments, contract('char-foldcase?', [], C.char))
  return V.mkChar(c.value.toLowerCase())
}
registerFn('char-foldcase', charFoldcase, Prelude)

// Strings (6.7)

function stringQ (x: any): boolean {
  checkContract(arguments, contract('string?', [C.any]))
  return typeof x === 'string'
}
registerFn('string?', stringQ, Prelude)

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
function makeString (k: number, c: V.Char): string {
  checkContract(arguments, contract('make-string', [C.integer, C.char]))
  return c.value.repeat(k)
}
registerFn('make-string', makeString, Prelude)

function string (c: V.Char, ...cs: V.Char[]): string {
  checkContract(arguments, contract('string', [C.char], C.string))
  return [c, ...cs].map((e) => e.value).join('')
}
registerFn('string', string, Prelude)

function stringLength (s: string): number {
  checkContract(arguments, contract('string-length', [C.string]))
  return s.length
}
registerFn('string-length', stringLength, Prelude)

function stringRef (s: string, i: number): V.Char {
  checkContract(arguments, contract('string-ref', [C.string, C.integer]))
  return V.mkChar(s[i])
}
registerFn('string-ref', stringRef, Prelude)

// N.B., string-set! is unimplemented since it is effectful.

function mkStringCompareFn (name: string, f: (a: string, b: string) => boolean): void {
  const fn = function (...args: string[]) {
    checkContract(arguments, contract(name, [], C.string))
    return pairwiseSatisfies((a, b) => f(a, b), args)
  }
  V.nameFn(name, fn)
  registerFn(name, fn, Prelude)
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
registerFn('string-upcase', stringUpcase, Prelude)

function stringDowncase (s: string): string {
  checkContract(arguments, contract('string-downcase', [C.string])) 
  return s.toLowerCase()
}
registerFn('string-downcase', stringDowncase, Prelude)

function stringFoldcase (s: string): string {
  checkContract(arguments, contract('string-foldcase', [C.string])) 
  return s.toLowerCase()
}
registerFn('string-foldcase', stringFoldcase, Prelude)

function substring (s: string, start: number, end: number): string {
  checkContract(arguments, contract('substring', [C.string, C.integer, C.integer])) 
  return s.substring(start, end)
}
registerFn('substring', substring, Prelude)

function stringAppend (...args: string[]): string {
  checkContract(arguments, contract('string-append', [], C.string))
  return args.join('')
}
registerFn('string-append', stringAppend, Prelude)

// TODO: stringToList has a 3-argument version, too, that specifies
// a substring of s to turn into a list.
function stringToList (s: string): V.List {
  checkContract(arguments, contract('string->list', [C.string]))
  let ret = null
  for (let i = s.length - 1; i >= 0; i--) {
    ret = V.mkPair(V.mkChar(s[i]), ret)
  }
  return ret
}
registerFn('string->list', stringToList, Prelude)

function listToString (l: V.List): string {
  checkContract(arguments, contract('list->string', [C.list]))
  let ret = ''
  while (l !== null) {
    ret += (l.fst as V.Char).value
    l = l.snd as V.List
  } 
  return ret
}
registerFn('list->string', listToString, Prelude)

function stringToVector (s: string): V.Char[] {
  checkContract(arguments, contract('string->vector', [C.string]))
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(V.mkChar(s[i]))
  }
  return ret
}
registerFn('string->vector', stringToVector, Prelude)

function vectorToString (v: V.Char[]): string {
  checkContract(arguments, contract('vector->string', [C.vector]))
  let ret = ''
  for (let i = 0; i < v.length; i++) {
    ret += v[i].value
  }
  return ret
}
registerFn('vector->string', vectorToString, Prelude)

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
registerFn('string-contains', stringContains, Prelude)

function stringSplit (s: string, sep: string): V.List {
  checkContract(arguments, contract('string-split', [C.string, C.string]))
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = V.mkPair(splits[i], ret)
  }
  return ret
}
registerFn('string-split', stringSplit, Prelude)


function stringSplitVector (s: string, sep: string): string[] {
  checkContract(arguments, contract('string-split-vector', [C.string, C.string])) 
  return s.split(sep)
}
registerFn('string-split-vector', stringSplitVector, Prelude)

// TODO: what should the type of a reactive-file object be? A struct? Or a JS object?

type ReactiveFile = { _scamperTag: 'struct', kind: '_reactive-file', callback: V.Closure | Function }
function withFile (callback: V.Closure | Function): ReactiveFile {
  checkContract(arguments, contract('with-file', [C.func]))  
  return {
    _scamperTag: 'struct',
    kind: '_reactive-file',
    callback
  }
}
registerFn('with-file', withFile, Prelude)

// Vectors (6.8)

function vectorQ (x: any): boolean {
  checkContract(arguments, contract('vector?', [C.any]))
  return V.isArray(x)
}
registerFn('vector?', vectorQ, Prelude)

function vector (...xs: Value[]): Value[] {
  checkContract(arguments, contract('vector', [], C.any))
  return xs
}
registerFn('vector', vector, Prelude)

function makeVector (n: number, fill: Value): Value[] {
  checkContract(arguments, contract('make-vector', [C.integer, C.any]))
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}
registerFn('make-vector', makeVector, Prelude)

function vectorLength (v: Value[]): number {
  checkContract(arguments, contract('vector-length', [C.vector])) 
  return v.length
}
registerFn('vector-length', vectorLength, Prelude)

function vectorRef (v: Value[], i: number): Value {
  checkContract(arguments, contract('vector-ref', [C.vector, C.integer]))
  return v[i]
}
registerFn('vector-ref', vectorRef, Prelude)

function vectorSet (v: Value[], i: number, x: Value): void {
  checkContract(arguments, contract('vector-set!', [C.vector, C.integer, C.any]))
  v[i] = x
}
registerFn('vector-set!', vectorSet, Prelude)

function vectorFill (v: Value[], x: Value): void {
  checkContract(arguments, contract('vector-fill!', [C.vector, C.any]))
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}
registerFn('vector-fill!', vectorFill, Prelude)

function vectorToList (v: Value[]): V.List {
  checkContract(arguments, contract('vector->list', [C.vector]))
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = V.mkPair(v[i], ret)
  }
  return ret
}
registerFn('vector->list', vectorToList, Prelude)

function listToVector (l: V.List): Value[] {
  checkContract(arguments, contract('list->vector', [C.list]))
  const ret = []
  while (l !== null) {
    ret.push(l.fst)
    l = l.snd as V.List
  }
  return ret
}
registerFn('list->vector', listToVector, Prelude)

const vectorRange = (m: number, n: number, step: number): number[] => {
  throw new ScamperError ('Runtime', 'vector-range unimplemented')
}
registerFn('vector-range', vectorRange, Prelude)

/*
// N.B., this is identical to rangePrim except it does not conver the output
// to a list! Probably should refactor this...
const vectorRangePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-range', [], 'number?', args, app).andThen(_ => {
    if (args.length === 0 || args.length > 3) {
      return runtimeError(msg('error-arity', 'vector-range', '1--3', args.length), app)
    } else {
      const m = args.length === 1 ? 0 : args[0] as number
      const n = args.length === 1 ? args[0] as number : args[1] as number
      const step = args.length < 3 ? 1 : args[2] as number
      const arr = []
      // N.B., to prevent the internal infinite loop that would result
      // from having a zero step.
      if (step === 0) {
        return runtimeError(msg('error-precondition-not-met', 'vector-range', '3', 'non-zero', step), app)
      }
      for (let i = m; step > 0 ? i < n : i > n; i += step) {
        arr.push(i)
      }
      return ok(arr)
    }
  }))
*/

function vectorAppend (...vecs: Value[][]): Value[] {
  checkContract(arguments, contract('vector-append', [], C.vector))
  const arr = []
  for (let i = 0; i < vecs.length; i++) {
    for (let j = 0; j < vecs[i].length; j++) {
      arr.push(vecs[i][j])
    }
  }
  return arr
}
registerFn('vector-append', vectorAppend, Prelude)

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

function procedureQ (x: any): boolean {
  checkContract(arguments, contract('procedure?', [C.any]))
  return V.isClosure(x) || V.isJsFunction(x)
}
registerFn('procedure?', procedureQ, Prelude)

function apply (f: V.Closure | Function, args: Value[]): Value {
  throw new ScamperError ('Runtime', 'apply unimplemented')
}
registerFn('apply', apply, Prelude)

function stringMap (f: V.Closure | Function, s: string): string {
  throw new ScamperError ('Runtime', 'string-map unimplemented')
}
registerFn('string-map', stringMap, Prelude)

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

const map = (f: V.Closure | Function, ...lsts: V.List[]): V.List => {
  throw new ScamperError ('Runtime', 'map unimplemented')
}
registerFn('map', map, Prelude)

/*
const mapPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('map', ['procedure?', 'list?'], 'list?', args, app).asyncAndThen(async _ => {
    const fn = args[0]
    // N.B., non-recursive, linear-time map when only one list is involved
    if (args.length === 2) {
      const list = args[1]
      if (list === null) {
        return Promise.resolve(ok(null))
      } else {
        let cur = list as L.PairType
        const ret = L.vpair(cur.fst, null) as L.PairType
        let result = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(cur.fst)]))
        if (result.tag === 'error') {
          return Promise.resolve(result)
        } else {
          ret.fst = result.value
        }
        let curRet: L.PairType = ret
        while (cur.snd !== null) {
          cur = cur.snd as L.PairType
          const newNode = L.vpair(null, null) as L.PairType
          result = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(cur.fst)]))
          if (result.tag === 'error') {
            return Promise.resolve(result)
          } else {
            newNode.fst = result.value
          }
          curRet.snd = newNode
          curRet = newNode
        }
        return Promise.resolve(ok(ret))
      }
    } else {
      const lists = args.slice(1).map(L.valueListToArray_)
      if (!(lists.map(l => l.length).every(n => n === lists[0].length))) {
        return runtimeError(msg('error-precondition-not-met', 'map', 2,
          'all lists have the same length', Pretty.expToString(0, app)), app)
      }
      const xs = transpose(lists)
      const exp = L.arrayToList(xs.map(vs => L.nlecall(L.nlevalue(fn), asValues(vs))))
      return evaluateExp(env, exp)
    }
  })
*/
// Additional list pipeline functions from racket/base

const filter = (f: V.Closure | Function, lst: V.List): V.List => {
  throw new ScamperError ('Runtime', 'filter unimplemented')
}
registerFn('filter', filter, Prelude)

const fold = (f: V.Closure | Function, init: Value, lst: V.List): Value => {
  throw new ScamperError ('Runtime', 'fold unimplemented')
}
registerFn('fold', fold, Prelude)

const reduce = (f: V.Closure | Function, lst: V.List): Value => {
  throw new ScamperError ('Runtime', 'reduce unimplemented')
}
registerFn('reduce', reduce, Prelude)

const foldLeft = (f: V.Closure | Function, init: Value, lst: V.List): Value => {
  throw new ScamperError ('Runtime', 'fold-left unimplemented')
}
registerFn('fold-left', foldLeft, Prelude)

const foldRight = (f: V.Closure | Function, init: Value, lst: V.List): Value => {
  throw new ScamperError ('Runtime', 'fold-right unimplemented')
}
registerFn('fold-right', foldRight, Prelude)

const reduceRight = (f: V.Closure | Function, lst: V.List): Value => {
  throw new ScamperError ('Runtime', 'reduce-right unimplemented')
}
registerFn('reduce-right', reduceRight, Prelude)

const vectorMap = (f: V.Closure | Function, ...vecs: Value[][]): Value[] => {
  throw new ScamperError ('Runtime', 'vector-map unimplemented')
}
registerFn('vector-map', vectorMap, Prelude)

const vectorMapBang = (f: V.Closure | Function, vec: Value[]): void => {
  throw new ScamperError ('Runtime', 'vector-map! unimplemented')
}
registerFn('vector-map!', vectorMapBang, Prelude)

const vectorForEach = (f: V.Closure | Function, vec: Value[]): void => {
  throw new ScamperError ('Runtime', 'vector-for-each unimplemented')
}
registerFn('vector-for-each', vectorForEach, Prelude)

const forRange = (start: number, end: number, f: V.Closure | Function): void => {
  throw new ScamperError ('Runtime', 'for-range unimplemented')
}
registerFn('for-range', forRange, Prelude)

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

const vectorFilter = (f: V.Closure | Function, lst: Value[]): Value[][] => {
  throw new ScamperError ('Runtime', 'vector-filter unimplemented')
}
registerFn('vector-filter', vectorFilter, Prelude)

// TODO: implement fold/reduce variants for vectors

function voidQ (x: any): boolean {
  checkContract(arguments, contract('void?', [C.any]))
  return x === undefined
}
registerFn('void?', voidQ, Prelude)

function error (msg: string): never {
  checkContract(arguments, contract('error', [C.string]))
  throw new ScamperError ('Runtime', msg)
}
registerFn('error', error, Prelude)

function qq (): never {
  checkContract(arguments, contract('??', []))
  throw new ScamperError ('Runtime', 'Hole encountered in program!')
}

const compose = (...fs: (V.Closure | Function)[]): V.Closure | Function => {
  throw new Error ('compose unimplemented')
}
registerFn('compose', compose, Prelude)

const pipe = (init: Value, ...fs: (V.Closure | Function)[]): V.Closure | Function => {
  throw new Error ('|> unimplemented')
}
registerFn('|>', pipe, Prelude)

const range = (m: number, n: number, step: number): number[] => {
  throw new Error ('range unimplemented')
}
registerFn('range', range, Prelude)

/*
const rangePrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('range', [], 'number?', args, app).andThen(_ => {
    if (args.length === 0 || args.length > 3) {
      return runtimeError(msg('error-arity', 'range', '1--3', args.length), app)
    } else {
      const m = args.length === 1 ? 0 : args[0] as number
      const n = args.length === 1 ? args[0] as number : args[1] as number
      const step = args.length < 3 ? 1 : args[2] as number
      const arr = []
      // N.B., to prevent the internal infinite loop that would result
      // from having a zero step.
      if (step === 0) {
        return runtimeError(msg('error-precondition-not-met', 'range', '3', 'non-zero', step), app)
      }
      for (let i = m; step > 0 ? i < n : i > n; i += step) {
        arr.push(i)
      }
      return ok(L.valueArrayToList(arr))
    }
  }))
*/

function random (n: number): number {
  checkContract(arguments, contract('random', [C.integer])) 
  return Math.floor(Math.random() * n)
}
registerFn('random', random, Prelude)



function withHandler (handler: V.Closure | Function, fn: V.Closure | Function, ...args: Value[]): Value {
  checkContract(arguments, contract('with-handler', [C.func, C.func], C.any))
  throw new ScamperError('Runtime', 'with-handler unimplemented')
}
registerFn('with-handler', withHandler, Prelude)

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

// Additional constants

const elseConst = true
const piConst = Math.PI
const voidConst = undefined

Prelude.push(['else', elseConst])
Prelude.push(['pi', piConst])
Prelude.push(['π', piConst])
Prelude.push(['void', voidConst])

export default Prelude
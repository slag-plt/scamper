import * as R from '../lpm/runtime.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { callFunction } from '../sem.js'
import * as Display from '../display.js'

export const Prelude: R.Library = new R.Library()

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
  return R.equals(x, y)
}
Prelude.registerValue('equal?', equalQ)

// Numbers (6.2)

function numberQ (x: any): boolean {
  checkContract(arguments, query1C('number?'))
  return typeof x === 'number'
}
Prelude.registerValue('number?', numberQ)

function realQ (x: any): boolean {
  checkContract(arguments, query1C('real?'))
  return typeof x === 'number' && !Number.isInteger(x)
}
Prelude.registerValue('real?', realQ)

function integerQ (x: any): boolean {
  checkContract(arguments, query1C('integer?'))
  return typeof x === 'number' && Number.isInteger(x)
}
Prelude.registerValue('integer?', integerQ)

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
Prelude.registerValue('nan?', nanQ)

function lt (x: number, y: number): boolean {
  checkContract(arguments, contract('<', [C.number, C.number]))
  return x < y
}
Prelude.registerValue('<', lt)

function leq (x: number, y: number): boolean {
  checkContract(arguments, contract('<=', [C.number, C.number]))
  return x <= y
}
Prelude.registerValue('<=', leq)

function gt (x: number, y: number): boolean {
  checkContract(arguments, contract('>', [C.number, C.number]))
  return x > y
}
Prelude.registerValue('>', gt)

function geq (x: number, y: number): boolean {
  checkContract(arguments, contract('>=', [C.number, C.number]))
  return x >= y
}
Prelude.registerValue('>=', geq)

function eq (x: number, y: number): boolean {
  checkContract(arguments, contract('=', [C.number, C.number]))
  return x === y
}
Prelude.registerValue('=', eq)

function eqEps (eps: number): R.ScamperFn {
  checkContract(arguments, contract('=-eps', [C.number]))
  const eq = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-eps`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  }
  R.nameFn(`(=-eps ${eps})`, eq)
  return eq
}
Prelude.registerValue('=-eps', eqEps)

function zeroQ (x: number): boolean {
  checkContract(arguments, contract('zero?', [C.number]))
  return x === 0
}
Prelude.registerValue('zero?', zeroQ)

function positiveQ (x: number): boolean {
  checkContract(arguments, contract('positive?', [C.number]))
  return x > 0
}
Prelude.registerValue('positive?', positiveQ)

function negativeQ (x: number): boolean {
  checkContract(arguments, contract('negative?', [C.number]))
  return x < 0
}
Prelude.registerValue('negative?', negativeQ)

function oddQ (x: number): boolean {
  checkContract(arguments, contract('odd?', [C.integer]))
  return (x & 1) === 1
}
Prelude.registerValue('odd?', oddQ)

function evenQ (x: number): boolean {
  checkContract(arguments, contract('even?', [C.integer]))
  return (x & 1) !== 1
}
Prelude.registerValue('even?', evenQ)

function max (...xs: number[]): number {
  checkContract(arguments, contract('max', [], C.number))
  return Math.max(...xs)
}
Prelude.registerValue('max', max)

function min (...xs: number[]): number {
  checkContract(arguments, contract('min', [], C.number))
  return Math.min(...xs)
}
Prelude.registerValue('min', min)

function plus (...xs: number[]): number {
  checkContract(arguments, contract('+', [], C.number))
  return xs.reduce((a, b) => a + b, 0)
}
Prelude.registerValue('+', plus)

function minus (...xs: number[]): number {
  checkContract(arguments, contract('-', [C.number], C.number))
  return xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
}
Prelude.registerValue('-', minus)

function times (...xs: number[]): number {
  checkContract(arguments, contract('*', [], C.number))
  return xs.reduce((a, b) => a * b, 1)
}
Prelude.registerValue('*', times)

function div (...xs: number[]): number {
  checkContract(arguments, contract('/', [C.number], C.number))
  return xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
}
Prelude.registerValue('/', div)

function abs (x: number): number {
  checkContract(arguments, contract('abs', [C.number]))
  return Math.abs(x)
}
Prelude.registerValue('abs', abs)

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
Prelude.registerValue('quotient', quotient)

function remainder (x: number, y: number): number {
  checkContract(arguments, contract('remainder', [C.integer, C.integer]))
  return x % y
}
Prelude.registerValue('remainder', remainder)

function modulo (x: number, y: number): number {
  checkContract(arguments, contract('modulo', [C.integer, C.integer]))
  return ((x % y) + y) % y
}
Prelude.registerValue('modulo', modulo)

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
Prelude.registerValue('floor', floor)

function ceiling (x: number): number {
  checkContract(arguments, contract('ceiling', [C.number]))
  return Math.ceil(x)
}
Prelude.registerValue('ceiling', ceiling)

function truncate (x: number): number {
  checkContract(arguments, contract('truncate', [C.number]))
  return Math.trunc(x)
}
Prelude.registerValue('truncate', truncate)

function round (x: number): number {
  checkContract(arguments, contract('round', [C.number]))
  return Math.round(x)
}
Prelude.registerValue('round', round)

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

function square (x: number): number {
  checkContract(arguments, contract('square', [C.number]))
  return x * x
}
Prelude.registerValue('square', square)

function sqrt (x: number): number {
  checkContract(arguments, contract('sqrt', [C.number]))
  return Math.sqrt(x)
}
Prelude.registerValue('sqrt', sqrt)

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

function expt (x: number, y: number): number {
  checkContract(arguments, contract('expt', [C.number, C.number]))
  return Math.pow(x, y)
}
Prelude.registerValue('expt', expt)

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
Prelude.registerValue('number->string', numberToString)

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
Prelude.registerValue('string->number', stringToNumber)

// Additional functions from racket/base

function exp (x: number): number {
  checkContract(arguments, contract('exp', [C.number]))
  return Math.exp(x)
}
Prelude.registerValue('exp', exp)

function log (x: number): number {
  checkContract(arguments, contract('log', [C.number]))
  return Math.log(x)
}
Prelude.registerValue('log', log)

function sin (x: number): number {
  checkContract(arguments, contract('sin', [C.number]))
  return Math.sin(x)
}
Prelude.registerValue('sin', sin)

function cos (x: number): number {
  checkContract(arguments, contract('cos', [C.number]))
  return Math.cos(x)
}
Prelude.registerValue('cos', cos)

function tan (x: number): number {
  checkContract(arguments, contract('tan', [C.number]))
  return Math.tan(x)
}
Prelude.registerValue('tan', tan)

function asin (x: number): number {
  checkContract(arguments, contract('asin', [C.number]))
  return Math.asin(x)
}
Prelude.registerValue('asin', asin)

function acos (x: number): number {
  checkContract(arguments, contract('acos', [C.number]))
  return Math.acos(x)
}
Prelude.registerValue('acos', acos)

function atan (x: number): number {
  checkContract(arguments, contract('atan', [C.number]))
  return Math.atan(x)
}
Prelude.registerValue('atan', atan)

function equalsEps (eps: number): R.Value {
  checkContract(arguments, contract('=-eps', [C.number]))
  const name = `=-(${eps})`
  const ret = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-(${eps})`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  }
  R.nameFn(name, ret)
  return ret
}
Prelude.registerValue('=-eps', equalsEps)

// Booleans (6.3)

function not (x: boolean): boolean {
  checkContract(arguments, contract('not', [C.boolean]))
  return !x
}
Prelude.registerValue('not', not)

function booleanQ (x: any): boolean {
  checkContract(arguments, contract('boolean?', [C.any]))
  return typeof x === 'boolean'
}
Prelude.registerValue('boolean?', booleanQ)

// From racket/base

function nand (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nand', [], C.boolean))
  return !xs.reduce((a, b) => a && b, true)
}
Prelude.registerValue('nand', nand)

function nor (...xs: boolean[]): boolean {
  checkContract(arguments, contract('nor', [], C.boolean))
  return !xs.reduce((a, b) => a || b, false)
}
Prelude.registerValue('nor', nor)

function implies (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('implies', [C.boolean, C.boolean]))
  return !x || y
}
Prelude.registerValue('implies', implies)

function xor (x: boolean, y: boolean): boolean {
  checkContract(arguments, contract('xor', [C.boolean, C.boolean]))
  return (x && !y) || (!x && y)
}
Prelude.registerValue('xor', xor)

// Additional functions

function anyOf(...fns: R.ScamperFn[]): R.ScamperFn {
  checkContract(arguments, contract('any-of', [], C.func))
  return function (v: any): boolean {
    checkContract(arguments, contract(`any-of`, [C.any]))
    for (let i = 0; i < fns.length; i++) {
      if (callFunction(fns[i], v)) {
        return true
      }
    }
    return false
  }
}
Prelude.registerValue('any-of', anyOf)

function allOf(...fns: R.ScamperFn[]): R.ScamperFn {
  checkContract(arguments, contract('any-of', [], C.func))
  return function (v: any): boolean {
    checkContract(arguments, contract(`any-of`, [C.any]))
    for (let i = 0; i < fns.length; i++) {
      if (!callFunction(fns[i], v)) {
        return false
      }
    }
    return true
  }
}
Prelude.registerValue('all-of', allOf)

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
  return R.isPair(x)
}
Prelude.registerValue('pair?', pairQ)

function listOf (pred: R.ScamperFn): R.ScamperFn {
  checkContract(arguments, contract('list-of', [C.func]))
  return function (l: R.List): boolean {
    // N.B., list-of returns false if the input is _not_ a list
    if (!listQ(l)) { return false; }
    while (l !== null) {
      if (!callFunction(pred, l.fst)) {
        return false
      }
      l = l.snd as R.List
    }
    return true
  }
}
Prelude.registerValue('list-of', listOf)

function cons (x: any, y: any): R.Value {
  checkContract(arguments, contract('cons', [C.any, C.any]))
  return R.mkPair(x, y)
}
Prelude.registerValue('cons', cons)

function pair (x: any, y: any): R.Value {
  checkContract(arguments, contract('pair', [C.any, C.any]))
  return R.mkPair(x, y)
}
Prelude.registerValue('pair', pair)

function car (x: R.Value): R.Value {
  checkContract(arguments, contract('car', [C.pair]))
  return (x as any).fst
}
Prelude.registerValue('car', car)

function cdr (x: R.Value): R.Value {
  checkContract(arguments, contract('cdr', [C.pair]))
  return (x as any).snd
}
Prelude.registerValue('cdr', cdr)

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

function nullQ (x: any): boolean {
  checkContract(arguments, contract('null?', [C.any]))
  return x === null
}
Prelude.registerValue('null?', nullQ)

function listQ (x: any): boolean {
  checkContract(arguments, contract('list?', [C.any]))
  return x === null || (R.isPair(x) && (x as any).isList)
}
Prelude.registerValue('list?', listQ)

function list (...xs: R.Value[]): R.List {
  checkContract(arguments, contract('list', [], C.any))
  let ret: R.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = R.mkPair(xs[i], ret)
  }
  return ret
}
Prelude.registerValue('list', list)


function makeList (n: number, fill: R.Value): R.List {
  checkContract(arguments, contract('make-list', [C.integer, C.any]))
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = R.mkPair(fill, ret)
  }
  return ret
}
Prelude.registerValue('make-list', makeList)

function length (l: R.List): number {
  checkContract(arguments, contract('length', [C.list]))
  let len = 0
  while (l !== null) {
    len += 1
    l = (l.snd as R.List)
  }
  return len
}
Prelude.registerValue('length', length)

function appendOne_ (l1: R.List, l2: R.List): R.List {
  if (l1 === null) {
    return l2
  } else {
    let head = R.mkPair(l1.fst, null)
    let last = head
    let cur = l1.snd as R.List
    while (cur !== null) {
      last.snd = R.mkPair(cur.fst, null)
      last = last.snd as R.Pair
      cur = cur.snd as R.List
    }
    last.snd = l2
    return head
  }
}

function append (l: R.List, ...ls: R.List[]): R.List {
  checkContract(arguments, contract('append', [C.list], C.list))
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret 
}
Prelude.registerValue('append', append)

function reverse (l: R.List): R.List {
  checkContract(arguments, contract('reverse', [C.list]))
  const queue = []
  while (l !== null) {
    queue.push(l)
    l = l.snd as R.List
  }
  queue.reverse()
  let ret = null
  while (queue.length > 0) {
    const next = queue.pop() as R.Pair
    ret = R.mkPair(next.fst, ret)
  }
  return ret
}
Prelude.registerValue('reverse', reverse)

function listTail (l: R.List, k: number): R.List {
  checkContract(arguments, contract('list-tail', [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.snd as R.List
    k -= 1
  }
  return l
}
Prelude.registerValue('list-tail', listTail)

function listTake (l: R.List, k: number): R.List {
  checkContract(arguments, contract('list-take', [C.list, C.nonneg]))
  let elts = []
  // N.B., push in reverse order so we built the list right-to-left
  while (l !== null && k > 0) {
    elts.push(l.fst)
    l = l.snd as R.List
    k -= 1
  }
  let ret: R.List = null
  for (let i = elts.length - 1; i >= 0; i--) {
    ret = R.mkPair(elts[i], ret)
  }
  return ret
}
Prelude.registerValue('list-take', listTake)

function listDrop (l: R.List, k: number): R.List {
  checkContract(arguments, contract('list-drop', [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.snd as R.List
    k -= 1
  }
  return l
}
Prelude.registerValue('list-drop', listDrop)

function listRef (l: R.List, n: number): R.Value {
  checkContract(arguments, contract('list-ref', [C.list, C.nonneg]))
  let i = n
  while (l !== null && i > 0) {
    l = l.snd as R.List
    i -= 1
  }
  if (l === null) {
    throw new R.ScamperError('Runtime', `list-ref: index ${n} out of bounds of list`)
  } else {
    return l.fst
  }
}
Prelude.registerValue('list-ref', listRef)

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

function indexOf (l: R.List, v: R.Value): number {
  checkContract(arguments, contract('index-of', [C.list, C.any]))
  let i = 0
  while (l !== null) {
    if (R.equals(l.fst, v)) {
      return i
    }
    l = l.snd as R.List
    i += 1
  }
  return -1
}
Prelude.registerValue('index-of', indexOf)

function assocKey (v: R.Value, l: R.List): boolean {
  checkContract(arguments, contract('assoc-key?', [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (R.equals((l.fst as R.Pair).fst, v)) {
      return true
    }
    l = l.snd as R.List
  }
  return false
}
Prelude.registerValue('assoc-key?', assocKey)

function assocRef (v: R.Value, l: R.List): R.Value {
  checkContract(arguments, contract('assoc-ref', [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (R.equals((l.fst as R.Pair).fst, v)) {
      return (l.fst as R.Pair).snd
    }
    l = l.snd as R.List
  }
  throw new R.ScamperError('Runtime', `assoc-ref: key ${v} not found in association list`)
}
Prelude.registerValue('assoc-ref', assocRef)

function assocSet (k: R.Value, v: R.Value, l: R.List): R.List {
  checkContract(arguments, contract('assoc-set', [C.any, C.any, C.listof(C.pair)]))
  const front = []
  // TODO: implement me—this isn't the right implementation!
  while (l !== null) {
    const entry = l.fst as R.Pair
    if (R.equals(entry.fst, k)) {
      front.push(R.mkPair(k, v))
      let ret = l.snd as R.List
      for (let i = front.length - 1; i >= 0; i--) {
        ret = R.mkPair(front[i], ret)
      }
      return ret
    } else {
      front.push(l.fst)
      l = l.snd as R.List
    }
  }
  return R.vectorToList(front.concat([R.mkPair(k, v)]))
}
Prelude.registerValue('assoc-set', assocSet)

// Miscellaneous list functions

function sort(l: R.List, lt: R.ScamperFn): R.List {
  checkContract(arguments, contract('sort', [C.list, C.func]))
  const arr = R.listToVector(l)
  arr.sort((a, b) => {
    const result = callFunction(lt, a, b)
    if (typeof result !== 'boolean') {
      throw new R.ScamperError('Runtime', `sort: comparator function must return a number`)
    } else {
      return result ? -1 : 1
    }
  })
  return R.vectorToList(arr)
}
Prelude.registerValue('sort', sort)

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
  return R.isChar(x)
}
Prelude.registerValue('char?', charQ)

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
  const fn = function (...args: R.Value[]) {
    checkContract(arguments, contract(name, [], C.char))
    return pairwiseSatisfies((a, b) => f((a as R.Char).value, (b as R.Char).value), args)
  }
  R.nameFn(name, fn)
  Prelude.registerValue(name, fn)
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
  const fn = function (x: R.Char) {
    checkContract(arguments, contract(name, [], C.char))
    return f(x.value)
  }
  R.nameFn(name, fn)
  Prelude.registerValue(name, fn)
}

mkCharPredicatePrim('char-alphabetic?', (a) => /\p{L}/gu.test(a))
mkCharPredicatePrim('char-numeric?', (a) => /\p{N}/gu.test(a))
mkCharPredicatePrim('char-whitespace?', (a) => /\p{Z}/gu.test(a))
mkCharPredicatePrim('char-upper-case?', (a) => /\p{Lu}/gu.test(a))
mkCharPredicatePrim('char-lower-case?', (a) => /\p{Ll}/gu.test(a))

function digitalValue (c: R.Char): number {
  checkContract(arguments, contract('digit-value', [], C.char))
  const n = parseInt(c.value, 10)
  if (isNaN(n)) {
    throw new R.ScamperError('Runtime', `digit-value: ${c.value} is not a decimal digit`)
  } else {
    return n
  }
}
Prelude.registerValue('digit-value', digitalValue)

function charToInteger (c: R.Char): number {
  checkContract(arguments, contract('char->integer', [], C.char))
  return c.value.codePointAt(0)!
}
Prelude.registerValue('char->integer', charToInteger)

function integerToChar (n: number): R.Char {
  checkContract(arguments, contract('integer->char', [C.integer]))
  return R.mkChar(String.fromCodePoint(n))
}
Prelude.registerValue('integer->char', integerToChar)

function charUpcase (c: R.Char): R.Char {
  checkContract(arguments, contract('char-upcase?', [], C.char))
  return R.mkChar(c.value.toUpperCase())
}
Prelude.registerValue('char-upcase', charUpcase)

function charDowncase (c: R.Char): R.Char {
  checkContract(arguments, contract('char-downcase?', [], C.char))
  return R.mkChar(c.value.toLowerCase())
}
Prelude.registerValue('char-downcase', charDowncase)

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
function charFoldcase (c: R.Char): R.Char {
  checkContract(arguments, contract('char-foldcase?', [], C.char))
  return R.mkChar(c.value.toLowerCase())
}
Prelude.registerValue('char-foldcase', charFoldcase)

// Strings (6.7)

function stringQ (x: any): boolean {
  checkContract(arguments, contract('string?', [C.any]))
  return typeof x === 'string'
}
Prelude.registerValue('string?', stringQ)

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
function makeString (k: number, c: R.Char): string {
  checkContract(arguments, contract('make-string', [C.integer, C.char]))
  return c.value.repeat(k)
}
Prelude.registerValue('make-string', makeString)

function string (c: R.Char, ...cs: R.Char[]): string {
  checkContract(arguments, contract('string', [C.char], C.char))
  return [c, ...cs].map((e) => e.value).join('')
}
Prelude.registerValue('string', string)

function stringLength (s: string): number {
  checkContract(arguments, contract('string-length', [C.string]))
  return s.length
}
Prelude.registerValue('string-length', stringLength)

function stringRef (s: string, i: number): R.Char {
  checkContract(arguments, contract('string-ref', [C.string, C.integer]))
  return R.mkChar(s[i])
}
Prelude.registerValue('string-ref', stringRef)

// N.B., string-set! is unimplemented since it is effectful.

function mkStringCompareFn (name: string, f: (a: string, b: string) => boolean): void {
  const fn = function (...args: string[]) {
    checkContract(arguments, contract(name, [], C.string))
    return pairwiseSatisfies((a, b) => f(a, b), args)
  }
  R.nameFn(name, fn)
  Prelude.registerValue(name, fn)
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
Prelude.registerValue('string-upcase', stringUpcase)

function stringDowncase (s: string): string {
  checkContract(arguments, contract('string-downcase', [C.string])) 
  return s.toLowerCase()
}
Prelude.registerValue('string-downcase', stringDowncase)

function stringFoldcase (s: string): string {
  checkContract(arguments, contract('string-foldcase', [C.string])) 
  return s.toLowerCase()
}
Prelude.registerValue('string-foldcase', stringFoldcase)

function substring (s: string, start: number, end: number): string {
  checkContract(arguments, contract('substring', [C.string, C.integer, C.integer])) 
  return s.substring(start, end)
}
Prelude.registerValue('substring', substring)

function stringAppend (...args: string[]): string {
  checkContract(arguments, contract('string-append', [], C.string))
  return args.join('')
}
Prelude.registerValue('string-append', stringAppend)

// TODO: stringToList has a 3-argument version, too, that specifies
// a substring of s to turn into a list.
function stringToList (s: string): R.List {
  checkContract(arguments, contract('string->list', [C.string]))
  let ret = null
  for (let i = s.length - 1; i >= 0; i--) {
    ret = R.mkPair(R.mkChar(s[i]), ret)
  }
  return ret
}
Prelude.registerValue('string->list', stringToList)

function listToString (l: R.List): string {
  checkContract(arguments, contract('list->string', [C.list]))
  let ret = ''
  while (l !== null) {
    if (!R.isChar(l.fst)) {
      throw new R.ScamperError('Runtime', `list->string: list contains non-character element: ${R.typeOf(l.fst)}`)
    }
    ret += (l.fst as R.Char).value
    l = l.snd as R.List
  } 
  return ret
}
Prelude.registerValue('list->string', listToString)

function stringToVector (s: string): R.Char[] {
  checkContract(arguments, contract('string->vector', [C.string]))
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(R.mkChar(s[i]))
  }
  return ret
}
Prelude.registerValue('string->vector', stringToVector)

function vectorToString (v: R.Char[]): string {
  checkContract(arguments, contract('vector->string', [C.vector]))
  let ret = ''
  for (let i = 0; i < v.length; i++) {
    ret += v[i].value
  }
  return ret
}
Prelude.registerValue('vector->string', vectorToString)

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
Prelude.registerValue('string-contains', stringContains)

function stringSplit (s: string, sep: string): R.List {
  checkContract(arguments, contract('string-split', [C.string, C.string]))
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = R.mkPair(splits[i], ret)
  }
  return ret
}
Prelude.registerValue('string-split', stringSplit)


function stringSplitVector (s: string, sep: string): string[] {
  checkContract(arguments, contract('string-split-vector', [C.string, C.string])) 
  return s.split(sep)
}
Prelude.registerValue('string-split-vector', stringSplitVector)

// TODO: what should the type of a reactive-file object be? A struct? Or a JS object?
// TODO: need to add a custom renderer for reactive file blobs

export interface ReactiveFile extends R.Struct {
  [R.structKind]: 'reactive-file',
  callback: R.ScamperFn
}

function withFile (callback: R.ScamperFn): ReactiveFile {
  checkContract(arguments, contract('with-file', [C.func]))  
  return {
    [R.scamperTag]: 'struct',
    [R.structKind]: 'reactive-file',
    callback
  }
}
Prelude.registerValue('with-file', withFile)

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
          const v = callFunction(rf.callback, e.target.result as string)
          outp.appendChild(Display.renderToHTML(v))
        } catch (e) {
          outp.appendChild(Display.renderToHTML(e as R.ScamperError))
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
  (v) => R.isStructKind(v, 'reactive-file'), renderReactiveFile)

// Vectors (6.8)

function vectorQ (x: any): boolean {
  checkContract(arguments, contract('vector?', [C.any]))
  return R.isArray(x)
}
Prelude.registerValue('vector?', vectorQ)

function vector (...xs: R.Value[]): R.Value[] {
  checkContract(arguments, contract('vector', [], C.any))
  return xs
}
Prelude.registerValue('vector', vector)

function makeVector (n: number, fill: R.Value): R.Value[] {
  checkContract(arguments, contract('make-vector', [C.integer, C.any]))
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}
Prelude.registerValue('make-vector', makeVector)

function vectorLength (v: R.Value[]): number {
  checkContract(arguments, contract('vector-length', [C.vector])) 
  return v.length
}
Prelude.registerValue('vector-length', vectorLength)

function vectorRef (v: R.Value[], i: number): R.Value {
  checkContract(arguments, contract('vector-ref', [C.vector, C.integer]))
  return v[i]
}
Prelude.registerValue('vector-ref', vectorRef)

function vectorSet (v: R.Value[], i: number, x: R.Value): void {
  checkContract(arguments, contract('vector-set!', [C.vector, C.integer, C.any]))
  v[i] = x
}
Prelude.registerValue('vector-set!', vectorSet)

function vectorFill (v: R.Value[], x: R.Value): void {
  checkContract(arguments, contract('vector-fill!', [C.vector, C.any]))
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}
Prelude.registerValue('vector-fill!', vectorFill)

function vectorToList (v: R.Value[]): R.List {
  checkContract(arguments, contract('vector->list', [C.vector]))
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = R.mkPair(v[i], ret)
  }
  return ret
}
Prelude.registerValue('vector->list', vectorToList)

function listToVector (l: R.List): R.Value[] {
  checkContract(arguments, contract('list->vector', [C.list]))
  const ret = []
  while (l !== null) {
    ret.push(l.fst)
    l = l.snd as R.List
  }
  return ret
}
Prelude.registerValue('list->vector', listToVector)

function vectorRange (...args: number[]): number[] {
  checkContract(arguments, contract('vector-range', [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new R.ScamperError('Runtime', '1, 2, or 3 numbers must be passed to function')
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new R.ScamperError('Runtime', '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return arr
  }
}
Prelude.registerValue('vector-range', vectorRange)

function vectorAppend (...vecs: R.Value[][]): R.Value[] {
  checkContract(arguments, contract('vector-append', [], C.vector))
  const arr = []
  for (let i = 0; i < vecs.length; i++) {
    for (let j = 0; j < vecs[i].length; j++) {
      arr.push(vecs[i][j])
    }
  }
  return arr
}
Prelude.registerValue('vector-append', vectorAppend)

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

function procedureQ (x: any): boolean {
  checkContract(arguments, contract('procedure?', [C.any]))
  return R.isClosure(x) || R.isJsFunction(x)
}
Prelude.registerValue('procedure?', procedureQ)

function apply (f: R.Closure | Function, args: R.List): R.Value {
  checkContract(arguments, contract('apply', [C.func, C.list]))

  return callFunction(f, ...R.listToVector(args))
}
Prelude.registerValue('apply', apply)

function stringMap (f: R.Closure | Function, s: string): string {
  checkContract(arguments, contract('string-map', [C.func, C.string]))
  let chs = []
  for (let i = 0; i < s.length; i++) {
    chs.push(R.mkChar(s[i]))
  }
  return chs.map((c) => callFunction(f, c).value).join('')
}
Prelude.registerValue('string-map', stringMap)

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

function mapOne (f: R.Closure | Function, l: R.List): R.List {
  const values = []
  while (l !== null) {
    values.push(callFunction(f, l.fst))
    l = l.snd as R.Pair
  }
  return R.vectorToList(values)
}

function map (f: R.Closure | Function, ...lsts: R.List[]): R.List {
  checkContract(arguments, contract('map', [C.func], C.list))
  if (lsts.length === 0) {
    return null
  } else if (lsts.length === 1) {
    return mapOne(f, lsts[0])
  } else {
    const lists = lsts.map(R.listToVector)
    if (!(lists.map(l => l.length).every(n => n === lists[0].length))) {
      throw new R.ScamperError('Runtime', 'the lists passed to the function call do not have the same length')
    }
    const xs = transpose(lists)
    return R.vectorToList(xs.map(vs => callFunction(f, ...vs)))
  }
}
Prelude.registerValue('map', map)

// Additional list pipeline functions from racket/base

function filter (f: R.Closure | Function, lst: R.List): R.List {
  checkContract(arguments, contract('filter', [C.func, C.list]))
  const values = []
  while (lst !== null) {
    if (callFunction(f, lst.fst)) {
      values.push(lst.fst)
    }
    lst = lst.snd as R.Pair
  }
  return R.vectorToList(values) 
}
Prelude.registerValue('filter', filter)

function fold (f: R.Closure | Function, init: R.Value, lst: R.List): R.Value {
  checkContract(arguments, contract('fold', [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as R.Pair
  }
  return acc
}
Prelude.registerValue('fold', fold)

function reduce (f: R.Closure | Function, lst: R.List): R.Value {
  checkContract(arguments, contract('reduce', [C.func, C.nonemptyList]))
  let acc = (lst as R.Pair).fst
  lst = (lst as R.Pair).snd as R.Pair
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as R.Pair
  }
  return acc
}
Prelude.registerValue('reduce', reduce)

function foldLeft (f: R.Closure | Function, init: R.Value, lst: R.List): R.Value {
  checkContract(arguments, contract('fold-left', [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = callFunction(f, acc, lst.fst)
    lst = lst.snd as R.Pair
  }
  return acc
}
Prelude.registerValue('fold-left', foldLeft)

function foldRight (f: R.Closure | Function, init: R.Value, lst: R.List): R.Value {
  checkContract(arguments, contract('fold-right', [C.func, C.any, C.list]))
  const values = R.listToVector(lst)
  let acc = init
  for (let i = values.length - 1; i >= 0; i--) {
    acc = callFunction(f, values[i], acc)
  }
  return acc
}
Prelude.registerValue('fold-right', foldRight)

function reduceRight (f: R.Closure | Function, lst: R.List): R.Value {
  checkContract(arguments, contract('reduce-right', [C.func, C.nonemptyList]))
  const values = R.listToVector(lst)
  let acc = values.pop()
  for (let i = values.length - 1; i >= 0; i--) {
    acc = callFunction(f, values[i], acc)
  }
  return acc
}
Prelude.registerValue('reduce-right', reduceRight)

function vectorMap (f: R.Closure | Function, ...vecs: R.Value[][]): R.Value[] {
  checkContract(arguments, contract('vector-map', [C.func], C.vector))
  if (vecs.length === 0) {
    return []
  } else if (vecs.length === 1) {
    return vecs[0].map((v) => callFunction(f, v))
  } else {
    if (!(vecs.map(l => l.length).every(n => n === vecs[0].length))) {
      throw new R.ScamperError('Runtime', 'the vectors passed to the function call do not have the same length')
    }
    const xs = transpose(vecs)
    return xs.map(vs => callFunction(f, ...vs))
  }
}
Prelude.registerValue('vector-map', vectorMap)

function vectorMapBang (f: R.Closure | Function, vec: R.Value[]): void {
  checkContract(arguments, contract('vector-map!', [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    vec[i] = callFunction(f, vec[i])
  }
}
Prelude.registerValue('vector-map!', vectorMapBang)

function vectorForEach (f: R.Closure | Function, vec: R.Value[]): void {
  checkContract(arguments, contract('vector-for-each', [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    callFunction(f, vec[i])
  }
}
Prelude.registerValue('vector-for-each', vectorForEach)

function forRange (start: number, end: number, f: R.Closure | Function): void {
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
Prelude.registerValue('for-range', forRange)

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

function vectorFilter (f: R.Closure | Function, lst: R.Value[]): R.Value[] {
  checkContract(arguments, contract('vector-filter', [C.func, C.vector]))
  const ret = []
  for (let i = 0; i < lst.length; i++) {
    if (callFunction(f, lst[i])) {
      ret.push(lst[i])
    }
  }
  return ret
}
Prelude.registerValue('vector-filter', vectorFilter)

// TODO: implement fold/reduce variants for vectors

function voidQ (x: any): boolean {
  checkContract(arguments, contract('void?', [C.any]))
  return x === undefined
}
Prelude.registerValue('void?', voidQ)

function error (msg: string): never {
  checkContract(arguments, contract('error', [C.string]))
  throw new R.ScamperError ('Runtime', msg)
}
Prelude.registerValue('error', error)

function qq (): never {
  checkContract(arguments, contract('??', []))
  throw new R.ScamperError ('Runtime', 'Hole encountered in program!')
}
Prelude.registerValue('??', qq)

function compose (...fss: (R.Closure | Function)[]): R.Closure | Function {
  checkContract(arguments, contract('compose', [C.func], C.func))
  let first = fss[fss.length - 1]
  return (x: R.Value) => {
    let ret = callFunction(first, x)
    for (let i = fss.length - 2; i >= 0; i--) {
      ret = callFunction(fss[i], ret)
    }
    return ret
  }
}
Prelude.registerValue('compose', compose)
Prelude.registerValue('o', compose)

function pipe (init: R.Value, ...fs: (R.Closure | Function)[]): R.Value {
  checkContract(arguments, contract('|>', [C.any, C.func], C.func))
  let acc = init
  for (let i = 0; i < fs.length; i++) {
    acc = callFunction(fs[i], acc)
  }
  return acc
}
Prelude.registerValue('|>', pipe)

function range (...args: number[]): R.List {
  checkContract(arguments, contract('range', [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new R.ScamperError('Runtime', '1, 2, or 3 numbers must be passed to function')
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new R.ScamperError('Runtime', '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return R.vectorToList(arr)
  }
}
Prelude.registerValue('range', range)

function random (n: number): number {
  checkContract(arguments, contract('random', [C.integer])) 
  return Math.floor(Math.random() * n)
}
Prelude.registerValue('random', random)

function withHandler (handler: R.Closure | Function, fn: R.Closure | Function, ...args: R.Value[]): R.Value {
  checkContract(arguments, contract('with-handler', [C.func, C.func], C.any))
  try {
    return callFunction(fn, ...args)
  } catch (e) {
    return callFunction(handler, (e as R.ScamperError).message)
  }
}
Prelude.registerValue('with-handler', withHandler)

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

// Additional Scamper-specific functions

function ignore (_v: R.Value): HTMLElement {
  checkContract(arguments, contract('ignore', [C.any]))
  const ret = document.createElement('div')
  ret.style.display = 'non'
  return ret
}
Prelude.registerValue('ignore', ignore)

function setMaximumRecursionDepth (n: number): any {
  checkContract(arguments, contract('set-maximum-recursion-depth', [C.nat]))
  return {
    [R.scamperTag]: 'set-maximum-recursion-depth',
    value: n
  }
}
Prelude.registerValue('set-maximum-recursion-depth!', setMaximumRecursionDepth)

function stringToWords (s: string): R.List {
  checkContract(arguments, contract('string->words', [C.string]))
  const words = s.split(/\s+/)
  for (let i = 0; i < words.length; i++) {
    words[i] = words[i].replace(/[.,;?:!]$/, '')
  }
  return R.vectorToList(words.filter((w) => w.length > 0))
}
Prelude.registerValue('string->words', stringToWords)

interface Ref extends R.Struct {
  [R.structKind]: 'ref',
  value: R.Value
}

function ref (v: R.Value): Ref {
  checkContract(arguments, contract('ref', [C.any]))
  return {
    [R.scamperTag]: 'struct',
    [R.structKind]: 'ref',
    value: v
  }
}
Prelude.registerValue('ref', ref)

function isRef (v: R.Value): boolean {
  return R.isStructKind(v, 'ref')
}
Prelude.registerValue('ref?', isRef)

function deref (r: Ref): R.Value {
  checkContract(arguments, contract('deref', [C.struct('ref')]))
  return r.value
}
Prelude.registerValue('deref', deref)

function refSet (r: Ref, v: R.Value): void {
  checkContract(arguments, contract('ref-set!', [C.struct('ref'), C.any]))
  r.value = v
}
Prelude.registerValue('ref-set!', refSet)

// Additional constants

const elseConst = true
const nullConst = null
const piConst = Math.PI
const voidConst = undefined

Prelude.registerValue('else', elseConst)
Prelude.registerValue('null', nullConst)
Prelude.registerValue('pi', piConst)
Prelude.registerValue('π', piConst)
Prelude.registerValue('void', voidConst)

export default Prelude
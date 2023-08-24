import { Value } from '../value.js'
import * as V from '../value.js'
import { callFunction } from '../sem.js'

// TODO:
//   + 1: implement higher-order functions once we factor out the runtime
//   + 2: implement error handling once we define an error type

const mkFn = V.mkJsFunction

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

const equalQ = (x: any, y: any): boolean => x === y

const equivalencePrimitives: [string, Value][] = [
  ['equal?', mkFn(equalQ, 2)]
]

// Numbers (6.2)

const numberQ = (x: any): boolean => typeof x === 'number'
const realQ = (x: any): boolean => typeof x === 'number' && !Number.isInteger(x)
const integerQ = (x: any): boolean => typeof x === 'number' && Number.isInteger(x)

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

const nanQ = (x: any): boolean => Number.isNaN(x)
const lt = (x: number, y: number): boolean => x < y
const leq = (x: number, y: number): boolean => x <= y
const gt = (x: number, y: number): boolean => x > y
const geq = (x: number, y: number): boolean => x >= y
const eq = (x: number, y: number): boolean => x === y
const zeroQ = (x: number): boolean => x === 0
const positiveQ = (x: number): boolean => x > 0
const negativeQ = (x: number): boolean => x < 0
const oddQ = (x: number): boolean => (x & 1) === 1
const evenQ = (x: number): boolean => (x & 1) !== 1
const max = (...xs: number[]): number => Math.max(...xs)
const min = (...xs: number[]): number => Math.min(...xs)
const plus = (...xs: number[]): number => xs.reduce((a, b) => a + b, 0)
const minus = (...xs: number[]): number => xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
const times = (...xs: number[]): number => xs.reduce((a, b) => a * b, 1)
const div = (...xs: number[]): number => xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
const abs = (x: number): number => Math.abs(x)

// N.B., not implementing the composite division functions:
//   (floor / n1 n2)
//   (floor-quotient n1 n2)
//   (floor-remainder n1 n2)
//   (truncate/ n1 n2)
//   (truncate-quotient n1 n2)
//   (truncate-remainder n1 n2)
// To avoid clutter in the documentation.

const quotient = (x: number, y: number): number => Math.trunc(x / y)
const remainder = (x: number, y: number): number => x % y
const modulo = (x: number, y: number): number => ((x % y) + y) % y

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)

// N.B., we don't implement:
//   (numerator q)
//   (denominator q)
// Since we don't implement rationals.

const floor = (x: number): number => Math.floor(x)
const ceiling = (x: number): number => Math.ceil(x)
const truncate = (x: number): number => Math.trunc(x)
const round = (x: number): number => Math.round(x)

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

const square = (x: number): number => Math.pow(x, 2)
const sqrt = (x: number): number => Math.sqrt(x)

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

const expt = (x: number, y: number): number => Math.pow(x, y)

// N.B., we don't implement:
//   (make-rectangular x1 x2)   ...probably not!
//   (make-polar x3 x4)         ...probably not!
//   (real-part z)              ...probably not!
//   (imag-part z)              ...probably not!
//   (magnitude z)              ...probably not!
//   (angle z)                  ...probably not!
// Because we don't implement complex numbers.

const numberToString = (x: number): string => x.toString()

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

const stringToNumber = (s: string): number => {
  if (/^[+-]?\d+$/.test(s)) {
    return parseInt(s)
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return parseFloat(s)
  } else {
    throw new Error(`Runtime error: string->number: invalid string: ${s}`)
  }
}

// Additional functions from racket/base

const exp = (x: number): number => Math.exp(x)
const log = (x: number): number => Math.log(x)
const sin = (x: number): number => Math.sin(x)
const cos = (x: number): number => Math.cos(x)
const tan = (x: number): number => Math.tan(x)
const asin = (x: number): number => Math.asin(x)
const acos = (x: number): number => Math.acos(x)
const atan = (x: number): number => Math.atan(x)
const equalsEps = (eps: number): Value => mkFn ((x: number, y: number): boolean => Math.abs(x - y) <= eps, 2)

const numericPrimitives: [string, Value][] = [
  ['number?', mkFn(numberQ, 1)],
  ['real?', mkFn(realQ, 1)],
  ['integer?', mkFn(integerQ, 1)],
  ['nan?', mkFn(nanQ, 1)],
  ['<', mkFn(lt, 2)],
  ['<=', mkFn(leq, 2)],
  ['>', mkFn(gt, 2)],
  ['>=', mkFn(geq, 2)],
  ['=', mkFn(eq, 2)],
  ['zero?', mkFn(zeroQ, 1)],
  ['positive?', mkFn(positiveQ, 1)],
  ['negative?', mkFn(negativeQ, 1)],
  ['odd?', mkFn(oddQ, 1)],
  ['even?', mkFn(evenQ, 1)],
  ['max', mkFn(max, 1, true)],
  ['min', mkFn(min, 1, true)],
  ['+', mkFn(plus, 1, true)],
  ['-', mkFn(minus, 1, true)],
  ['*', mkFn(times, 1, true)],
  ['/', mkFn(div, 1, true)],
  ['abs', mkFn(abs, 1)],
  ['quotient', mkFn(quotient, 2)],
  ['remainder', mkFn(remainder, 2)],
  ['modulo', mkFn(modulo, 2)],
  ['floor', mkFn(floor, 1)],
  ['ceiling', mkFn(ceiling, 1)],
  ['truncate', mkFn(truncate, 1)],
  ['round', mkFn(round, 1)],
  ['square', mkFn(square, 1)],
  ['sqrt', mkFn(sqrt, 1)],
  ['expt', mkFn(expt, 2)],
  ['number->string', mkFn(numberToString, 1)],
  ['string->number', mkFn(stringToNumber, 1)],
  ['exp', mkFn(exp, 1)],
  ['log', mkFn(log, 1)],
  ['sin', mkFn(sin, 1)],
  ['cos', mkFn(cos, 1)],
  ['tan', mkFn(tan, 1)],
  ['asin', mkFn(asin, 1)],
  ['acos', mkFn(acos, 1)],
  ['atan', mkFn(atan, 1)],
  ['=-eps', mkFn(equalsEps, 1)]
]

// Booleans (6.3)

const not = (x: boolean): boolean => !x
const booleanQ = (x: any): boolean => typeof x === 'boolean'

// From racket/base

const nand = (...xs: boolean[]): boolean => !xs.reduce((a, b) => a && b, true)
const nor = (...xs: boolean[]): boolean => !xs.reduce((a, b) => a || b, false)
const implies = (x: boolean, y: boolean): boolean => !x || y
const xor = (x: boolean, y: boolean): boolean => (x && !y) || (!x && y)

const booleanPrimitives: [string, Value][] = [
  ['not', mkFn(not, 1)],
  ['boolean?', mkFn(booleanQ, 1)],
  ['nand', mkFn(nand, 1, true)],
  ['nor', mkFn(nor, 1, true)],
  ['implies', mkFn(implies, 2)],
  ['xor', mkFn(xor, 2)]
]

// Pairs and Lists (6.4)

const pairQ = (x: any): boolean => V.isPair(x)
const cons = (x: any, y: any): Value => V.mkPair(x, y)
const pair = cons
const car = (x: Value): Value => (x as any).fst
const cdr = (x: Value): Value => (x as any).snd

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

const nullQ = (x: any): boolean => x === null
const listQ = (x: any): boolean => x === null || (V.isPair(x) && (x as any).isList) 

const pairListPrimitives: [string, Value][] = [
  ['pair?', mkFn(pairQ, 1)],
  ['cons', mkFn(cons, 2)],
  ['pair', mkFn(pair, 2)],
  ['car', mkFn(car, 1)],
  ['cdr', mkFn(cdr, 1)],
  ['null?', mkFn(nullQ, 1)],
  ['list?', mkFn(listQ, 1)]
]

const list = (...xs: Value[]): V.List => {
  let ret: V.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = V.mkPair(xs[i], ret)
  }
  return ret
}

const makeList: (n: number, fill: Value) => V.List = (n, fill) => {
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = V.mkPair(fill, ret)
  }
  return ret
}

const length = (l: V.List): number => {
  let len = 0
  while (l !== null) {
    len += 1
    l = (l.snd as V.List)
  }
  return len
}

function appendOne_ (l1: V.List, l2: V.List): V.List {
  if (l1 === null) {
    return l2
  } else {
    return V.mkPair(l1.fst, appendOne_(l1.snd as V.List, l2))
  }
}

function append (l: V.List, ...ls: V.List[]): V.List {
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret 
}

function reverse (l: V.List): V.List {
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

function listTail (l: V.List, k: number): V.List {
  throw new Error ('Prelude.tail unimplemented')
}

function listTake (l: V.List, k: number): V.List {
  throw new Error ('Prelude.take unimplemented')
}

function listRef (l: V.List, n: number): Value {
  throw new Error ('Prelude.ref unimplemented')
}

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

function assocKey (v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}

function assocRef (v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}

function assocSet (k: Value, v: Value, l: V.List): boolean {
  throw new Error ('Prelude.assocKey unimplemented')
}

const listPrimitives: [string, Value][] = [
  ['list', mkFn(list, 0, true)],
  ['make-list', mkFn(makeList, 2)],
  ['length', mkFn(length, 1)],
  ['append', mkFn(append, 1, true)],
  ['reverse', mkFn(reverse, 1)],
  ['list-tail', mkFn(listTail, 2)],
  ['list-drop', mkFn(listTail, 2)],
  ['list-take', mkFn(listTake, 2)],
  ['list-ref', mkFn(listRef, 2)],
  ['index-of', mkFn(indexOf, 2)],
  ['assoc-key?', mkFn(assocKey, 2)],
  ['assoc-ref', mkFn(assocRef, 2)],
  ['assoc-set', mkFn(assocSet, 3)]
]

// Symbols (6.5)

// TODO: implement:
//   (symbol? obj)
//   (symbol=? sym1 ... symk)
//   (symbol->string sym)
//   (string->symbol str)
//
// ...but we don't implement symbols, will we?

// Characters (6.6)

// TODO: implement:

const charQ = (x: any): boolean => V.isChar(x)

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

function mkCharCompareFn (name: string, f: (a: string, b: string) => boolean): Function {
  return (...args: Value[]) => (pairwiseSatisfies((a, b) => f((a as V.Char).value, (b as V.Char).value), args))
}

const charEqQ: Function = mkCharCompareFn('char=?', (a, b) => a === b)
const charLtQ: Function = mkCharCompareFn('char<?', (a, b) => a.codePointAt(0)! < b.codePointAt(0)!)
const charGtQ: Function = mkCharCompareFn('char>?', (a, b) => a.codePointAt(0)! > b.codePointAt(0)!)
const charLeqQ: Function = mkCharCompareFn('char<=?', (a, b) => a.codePointAt(0)! <= b.codePointAt(0)!)
const charGeqQ: Function = mkCharCompareFn('char>=?', (a, b) => a.codePointAt(0)! >= b.codePointAt(0)!)
const charEqCiQ: Function = mkCharCompareFn('char-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
const charLtCiQ: Function = mkCharCompareFn('char-ci<?', (a, b) => a.toLowerCase().codePointAt(0)! < b.toLowerCase().codePointAt(0)!)
const charGtCiQ: Function = mkCharCompareFn('char-ci>?', (a, b) => a.toLowerCase().codePointAt(0)! > b.toLowerCase().codePointAt(0)!)
const charLeqCiQ: Function = mkCharCompareFn('char-ci<=?', (a, b) => a.toLowerCase().codePointAt(0)! <= b.toLowerCase().codePointAt(0)!)
const charGeqCiQ: Function = mkCharCompareFn('char-ci>=?', (a, b) => a.toLowerCase().codePointAt(0)! >= b.toLowerCase().codePointAt(0)!)

function mkCharPredicatePrim (name: string, f: (a: string) => boolean): Function {
  return (x: V.Char) => f(x.value)
}

const charAlphabeticQ: Function =
  mkCharPredicatePrim('char-alphabetic?', (a) => /\p{L}/gu.test(a))
const charNumericQ: Function =
  mkCharPredicatePrim('char-numeric?', (a) => /\p{N}/gu.test(a))
const charWhitespaceQ: Function =
  mkCharPredicatePrim('char-whitespace?', (a) => /\p{Z}/gu.test(a))
const charUpperCaseQ: Function =
  mkCharPredicatePrim('char-upper-case?', (a) => /\p{Lu}/gu.test(a))
const charLowerCaseQ: Function =
  mkCharPredicatePrim('char-lower-case?', (a) => /\p{Ll}/gu.test(a))

function digitalValue (c: V.Char): number {
  const n = parseInt(c.value, 10)
  if (isNaN(n)) {
    throw new Error('digitalValue: not a decimal digit')
  } else {
    return n
  }
}

const charToInteger = (c: V.Char): number => c.value.codePointAt(0)!
const integerToChar = (n: number): V.Char => V.mkChar(String.fromCodePoint(n))
const charUpcase = (c: V.Char): V.Char => V.mkChar(c.value.toUpperCase())
const charDowncase = (c: V.Char): V.Char => V.mkChar(c.value.toLowerCase())

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
const charFoldcase = (c: V.Char): V.Char => V.mkChar(c.value.toLowerCase())

// Strings (6.7)

const stringQ = (x: any): boolean => typeof x === 'string'

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
const makeString = (k: number, c: V.Char): string => c.value.repeat(k)

const stringPrim = (...args: V.Char[]): string => args.map((e) => e.value).join('')
const stringLength = (s: string): number => s.length
const stringRef = (s: string, i: number): V.Char => V.mkChar(s[i])

// N.B., string-set! is unimplemented since it is effectful.

function mkStringCompareFn (name: string, f: (a: string, b: string) => boolean): Function {
  return (...args: string[]) => pairwiseSatisfies((a, b) => f(a, b), args)
}

const stringEq: Function = mkStringCompareFn('string=?', (a, b) => a === b)
const stringLt: Function = mkStringCompareFn('string<?', (a, b) => a < b)
const stringGt: Function = mkStringCompareFn('string>?', (a, b) => a > b)
const stringLeq: Function = mkStringCompareFn('string<=?', (a, b) => a <= b)
const stringGeq: Function = mkStringCompareFn('string>=?', (a, b) => a >= b)
const stringEqCi: Function = mkStringCompareFn('string-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
const stringLtCi: Function = mkStringCompareFn('string-ci<?', (a, b) => a.toLowerCase() < b.toLowerCase())
const stringGtCi: Function = mkStringCompareFn('string-ci>?', (a, b) => a.toLowerCase() > b.toLowerCase())
const stringLeqCi: Function = mkStringCompareFn('string-ci<=?', (a, b) => a.toLowerCase() <= b.toLowerCase())
const stringGeqCi: Function = mkStringCompareFn('string-ci>=?', (a, b) => a.toLowerCase() >= b.toLowerCase())

const stringUpcase = (s: string): string => s.toUpperCase()
const stringDowncase = (s: string): string => s.toLowerCase()
const stringFoldcase = (s: string): string => s.toLowerCase()
const substring = (s: string, start: number, end: number): string => s.substring(start, end)
const stringAppend = (...args: string[]): string => args.join('')
const stringToList = (s: string): V.List => {
  throw new Error ('string->list unimplemented')
}

/*
const stringListPrim: L.Prim = (_env, args, app) => {
  if (args.length !== 1 && args.length !== 3) {
    return Promise.resolve(runtimeError(msg('error-arity', 'string->list', '1 or 3', args.length), app))
  }
  if (!L.valueIsString(args[0])) {
    return Promise.resolve(runtimeError(msg('error-type-expected-fun', 1, 'string->list', 'string', L.nlevalue(args[0])), app))
  }
  const str = args[0] as string
  let start, end
  if (args.length === 1) {
    start = 0
    end = str.length
  } else {
    if (!L.valueIsInteger(args[1])) {
      return Promise.resolve(runtimeError(msg('error-type-expected-fun', 2, 'string->list', 'integer', L.nlevalue(args[1])), app))
    }
    if (!L.valueIsInteger(args[2])) {
      return Promise.resolve(runtimeError(msg('error-type-expected-fun', 3, 'string->list', 'integer', L.nlevalue(args[2])), app))
    }
    start = args[1] as number
    end = args[2] as number
  }
  return Promise.resolve(ok(L.valueArrayToList(str.substring(start, end).split('').map(L.vchar))))
}
*/

function listToString (l: V.List): string {
  let ret = ''
  while (l !== null) {
    ret += (l.fst as V.Char).value
    l = l.snd as V.List
  } 
  return ret
}

function stringToVector (s: string): V.Char[] {
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(V.mkChar(s[i]))
  }
  return ret
}

function vectorToString (v: V.Char[]): string {
  let ret = ''
  for (let i = 0; i < v.length; i++) {
    ret += v[i].value
  }
  return ret
}

// N.B., the following functions:
//
//   (string-copy string)
//   (string-copy string start)
//   (string-copy string start end)
//
// and string-copy! and string-fill! are unimplemented since they don't make
// sense in an immutable context.

// Additional functions from racket/string.

const stringContains = (s: string, sub: string): boolean => s.includes(sub)
const stringSplit = (s: string, sep: string): V.List => {
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = V.mkPair(splits[i], ret)
  }
  return ret
}
const stringSplitVector = (s: string, sep: string): string[] => s.split(sep)

// const fileStringPrim: L.Prim = (_env, args, app) =>
//   Utils.checkArgsResult('file->string', ['string?'], undefined, args, app).asyncAndThen(async _ =>
//     (await fs.read(args[0] as string)).asyncAndThen(s =>
//       Promise.resolve(ok(s))))

// const fileLinesPrim: L.Prim = (env, args, app) =>
//   Utils.checkArgsResult('file->lines', ['string?'], undefined, args, app).asyncAndThen(_ =>
//     evaluateExp(env, L.nlecall(L.nlevar('string-split'), [
//       L.nlecall(L.nlevar('file->string'), [L.nlevalue(args[0])]),
//       L.nlestr('\n')
//     ])))

// TODO: what should the type of a reactive-file object be? A struct? Or a JS object?

type ReactiveFile = { _scamperTag: 'struct', kind: '_reactive-file', callback: V.Closure | V.JsFunction }
const withFile = (callback: V.Closure | V.JsFunction): ReactiveFile => ({
  _scamperTag: 'struct',
  kind: '_reactive-file',
  callback
})

const stringPrimitives: [string, Value][] = [
  ['char?', mkFn(charQ, 1)],
  ['char=?', mkFn(charEqQ, 1, true)],
  ['char<?', mkFn(charLtQ, 1, true)],
  ['char>?', mkFn(charGtQ, 1, true)],
  ['char<=?', mkFn(charLeqQ, 1, true)],
  ['char>=?', mkFn(charGeqQ, 1, true)],
  ['char-ci=?', mkFn(charEqCiQ, 1, true)],
  ['char-ci<?', mkFn(charLtCiQ, 1, true)],
  ['char-ci>?', mkFn(charGtCiQ, 1, true)],
  ['char-ci<=?', mkFn(charLeqCiQ, 1, true)],
  ['char-ci>=?', mkFn(charGeqCiQ, 1, true)],
  ['char-alphabetic?', mkFn(charAlphabeticQ, 1)],
  ['char-numeric?', mkFn(charNumericQ, 1)],
  ['char-whitespace?', mkFn(charWhitespaceQ, 1)],
  ['char-upper-case?', mkFn(charUpperCaseQ, 1)],
  ['char-lower-case?', mkFn(charLowerCaseQ, 1)],
  ['digit-value', mkFn(digitalValue, 1)],
  ['char->integer', mkFn(charToInteger, 1)],
  ['integer->char', mkFn(integerToChar, 1)],
  ['char-upcase', mkFn(charUpcase, 1)],
  ['char-downcase', mkFn(charDowncase, 1)],
  ['char-foldcase', mkFn(charFoldcase, 1)],
  ['string?', mkFn(stringQ, 1)],
  ['make-string', mkFn(makeString, 2)], 
  ['string', mkFn(stringPrim, 0, true)],
  ['string-length', mkFn(stringLength, 1)],
  ['string-ref', mkFn(stringRef, 2)],
  ['string=?', mkFn(stringEq, 1, true)],
  ['string<?', mkFn(stringLt, 1, true)],
  ['string>?', mkFn(stringGt, 1, true)],
  ['string<=?', mkFn(stringLeq, 1, true)],
  ['string>=?', mkFn(stringGeq, 1, true)],
  ['string-ci=?', mkFn(stringEqCi, 1, true)],
  ['string-ci<?', mkFn(stringLtCi, 1, true)],
  ['string-ci>?', mkFn(stringGtCi, 1, true)],
  ['string-ci<=?', mkFn(stringLeqCi, 1, true)],
  ['string-ci>=?', mkFn(stringGeqCi, 1, true)],
  ['string-upcase', mkFn(stringUpcase, 1)],
  ['string-downcase', mkFn(stringDowncase, 1)],
  ['string-foldcase', mkFn(stringFoldcase, 1)],
  ['substring', mkFn(substring, 3)],
  ['string->list', mkFn(stringToList, 1)],
  ['list->string', mkFn(listToString, 1)],
  ['string->vector', mkFn(stringToVector, 1)],
  ['vector->string', mkFn(vectorToString, 1)],
  ['string-contains', mkFn(stringContains, 2)],
  ['string-split', mkFn(stringSplit, 2)],
  ['string-split-vector', mkFn(stringSplitVector, 2)],
  ['string-append', mkFn(stringAppend, 0, true)],
  ['with-file', mkFn(withFile, 1)]
]

// Vectors (6.8)

const vectorQ = (x: any): boolean => Array.isArray(x)
const vector = (...xs: Value[]): Value[] => xs
const makeVector = (n: number, fill: Value): Value[] => {
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}
const vectorLength = (v: Value[]): number => v.length
const vectorRef = (v: Value[], i: number): Value => v[i]
const vectorSet = (v: Value[], i: number, x: Value): void => { v[i] = x }
const vectorFill = (v: Value[], x: Value): void => {
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}

const vectorToList = (v: Value[]): V.List => {
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = V.mkPair(v[i], ret)
  }
  return ret
}

const listToVector = (l: V.List): Value[] => {
  const ret = []
  while (l !== null) {
    ret.push(l.fst)
    l = l.snd as V.List
  }
  return ret
}

const vectorRange = (m: number, n: number, step: number): number[] => {
  throw new Error ('vector-range unimplemented')
}

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

const vectorAppend = (...vecs: Value[][]): Value[] => {
  const arr = []
  for (let i = 0; i < vecs.length; i++) {
    for (let j = 0; j < vecs[i].length; j++) {
      arr.push(vecs[i][j])
    }
  }
  return arr
}

const vectorPrimitives: [string, Value][] = [
  ['vector?', mkFn(vectorQ, 1)],
  ['vector', mkFn(vector, 0, true)],
  ['make-vector', mkFn(makeVector, 2)],
  ['vector-length', mkFn(vectorLength, 1)],
  ['vector-ref', mkFn(vectorRef, 2)],
  ['vector-set!', mkFn(vectorSet, 3)],
  ['vector-fill!', mkFn(vectorFill, 2)],
  ['vector->list', mkFn(vectorToList, 1)],
  ['list->vector', mkFn(vectorToList, 1)],
  ['vector-range', mkFn(vectorRange, 1, true)]
]

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

const procedureQ = (x: any): boolean => V.isClosure(x) || V.isJsFunction(x)
const apply = (f: V.Closure | V.JsFunction, args: Value[]): Value => {
  throw new Error ('apply unimplemented')
}
const stringMap = (f: V.Closure | V.JsFunction, s: string): string => {
  throw new Error ('string-map unimplemented')
}

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

const map = (f: V.Closure | V.JsFunction, ...lsts: V.List[]): V.List => {
  throw new Error ('map unimplemented')
}

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

const filter = (f: V.Closure | V.JsFunction, lst: V.List): V.List => {
  throw new Error ('filter unimplemented')
}

const fold = (f: V.Closure | V.JsFunction, init: Value, lst: V.List): Value => {
  throw new Error ('fold unimplemented')
}

const reduce = (f: V.Closure | V.JsFunction, lst: V.List): Value => {
  throw new Error ('reduce unimplemented')
}

const foldLeft = fold

const foldRight = (f: V.Closure | V.JsFunction, init: Value, lst: V.List): Value => {
  throw new Error ('fold-right unimplemented')
}

const reduceRight = (f: V.Closure | V.JsFunction, lst: V.List): Value => {
  throw new Error ('reduce-right unimplemented')
}

const vectorMap = (f: V.Closure | V.JsFunction, ...vecs: Value[][]): Value[] => {
  throw new Error ('vector-map unimplemented')
}

const vectorMapBang = (f: V.Closure | V.JsFunction, vec: Value[]): void => {
  throw new Error ('vector-map! unimplemented')
}

const vectorForEach = (f: V.Closure | V.JsFunction, vec: Value[]): void => {
  throw new Error ('vector-for-each unimplemented')
}

const forRange = (start: number, end: number, f: V.Closure | V.JsFunction): void => {
  throw new Error ('for-range unimplemented')
}

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

const vectorFilter = (f: V.Closure | V.JsFunction, lst: Value[]): Value[][] => {
  throw new Error ('vector-filter unimplemented')
}

// TODO: implement fold/reduce variants for vectors

const voidQ = (x: any): boolean => x === undefined
const error = (msg: string): never => { throw new Error(msg) }
const qq = (): never => { throw new Error('??') }
const compose = (...fs: (V.Closure | V.JsFunction)[]): V.Closure | V.JsFunction => {
  throw new Error ('compose unimplemented')
}
const pipe = (init: Value, ...fs: (V.Closure | V.JsFunction)[]): V.Closure | V.JsFunction => {
  throw new Error ('|> unimplemented')
}
const range = (m: number, n: number, step: number): number[] => {
  throw new Error ('range unimplemented')
}

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

const random = (n: number): number => Math.floor(Math.random() * n)
const withHandler = (handler: V.Closure | V.JsFunction, fn: V.Closure | V.JsFunction, ...args: Value[]): Value => {
  throw new Error ('with-handler unimplemented')
}

const controlPrimitives: [string, Value][] = [
  ['procedure?', mkFn(procedureQ, 1)],
  ['apply', mkFn(apply, 2, true)],
  ['string-map', mkFn(stringMap, 2)],
  ['map', mkFn(map, 2, true)],
  ['filter', mkFn(filter, 2, true)],
  ['fold', mkFn(fold, 3)],
  ['reduce', mkFn(reduce, 2)],
  ['fold-left', mkFn(foldLeft, 3)],
  ['fold-right', mkFn(foldRight, 3)],
  ['reduce-right', mkFn(reduceRight, 2)],
  ['vector-map', mkFn(vectorMap, 2, true)],
  ['vector-map!', mkFn(vectorMapBang, 2)],
  ['vector-for-each', mkFn(vectorForEach, 2)],
  ['for-range', mkFn(forRange, 3)],
  ['vector-filter', mkFn(vectorFilter, 2)],
  ['vector-append', mkFn(vectorAppend, 0, true)],
  ['voidQ', mkFn(voidQ, 1)],
  ['error', mkFn(error, 1)],
  ['??', mkFn(qq, 0)],
  ['compose', mkFn(compose, 1, true)],
  ['o', mkFn(compose, 1, true)],
  ['|>', mkFn(pipe, 2, true)],
  ['range', mkFn(range, 1, true)],
  ['random', mkFn(random, 1)],
  ['with-handler', mkFn(withHandler, 2, true)]
]

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

const Prelude: [string, Value][] = [
  ...equivalencePrimitives,
  ...numericPrimitives,
  ...booleanPrimitives,
  ...pairListPrimitives,
  ...listPrimitives,
  ...stringPrimitives,
  ...vectorPrimitives,
  ...controlPrimitives,
  ['else', elseConst],
  ['pi', piConst],
  ['π', piConst],
  ['void', voidConst]
]

export default Prelude
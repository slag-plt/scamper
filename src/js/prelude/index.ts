import * as L from "../../lpm"
import { checkContract, contract } from "../contract.js"
import * as C from "../contract.js"
import { SubthreadErrors } from "../../lpm"

export * from "./files.js"

const query1C = (name: string) => contract(name, [C.any])
const query2C = (name: string) => contract(name, [C.any, C.any])

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

export function equalQ(x: any, y: any): boolean {
  checkContract(arguments, query2C("equal?"))
  return L.equals(x, y)
}

// Numbers (6.2)

export function numberQ(x: any): boolean {
  checkContract(arguments, query1C("number?"))
  return typeof x === "number"
}

export function realQ(x: any): boolean {
  checkContract(arguments, query1C("real?"))
  return typeof x === "number" && !Number.isInteger(x)
}

export function integerQ(x: any): boolean {
  checkContract(arguments, query1C("integer?"))
  return typeof x === "number" && Number.isInteger(x)
}

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

export function nanQ(x: any): boolean {
  checkContract(arguments, query1C("nan?"))
  return Number.isNaN(x)
}

export function lt(x: number, y: number): boolean {
  checkContract(arguments, contract("<", [C.number, C.number]))
  return x < y
}

export function leq(x: number, y: number): boolean {
  checkContract(arguments, contract("<=", [C.number, C.number]))
  return x <= y
}

export function gt(x: number, y: number): boolean {
  checkContract(arguments, contract(">", [C.number, C.number]))
  return x > y
}

export function geq(x: number, y: number): boolean {
  checkContract(arguments, contract(">=", [C.number, C.number]))
  return x >= y
}

export function eq(x: number, y: number): boolean {
  checkContract(arguments, contract("=", [C.number, C.number]))
  return x === y
}

export function eqEps(eps: number): L.ScamperFn {
  checkContract(arguments, contract("=-eps", [C.number]))
  const eq = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-eps`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  } as L.JsFunction
  L.nameFn(`(=-eps ${eps.toString()})`, eq)
  return eq
}

export function zeroQ(x: number): boolean {
  checkContract(arguments, contract("zero?", [C.number]))
  return x === 0
}

export function positiveQ(x: number): boolean {
  checkContract(arguments, contract("positive?", [C.number]))
  return x > 0
}

export function negativeQ(x: number): boolean {
  checkContract(arguments, contract("negative?", [C.number]))
  return x < 0
}

export function oddQ(x: number): boolean {
  checkContract(arguments, contract("odd?", [C.integer]))
  return (x & 1) === 1
}

export function evenQ(x: number): boolean {
  checkContract(arguments, contract("even?", [C.integer]))
  return (x & 1) !== 1
}

export function max(...xs: number[]): number {
  checkContract(arguments, contract("max", [], C.number))
  return Math.max(...xs)
}

export function min(...xs: number[]): number {
  checkContract(arguments, contract("min", [], C.number))
  return Math.min(...xs)
}

export function plus(...xs: number[]): number {
  checkContract(arguments, contract("+", [], C.number))
  return xs.reduce((a, b) => a + b, 0)
}

export function minus(...xs: number[]): number {
  checkContract(arguments, contract("-", [C.number], C.number))
  return xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
}

export function times(...xs: number[]): number {
  checkContract(arguments, contract("*", [], C.number))
  return xs.reduce((a, b) => a * b, 1)
}

export function div(...xs: number[]): number {
  checkContract(arguments, contract("/", [C.number], C.number))
  return xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
}

export function abs(x: number): number {
  checkContract(arguments, contract("abs", [C.number]))
  return Math.abs(x)
}

// N.B., not implementing the composite division functions:
//   (floor / n1 n2)
//   (floor-quotient n1 n2)
//   (floor-remainder n1 n2)
//   (truncate/ n1 n2)
//   (truncate-quotient n1 n2)
//   (truncate-remainder n1 n2)
// To avoid clutter in the documentation.

export function quotient(x: number, y: number): number {
  checkContract(arguments, contract("quotient", [C.integer, C.integer]))
  return Math.trunc(x / y)
}

export function remainder(x: number, y: number): number {
  checkContract(arguments, contract("remainder", [C.integer, C.integer]))
  return x % y
}

export function modulo(x: number, y: number): number {
  checkContract(arguments, contract("modulo", [C.integer, C.integer]))
  return ((x % y) + y) % y
}

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)

// N.B., we don't implement:
//   (numerator q)
//   (denominator q)
// Since we don't implement rationals.

export function floor(x: number): number {
  checkContract(arguments, contract("floor", [C.number]))
  return Math.floor(x)
}

export function ceiling(x: number): number {
  checkContract(arguments, contract("ceiling", [C.number]))
  return Math.ceil(x)
}

export function truncate(x: number): number {
  checkContract(arguments, contract("truncate", [C.number]))
  return Math.trunc(x)
}

export function round(x: number): number {
  checkContract(arguments, contract("round", [C.number]))
  return Math.round(x)
}

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

export function square(x: number): number {
  checkContract(arguments, contract("square", [C.number]))
  return x * x
}

export function sqrt(x: number): number {
  checkContract(arguments, contract("sqrt", [C.number]))
  return Math.sqrt(x)
}

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

export function expt(x: number, y: number): number {
  checkContract(arguments, contract("expt", [C.number, C.number]))
  return Math.pow(x, y)
}

// N.B., we don't implement:
//   (make-rectangular x1 x2)   ...probably not!
//   (make-polar x3 x4)         ...probably not!
//   (real-part z)              ...probably not!
//   (imag-part z)              ...probably not!
//   (magnitude z)              ...probably not!
//   (angle z)                  ...probably not!
// Because we don't implement complex numbers.

export function numberToString(x: number): string {
  checkContract(arguments, contract("number->string", [C.number]))
  return x.toString()
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

export function stringToNumber(s: string): number {
  checkContract(arguments, contract("string->number", [C.string]))
  if (/^[+-]?\d+$/.test(s)) {
    return parseInt(s)
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return parseFloat(s)
  } else {
    throw new Error(`Runtime error: string->number: invalid string: ${s}`)
  }
}

// Additional functions from racket/base

export function exp(x: number): number {
  checkContract(arguments, contract("exp", [C.number]))
  return Math.exp(x)
}

export function log(x: number): number {
  checkContract(arguments, contract("log", [C.number]))
  return Math.log(x)
}

export function sin(x: number): number {
  checkContract(arguments, contract("sin", [C.number]))
  return Math.sin(x)
}

export function cos(x: number): number {
  checkContract(arguments, contract("cos", [C.number]))
  return Math.cos(x)
}

export function tan(x: number): number {
  checkContract(arguments, contract("tan", [C.number]))
  return Math.tan(x)
}

export function asin(x: number): number {
  checkContract(arguments, contract("asin", [C.number]))
  return Math.asin(x)
}

export function acos(x: number): number {
  checkContract(arguments, contract("acos", [C.number]))
  return Math.acos(x)
}

export function atan(x: number): number {
  checkContract(arguments, contract("atan", [C.number]))
  return Math.atan(x)
}

export function equalsEps(eps: number): L.Value {
  checkContract(arguments, contract("=-eps", [C.number]))
  const name = `=-(${eps})`
  const ret = function (x: number, y: number): boolean {
    checkContract(arguments, contract(`=-(${eps})`, [C.number, C.number]))
    return Math.abs(x - y) <= eps
  }
  L.nameFn(name, ret)
  return ret
}

// Booleans (6.3)

export function not(x: boolean): boolean {
  checkContract(arguments, contract("not", [C.boolean]))
  return !x
}

export function booleanQ(x: any): boolean {
  checkContract(arguments, contract("boolean?", [C.any]))
  return typeof x === "boolean"
}

// From racket/base

export function nand(...xs: boolean[]): boolean {
  checkContract(arguments, contract("nand", [], C.boolean))
  return !xs.reduce((a, b) => a && b, true)
}

export function nor(...xs: boolean[]): boolean {
  checkContract(arguments, contract("nor", [], C.boolean))
  return !xs.reduce((a, b) => a || b, false)
}

export function implies(x: boolean, y: boolean): boolean {
  checkContract(arguments, contract("implies", [C.boolean, C.boolean]))
  return !x || y
}

export function xor(x: boolean, y: boolean): boolean {
  checkContract(arguments, contract("xor", [C.boolean, C.boolean]))
  return (x && !y) || (!x && y)
}

// Additional functions

export function anyOf(...fns: L.ScamperFn[]): L.ScamperFn {
  checkContract(arguments, contract("any-of", [], C.func))
  return function (v: any): boolean {
    checkContract(arguments, contract(`any-of`, [C.any]))
    for (let i = 0; i < fns.length; i++) {
      if (L.callScamperFn(fns[i], v)) {
        return true
      }
    }
    return false
  }
}

export function allOf(...fns: L.ScamperFn[]): L.ScamperFn {
  checkContract(arguments, contract("any-of", [], C.func))
  return function (v: any): boolean {
    checkContract(arguments, contract(`any-of`, [C.any]))
    for (let i = 0; i < fns.length; i++) {
      if (!L.callScamperFn(fns[i], v)) {
        return false
      }
    }
    return true
  }
}

// Pairs and Lists (6.4)

// NOTE: like Clojure, we distinguish between pairs and lists (cons).

export function pairQ(x: any): boolean {
  checkContract(arguments, contract("pair?", [C.any]))
  return L.isPair(x)
}

export function listOf(pred: L.ScamperFn): L.ScamperFn {
  checkContract(arguments, contract("list-of", [C.func]))
  return function (l: L.List): boolean {
    // N.B., list-of returns false if the input is _not_ a list
    if (!listQ(l)) {
      return false
    }
    while (l !== null) {
      if (!L.callScamperFn(pred, l.head)) {
        return false
      }
      l = l.tail
    }
    return true
  } as L.JsFunction
}

export function cons(x: any, y: any): L.Value {
  checkContract(arguments, contract("cons", [C.any, C.any]))
  return L.mkCons(x, y)
}

export function pair(x: any, y: any): L.Value {
  checkContract(arguments, contract("pair", [C.any, C.any]))
  return L.mkPair(x, y)
}

export function car(x: L.Value): L.Value {
  checkContract(arguments, contract("car", [C.or(C.pair, C.list)]))
  if (L.isPair(x)) {
    return (x as any).fst
  } else {
    return (x as any).head
  }
}

export function cdr(x: L.Value): L.Value {
  checkContract(arguments, contract("cdr", [C.or(C.pair, C.list)]))
  if (L.isPair(x)) {
    return (x as any).snd
  } else {
    return (x as any).tail
  }
}

const listAccessors = [
  // 4-character accessors
  "caar",
  "cadr",
  "cdar",
  "cddr",
  // 5-character accessors
  "caaar",
  "cadar",
  "cdaar",
  "cddar",
  "caadr",
  "caddr",
  "cdadr",
  "cdddr",
  // 6-character accessors
  "caaaar",
  "cadaar",
  "cdaaar",
  "cddaar",
  "caadar",
  "caddar",
  "cdadar",
  "cdddar",
  "caaadr",
  "cadadr",
  "cdaadr",
  "cddadr",
  "caaddr",
  "cadddr",
  "cdaddr",
  "cddddr",
]
export const listAccessorFns: Record<string, (x: L.Value) => L.Value> = {}
listAccessors.forEach((name) => {
  const path = name.slice(1, name.length - 1)
  const fn = function (x: L.Value): L.Value {
    checkContract(arguments, contract(name, [C.or(C.pair, C.list)]))
    let ret = path.endsWith("a") ? car(x) : cdr(x)
    for (let i = path.length - 2; i >= 0; i--) {
      ret = path[i] === "a" ? car(ret) : cdr(ret)
    }
    return ret
  }
  listAccessorFns[name] = fn
})

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

export function nullQ(x: any): boolean {
  checkContract(arguments, contract("null?", [C.any]))
  return x === null
}

export function listQ(x: any): boolean {
  checkContract(arguments, contract("list?", [C.any]))
  return L.isList(x)
}

export function list(...xs: L.Value[]): L.List {
  checkContract(arguments, contract("list", [], C.any))
  let ret: L.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = L.mkCons(xs[i], ret)
  }
  return ret
}

export function makeList(n: number, fill: L.Value): L.List {
  checkContract(arguments, contract("make-list", [C.integer, C.any]))
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = L.mkCons(fill, ret)
  }
  return ret
}

export function length(l: L.List): number {
  checkContract(arguments, contract("length", [C.list]))
  let len = 0
  while (l !== null) {
    len += 1
    l = l.tail
  }
  return len
}

function appendOne_(l1: L.List, l2: L.List): L.List {
  if (l1 === null) {
    return l2
  } else {
    const head = L.mkCons(l1.head, null)
    let last: L.List = head
    let cur = l1.tail
    while (cur !== null) {
      last.tail = L.mkCons(cur.head, null)
      last = last.tail
      cur = cur.tail
    }
    last.tail = l2
    return head
  }
}

export function append(l: L.List, ...ls: L.List[]): L.List {
  checkContract(arguments, contract("append", [C.list], C.list))
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret
}

export function reverse(l: L.List): L.List {
  checkContract(arguments, contract("reverse", [C.list]))
  const queue = []
  while (l !== null) {
    queue.push(l)
    l = l.tail
  }
  queue.reverse()
  let ret = null
  while (queue.length > 0) {
    const next = queue.pop()!
    ret = L.mkCons(next.head, ret)
  }
  return ret
}

export function listTail(l: L.List, k: number): L.List {
  checkContract(arguments, contract("list-tail", [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.tail
    k -= 1
  }
  return l
}

export function listTake(l: L.List, k: number): L.List {
  checkContract(arguments, contract("list-take", [C.list, C.nonneg]))
  const elts = []
  // N.B., push in reverse order so we built the list right-to-left
  while (l !== null && k > 0) {
    elts.push(l.head)
    l = l.tail
    k -= 1
  }
  let ret: L.List = null
  for (let i = elts.length - 1; i >= 0; i--) {
    ret = L.mkCons(elts[i], ret)
  }
  return ret
}

export function listDrop(l: L.List, k: number): L.List {
  checkContract(arguments, contract("list-drop", [C.list, C.nonneg]))
  while (l !== null && k > 0) {
    l = l.tail
    k -= 1
  }
  return l
}

export function listRef(l: L.List, n: number): L.Value {
  checkContract(arguments, contract("list-ref", [C.list, C.nonneg]))
  let i = n
  while (l !== null && i > 0) {
    l = l.tail
    i -= 1
  }
  if (l === null) {
    throw new L.ScamperError(
      "Runtime",
      `list-ref: index ${n} out of bounds of list`,
    )
  } else {
    return l.head
  }
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

export function indexOf(l: L.List, v: L.Value): number {
  checkContract(arguments, contract("index-of", [C.list, C.any]))
  let i = 0
  while (l !== null) {
    if (L.equals(l.head, v)) {
      return i
    }
    l = l.tail
    i += 1
  }
  return -1
}

export function assocKey(v: L.Value, l: L.List): boolean {
  checkContract(arguments, contract("assoc-key?", [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (L.equals((l.head as L.Pair).fst, v)) {
      return true
    }
    l = l.tail
  }
  return false
}

export function assocRef(v: L.Value, l: L.List): L.Value {
  checkContract(arguments, contract("assoc-ref", [C.any, C.listof(C.pair)]))
  while (l !== null) {
    if (L.equals((l.head as L.Pair).fst, v)) {
      return (l.head as L.Pair).snd
    }
    l = l.tail
  }
  throw new L.ScamperError(
    "Runtime",
    `assoc-ref: key ${v} not found in association list`,
  )
}

export function assocSet(k: L.Value, v: L.Value, l: L.List): L.List {
  checkContract(
    arguments,
    contract("assoc-set", [C.any, C.any, C.listof(C.pair)]),
  )
  const front = []
  // TODO: implement me—this isn't the right implementation!
  while (l !== null) {
    const entry = l.head as L.Pair
    if (L.equals(entry.fst, k)) {
      front.push(L.mkPair(k, v))
      let ret = l.tail
      for (let i = front.length - 1; i >= 0; i--) {
        ret = L.mkCons(front[i], ret)
      }
      return ret
    } else {
      front.push(l.head)
      l = l.tail
    }
  }
  return L.vectorToList(front.concat([L.mkPair(k, v)]))
}

// Miscellaneous list functions

export function sort(l: L.List, lt: L.ScamperFn): L.List {
  checkContract(arguments, contract("sort", [C.list, C.func]))
  const arr = L.listToVector(l)
  arr.sort((a, b) => {
    const result = L.callScamperFn(lt, a, b)
    if (typeof result !== "boolean") {
      throw new L.ScamperError(
        "Runtime",
        `sort: comparator function must return a number`,
      )
    } else {
      return result ? -1 : 1
    }
  })
  return L.vectorToList(arr)
}

// Symbols (6.5)

// TODO: implement:
//   (symbol? obj)
//   (symbol=? sym1 ... symk)
//   (symbol->string sym)
//   (string->symbol str)
//
// ...but we don't implement symbols, will we?

// Characters (6.6)

export function charQ(x: any): boolean {
  checkContract(arguments, contract("char?", [C.any]))
  return L.isChar(x)
}

function pairwiseSatisfies<T>(f: (a: T, b: T) => boolean, xs: T[]): boolean {
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

export const charCompareFns: Record<string, (...args: L.Value[]) => boolean> = {}
function mkCharCompareFn(
  name: string,
  f: (a: string, b: string) => boolean,
): void {
  const fn = function (...args: L.Value[]) {
    checkContract(arguments, contract(name, [], C.char))
    return pairwiseSatisfies(
      (a, b) => f((a as L.Char).value, (b as L.Char).value),
      args,
    )
  }
  L.nameFn(name, fn)
  charCompareFns[name] = fn
}

mkCharCompareFn("char=?", (a, b) => a === b)
mkCharCompareFn("char<?", (a, b) => a < b)
mkCharCompareFn("char>?", (a, b) => a > b)
mkCharCompareFn("char<=?", (a, b) => a <= b)
mkCharCompareFn("char>=?", (a, b) => a >= b)
mkCharCompareFn("char-ci=?", (a, b) => a.toLowerCase() === b.toLowerCase())
mkCharCompareFn("char-ci<?", (a, b) => a.toLowerCase() < b.toLowerCase())
mkCharCompareFn("char-ci>?", (a, b) => a.toLowerCase() > b.toLowerCase())
mkCharCompareFn("char-ci<=?", (a, b) => a.toLowerCase() <= b.toLowerCase())
mkCharCompareFn("char-ci>=?", (a, b) => a.toLowerCase() >= b.toLowerCase())

export const charPredicateFns: Record<string, (x: L.Char) => boolean> = {}
function mkCharPredicatePrim(name: string, f: (a: string) => boolean): void {
  const fn = function (x: L.Char) {
    checkContract(arguments, contract(name, [], C.char))
    return f(x.value)
  }
  L.nameFn(name, fn)
  charPredicateFns[name] = fn
}

mkCharPredicatePrim("char-alphabetic?", (a) => /\p{L}/gu.test(a))
mkCharPredicatePrim("char-numeric?", (a) => /\p{N}/gu.test(a))
mkCharPredicatePrim("char-whitespace?", (a) => /\p{Z}/gu.test(a))
mkCharPredicatePrim("char-upper-case?", (a) => /\p{Lu}/gu.test(a))
mkCharPredicatePrim("char-lower-case?", (a) => /\p{Ll}/gu.test(a))

export function digitalValue(c: L.Char): number {
  checkContract(arguments, contract("digit-value", [], C.char))
  const n = parseInt(c.value, 10)
  if (isNaN(n)) {
    throw new L.ScamperError(
      "Runtime",
      `digit-value: ${c.value} is not a decimal digit`,
    )
  } else {
    return n
  }
}

export function charToInteger(c: L.Char): number {
  checkContract(arguments, contract("char->integer", [], C.char))
  return c.value.codePointAt(0)!
}

export function integerToChar(n: number): L.Char {
  checkContract(arguments, contract("integer->char", [C.integer]))
  return L.mkChar(String.fromCodePoint(n))
}

export function charUpcase(c: L.Char): L.Char {
  checkContract(arguments, contract("char-upcase?", [], C.char))
  return L.mkChar(c.value.toUpperCase())
}

export function charDowncase(c: L.Char): L.Char {
  checkContract(arguments, contract("char-downcase?", [], C.char))
  return L.mkChar(c.value.toLowerCase())
}

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
export function charFoldcase(c: L.Char): L.Char {
  checkContract(arguments, contract("char-foldcase?", [], C.char))
  return L.mkChar(c.value.toLowerCase())
}

// Strings (6.7)

export function stringQ(x: any): boolean {
  checkContract(arguments, contract("string?", [C.any]))
  return typeof x === "string"
}

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
export function makeString(k: number, c: L.Char): string {
  checkContract(arguments, contract("make-string", [C.integer, C.char]))
  return c.value.repeat(k)
}

export function string(c: L.Char, ...cs: L.Char[]): string {
  checkContract(arguments, contract("string", [C.char], C.char))
  return [c, ...cs].map((e) => e.value).join("")
}

export function stringLength(s: string): number {
  checkContract(arguments, contract("string-length", [C.string]))
  return s.length
}

export function stringRef(s: string, i: number): L.Char {
  checkContract(arguments, contract("string-ref", [C.string, C.integer]))
  return L.mkChar(s[i])
}

// N.B., string-set! is unimplemented since it is effectful.

export const stringCompareFns: Record<string, (...args: string[]) => boolean> = {}
function mkStringCompareFn(
  name: string,
  f: (a: string, b: string) => boolean,
): void {
  const fn = function (...args: string[]) {
    checkContract(arguments, contract(name, [], C.string))
    return pairwiseSatisfies((a, b) => f(a, b), args)
  }
  L.nameFn(name, fn)
  stringCompareFns[name] = fn
}

mkStringCompareFn("string=?", (a, b) => a === b)
mkStringCompareFn("string<?", (a, b) => a < b)
mkStringCompareFn("string>?", (a, b) => a > b)
mkStringCompareFn("string<=?", (a, b) => a <= b)
mkStringCompareFn("string>=?", (a, b) => a >= b)
mkStringCompareFn("string-ci=?", (a, b) => a.toLowerCase() === b.toLowerCase())
mkStringCompareFn("string-ci<?", (a, b) => a.toLowerCase() < b.toLowerCase())
mkStringCompareFn("string-ci>?", (a, b) => a.toLowerCase() > b.toLowerCase())
mkStringCompareFn("string-ci<=?", (a, b) => a.toLowerCase() <= b.toLowerCase())
mkStringCompareFn("string-ci>=?", (a, b) => a.toLowerCase() >= b.toLowerCase())

export function stringUpcase(s: string): string {
  checkContract(arguments, contract("string-upcase", [C.string]))
  return s.toUpperCase()
}

export function stringDowncase(s: string): string {
  checkContract(arguments, contract("string-downcase", [C.string]))
  return s.toLowerCase()
}

export function stringFoldcase(s: string): string {
  checkContract(arguments, contract("string-foldcase", [C.string]))
  return s.toLowerCase()
}

export function substring(s: string, start: number, end: number): string {
  checkContract(
    arguments,
    contract("substring", [C.string, C.integer, C.integer]),
  )
  return s.substring(start, end)
}

export function stringAppend(...args: string[]): string {
  checkContract(arguments, contract("string-append", [], C.string))
  return args.join("")
}

// TODO: stringToList has a 3-argument version, too, that specifies
// a substring of s to turn into a list.
export function stringToList(s: string): L.List {
  checkContract(arguments, contract("string->list", [C.string]))
  let ret = null
  for (let i = s.length - 1; i >= 0; i--) {
    ret = L.mkCons(L.mkChar(s[i]), ret)
  }
  return ret
}

export function listToString(l: L.List): string {
  checkContract(arguments, contract("list->string", [C.list]))
  let ret = ""
  while (l !== null) {
    if (!L.isChar(l.head)) {
      throw new L.ScamperError(
        "Runtime",
        `list->string: list contains non-character element: ${L.typeOf(l.head)}`,
      )
    }
    ret += l.head.value
    l = l.tail
  }
  return ret
}

export function stringToVector(s: string): L.Char[] {
  checkContract(arguments, contract("string->vector", [C.string]))
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(L.mkChar(s[i]))
  }
  return ret
}

export function vectorToString(v: L.Char[]): string {
  checkContract(arguments, contract("vector->string", [C.vector]))
  let ret = ""
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

export function stringContains(s: string, sub: string): boolean {
  checkContract(arguments, contract("string-contains", [C.string, C.string]))
  return s.includes(sub)
}

export function stringSplit(s: string, sep: string): L.List {
  checkContract(arguments, contract("string-split", [C.string, C.string]))
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = L.mkCons(splits[i], ret)
  }
  return ret
}

export function stringSplitVector(s: string, sep: string): string[] {
  checkContract(
    arguments,
    contract("string-split-vector", [C.string, C.string]),
  )
  return s.split(sep)
}

// Vectors (6.8)

export function vectorQ(x: any): boolean {
  checkContract(arguments, contract("vector?", [C.any]))
  return L.isArray(x)
}

export function vector(...xs: L.Value[]): L.Value[] {
  checkContract(arguments, contract("vector", [], C.any))
  return xs
}

export function makeVector(n: number, fill: L.Value): L.Value[] {
  checkContract(arguments, contract("make-vector", [C.integer, C.any]))
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}

export function vectorLength(v: L.Value[]): number {
  checkContract(arguments, contract("vector-length", [C.vector]))
  return v.length
}

export function vectorRef(v: L.Value[], i: number): L.Value {
  checkContract(arguments, contract("vector-ref", [C.vector, C.integer]))
  return v[i]
}

export function vectorSet(v: L.Value[], i: number, x: L.Value): void {
  checkContract(
    arguments,
    contract("vector-set!", [C.vector, C.integer, C.any]),
  )
  v[i] = x
}

export function vectorFill(v: L.Value[], x: L.Value): void {
  checkContract(arguments, contract("vector-fill!", [C.vector, C.any]))
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}

export function vectorToList(v: L.Value[]): L.List {
  checkContract(arguments, contract("vector->list", [C.vector]))
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = L.mkCons(v[i], ret)
  }
  return ret
}

export function listToVector(l: L.List): L.Value[] {
  checkContract(arguments, contract("list->vector", [C.list]))
  const ret = []
  while (l !== null) {
    ret.push(l.head)
    l = l.tail
  }
  return ret
}

export function vectorRange(...args: number[]): number[] {
  checkContract(arguments, contract("vector-range", [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new L.ScamperError(
      "Runtime",
      "1, 2, or 3 numbers must be passed to function",
    )
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new L.ScamperError("Runtime", '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return arr
  }
}

export function vectorAppend(...vecs: L.Value[][]): L.Value[] {
  checkContract(arguments, contract("vector-append", [], C.vector))
  const arr = []
  for (let i = 0; i < vecs.length; i++) {
    for (let j = 0; j < vecs[i].length; j++) {
      arr.push(vecs[i][j])
    }
  }
  return arr
}

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

export function procedureQ(x: any): boolean {
  checkContract(arguments, contract("procedure?", [C.any]))
  return L.isClosure(x) || L.isJsFunction(x)
}

export function apply(f: L.ScamperFn, args: L.List): L.Value {
  checkContract(arguments, contract("apply", [C.func, C.list]))

  return L.callScamperFn(f, ...L.listToVector(args))
}

export function stringMap(f: L.ScamperFn, s: string): string {
  checkContract(arguments, contract("string-map", [C.func, C.string]))
  const chs = []
  for (let i = 0; i < s.length; i++) {
    chs.push(L.mkChar(s[i]))
  }
  return chs.map((c) => (L.callScamperFn(f, c) as L.Char).value).join("")
}

/**
 * @param arr - a rectangular array of arrays, i.e., each array has the same
 * length
 * @returns the transposition of this array of arrays where rows become columns
 * and columns become rows.
 */
function transpose<T>(arr: T[][]): T[][] {
  if (arr.length === 0) {
    return []
  }
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

function mapOne(f: L.ScamperFn, l: L.List): L.List {
  const values = []
  while (l !== null) {
    values.push(L.callScamperFn(f, l.head))
    l = l.tail
  }
  return L.vectorToList(values)
}

export function map(f: L.ScamperFn, ...lsts: L.List[]): L.List {
  checkContract(arguments, contract("map", [C.func], C.list))
  if (lsts.length === 0) {
    return null
  } else if (lsts.length === 1) {
    return mapOne(f, lsts[0])
  } else {
    const lists = lsts.map(L.listToVector)
    if (!lists.map((l) => l.length).every((n) => n === lists[0].length)) {
      throw new L.ScamperError(
        "Runtime",
        "the lists passed to the function call do not have the same length",
      )
    }
    const xs = transpose(lists)
    return L.vectorToList(xs.map((vs) => L.callScamperFn(f, ...vs)))
  }
}

// Additional list pipeline functions from racket/base

export function filter(f: L.ScamperFn, lst: L.List): L.List {
  checkContract(arguments, contract("filter", [C.func, C.list]))
  const values = []
  while (lst !== null) {
    if (L.callScamperFn(f, lst.head)) {
      values.push(lst.head)
    }
    lst = lst.tail
  }
  return L.vectorToList(values)
}

export function fold(f: L.ScamperFn, init: L.Value, lst: L.List): L.Value {
  checkContract(arguments, contract("fold", [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = L.callScamperFn(f, acc, lst.head)
    lst = lst.tail
  }
  return acc
}

export function reduce(f: L.ScamperFn, lst: L.List): L.Value {
  checkContract(arguments, contract("reduce", [C.func, C.nonemptyList]))
  let acc = lst!.head
  lst = lst!.tail
  while (lst !== null) {
    acc = L.callScamperFn(f, acc, lst.head)
    lst = lst.tail
  }
  return acc
}

export function foldLeft(
  f: L.ScamperFn,
  init: L.Value,
  lst: L.List,
): L.Value {
  checkContract(arguments, contract("fold-left", [C.func, C.any, C.list]))
  let acc = init
  while (lst !== null) {
    acc = L.callScamperFn(f, lst.head, acc)
    lst = lst.tail
  }
  return acc
}

export function foldRight(
  f: L.ScamperFn,
  init: L.Value,
  lst: L.List,
): L.Value {
  checkContract(arguments, contract("fold-right", [C.func, C.any, C.list]))
  const values = L.listToVector(lst)
  let acc = init
  for (let i = values.length - 1; i >= 0; i--) {
    // N.B., the type of the higher-order function is a -> b -> b!
    acc = L.callScamperFn(f, values[i], acc)
  }
  return acc
}

export function reduceRight(f: L.ScamperFn, lst: L.List): L.Value {
  checkContract(arguments, contract("reduce-right", [C.func, C.nonemptyList]))
  const values = L.listToVector(lst)
  let acc = values.pop()
  for (let i = values.length - 1; i >= 0; i--) {
    // N.B., the type of the higher-order function is a -> b -> b!
    acc = L.callScamperFn(f, values[i], acc)
  }
  return acc
}

export function vectorMap(f: L.ScamperFn, ...vecs: L.Value[][]): L.Value[] {
  checkContract(arguments, contract("vector-map", [C.func], C.vector))
  if (vecs.length === 0) {
    return []
  } else if (vecs.length === 1) {
    return vecs[0].map((v) => L.callScamperFn(f, v))
  } else {
    if (!vecs.map((l) => l.length).every((n) => n === vecs[0].length)) {
      throw new L.ScamperError(
        "Runtime",
        "the vectors passed to the function call do not have the same length",
      )
    }
    const xs = transpose(vecs)
    return xs.map((vs) => L.callScamperFn(f, ...vs))
  }
}

export function vectorMapBang(f: L.ScamperFn, vec: L.Value[]): void {
  checkContract(arguments, contract("vector-map!", [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    vec[i] = L.callScamperFn(f, vec[i])
  }
}

export function vectorForEach(f: L.ScamperFn, vec: L.Value[]): void {
  checkContract(arguments, contract("vector-for-each", [C.func, C.vector]))
  for (let i = 0; i < vec.length; i++) {
    L.callScamperFn(f, vec[i])
  }
}

export function forRange(start: number, end: number, f: L.ScamperFn): void {
  checkContract(
    arguments,
    contract("for-range", [C.integer, C.integer, C.func]),
  )
  if (start < end) {
    for (let i = start; i < end; i++) {
      L.callScamperFn(f, i)
    }
  } else {
    for (let i = start; i > end; i--) {
      L.callScamperFn(f, i)
    }
  }
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

export function vectorFilter(f: L.ScamperFn, lst: L.Value[]): L.Value[] {
  checkContract(arguments, contract("vector-filter", [C.func, C.vector]))
  const ret = []
  for (let i = 0; i < lst.length; i++) {
    if (L.callScamperFn(f, lst[i])) {
      ret.push(lst[i])
    }
  }
  return ret
}

// TODO: implement fold/reduce variants for vectors

export function voidQ(x: any): boolean {
  checkContract(arguments, contract("void?", [C.any]))
  return x === undefined
}

export function error(msg: string): never {
  checkContract(arguments, contract("error", [C.string]))
  throw new L.ScamperError("Runtime", msg)
}

export function qq(): never {
  checkContract(arguments, contract("??", []))
  throw new L.ScamperError("Runtime", "Hole encountered in program!")
}

export function compose(...fss: (L.ScamperFn)[]): L.ScamperFn {
  checkContract(arguments, contract("compose", [C.func], C.func))
  const first = fss[fss.length - 1]
  return (x: L.Value) => {
    let ret = L.callScamperFn(first, x)
    for (let i = fss.length - 2; i >= 0; i--) {
      ret = L.callScamperFn(fss[i], ret)
    }
    return ret
  }
}

export function pipe(init: L.Value, ...fs: (L.ScamperFn)[]): L.Value {
  checkContract(arguments, contract("|>", [C.any, C.func], C.func))
  let acc = init
  for (let i = 0; i < fs.length; i++) {
    acc = L.callScamperFn(fs[i], acc)
  }
  return acc
}

export function range(...args: number[]): L.List {
  checkContract(arguments, contract("range", [], C.number))
  if (args.length === 0 || args.length > 3) {
    throw new L.ScamperError(
      "Runtime",
      "1, 2, or 3 numbers must be passed to function",
    )
  } else {
    const m = args.length === 1 ? 0 : args[0]
    const n = args.length === 1 ? args[0] : args[1]
    const step = args.length < 3 ? 1 : args[2]
    const arr = []
    // N.B., to prevent the internal infinite loop that would result
    // from having a zero step.
    if (step === 0) {
      throw new L.ScamperError("Runtime", '"step" argument must be non-zero')
    }
    for (let i = m; step > 0 ? i < n : i > n; i += step) {
      arr.push(i)
    }
    return L.vectorToList(arr)
  }
}

export function random(n: number): number {
  checkContract(arguments, contract("random", [C.integer]))
  return Math.floor(Math.random() * n)
}

export function withHandler(
  handler: L.ScamperFn,
  fn: L.ScamperFn,
  ...args: L.Value[]
): L.Value {
  checkContract(arguments, contract("with-handler", [C.func, C.func], C.any))
  try {
    return L.callScamperFn(fn, ...args)
  } catch (e) {
    // TODO: subthreads could theoretically throw multiple errors, don't know how we will address that
    if (e instanceof SubthreadErrors && e.errors.length == 1) {
      return L.callScamperFn(handler, e.errors[0].message)
    }
    if (e instanceof L.ScamperError) {
      return L.callScamperFn(handler, e.message)
    }
    // shouldn't happen, but if it's not a ScamperError, just try to cast and see what happens.
    return L.callScamperFn(handler, (e as L.ScamperError).message)
  }
}

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

// Additional Scamper-specific functions

export function ignore(_v: L.Value): HTMLElement {
  checkContract(arguments, contract("ignore", [C.any]))
  const ret = document.createElement("div")
  ret.style.display = "non"
  return ret
}

export function setMaximumRecursionDepth(n: number): any {
  checkContract(arguments, contract("set-maximum-recursion-depth", [C.nat]))
  return {
    [L.scamperTag]: "set-maximum-recursion-depth",
    value: n,
  }
}

export function stringToWords(s: string): L.List {
  checkContract(arguments, contract("string->words", [C.string]))
  const words = s.split(/\s+/)
  for (let i = 0; i < words.length; i++) {
    words[i] = words[i].replace(/[.,;?:!]$/, "")
  }
  return L.vectorToList(words.filter((w) => w.length > 0))
}

interface Ref extends L.Struct {
  [L.structKind]: "ref"
  value: L.Value
}

export function ref(v: L.Value): Ref {
  checkContract(arguments, contract("ref", [C.any]))
  return {
    [L.scamperTag]: "struct",
    [L.structKind]: "ref",
    value: v,
  }
}

export function isRef(v: L.Value): boolean {
  return L.isStructKind(v, "ref")
}

export function deref(r: Ref): L.Value {
  checkContract(arguments, contract("deref", [C.struct("ref")]))
  return r.value
}

export function refSet(r: Ref, v: L.Value): void {
  checkContract(arguments, contract("ref-set!", [C.struct("ref"), C.any]))
  r.value = v
}

// Additional constants

export const elseConst = true
export const nullConst = null
export const piConst = Math.PI
export const voidConst = undefined


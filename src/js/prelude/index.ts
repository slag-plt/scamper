import * as L from "../../lpm"
import { SubthreadErrors } from "../../lpm"

export * from "./files.js"

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

export function prelude_equalQ(x: any, y: any): boolean {
  return L.equals(x, y)
}

// Numbers (6.2)

export function prelude_numberQ(x: any): boolean {
  return typeof x === "number"
}

export function prelude_realQ(x: any): boolean {
  return typeof x === "number" && !Number.isInteger(x)
}

export function prelude_integerQ(x: any): boolean {
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

export function prelude_nanQ(x: any): boolean {
  return Number.isNaN(x)
}

export function prelude_lt(x: number, y: number): boolean {
  return x < y
}

export function prelude_leq(x: number, y: number): boolean {
  return x <= y
}

export function prelude_gt(x: number, y: number): boolean {
  return x > y
}

export function prelude_geq(x: number, y: number): boolean {
  return x >= y
}

export function prelude_eq(x: number, y: number): boolean {
  return x === y
}

export function prelude_eqEps(eps: number): L.ScamperFn {
  const eq = function (x: number, y: number): boolean {
    return Math.abs(x - y) <= eps
  } as L.JsFunction
  L.nameFn(`(=-eps ${eps.toString()})`, eq)
  return eq
}

export function prelude_zeroQ(x: number): boolean {
  return x === 0
}

export function prelude_positiveQ(x: number): boolean {
  return x > 0
}

export function prelude_negativeQ(x: number): boolean {
  return x < 0
}

export function prelude_oddQ(x: number): boolean {
  return (x & 1) === 1
}

export function prelude_evenQ(x: number): boolean {
  return (x & 1) !== 1
}

export function prelude_max(...xs: number[]): number {
  return Math.max(...xs)
}

export function prelude_min(...xs: number[]): number {
  return Math.min(...xs)
}

export function prelude_plus(...xs: number[]): number {
  return xs.reduce((a, b) => a + b, 0)
}

export function prelude_minus(...xs: number[]): number {
  return xs.length === 1 ? -xs[0] : xs.reduce((a, b) => a - b)
}

export function prelude_times(...xs: number[]): number {
  return xs.reduce((a, b) => a * b, 1)
}

export function prelude_div(...xs: number[]): number {
  return xs.length === 1 ? 1 / xs[0] : xs.reduce((a, b) => a / b)
}

export function prelude_abs(x: number): number {
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

export function prelude_quotient(x: number, y: number): number {
  return Math.trunc(x / y)
}

export function prelude_remainder(x: number, y: number): number {
  return x % y
}

export function prelude_modulo(x: number, y: number): number {
  return ((x % y) + y) % y
}

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)

// N.B., we don't implement:
//   (numerator q)
//   (denominator q)
// Since we don't implement rationals.

export function prelude_floor(x: number): number {
  return Math.floor(x)
}

export function prelude_ceiling(x: number): number {
  return Math.ceil(x)
}

export function prelude_truncate(x: number): number {
  return Math.trunc(x)
}

export function prelude_round(x: number): number {
  return Math.round(x)
}

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

export function prelude_square(x: number): number {
  return x * x
}

export function prelude_sqrt(x: number): number {
  return Math.sqrt(x)
}

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

export function prelude_expt(x: number, y: number): number {
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

export function prelude_numberToString(x: number): string {
  return x.toString()
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

export function prelude_stringToNumber(s: string): number {
  if (/^[+-]?\d+$/.test(s)) {
    return parseInt(s)
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return parseFloat(s)
  } else {
    throw new Error(`Runtime error: string->number: invalid string: ${s}`)
  }
}

// Additional functions from racket/base

export function prelude_exp(x: number): number {
  return Math.exp(x)
}

export function prelude_log(x: number): number {
  return Math.log(x)
}

export function prelude_sin(x: number): number {
  return Math.sin(x)
}

export function prelude_cos(x: number): number {
  return Math.cos(x)
}

export function prelude_tan(x: number): number {
  return Math.tan(x)
}

export function prelude_asin(x: number): number {
  return Math.asin(x)
}

export function prelude_acos(x: number): number {
  return Math.acos(x)
}

export function prelude_atan(x: number): number {
  return Math.atan(x)
}

export function prelude_equalsEps(eps: number): L.Value {
  const name = `=-(${eps})`
  const ret = function (x: number, y: number): boolean {
    return Math.abs(x - y) <= eps
  }
  L.nameFn(name, ret)
  return ret
}

// Booleans (6.3)

export function prelude_not(x: boolean): boolean {
  return !x
}

export function prelude_booleanQ(x: any): boolean {
  return typeof x === "boolean"
}

// From racket/base

export function prelude_nand(...xs: boolean[]): boolean {
  return !xs.reduce((a, b) => a && b, true)
}

export function prelude_nor(...xs: boolean[]): boolean {
  return !xs.reduce((a, b) => a || b, false)
}

export function prelude_implies(x: boolean, y: boolean): boolean {
  return !x || y
}

export function prelude_xor(x: boolean, y: boolean): boolean {
  return (x && !y) || (!x && y)
}

// Additional functions

export function prelude_anyOf(...fns: L.ScamperFn[]): L.ScamperFn {
  return function (v: any): boolean {
    for (let i = 0; i < fns.length; i++) {
      if (L.callScamperFn(fns[i], v)) {
        return true
      }
    }
    return false
  }
}

export function prelude_allOf(...fns: L.ScamperFn[]): L.ScamperFn {
  return function (v: any): boolean {
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

export function prelude_pairQ(x: any): boolean {
  return L.isPair(x)
}

export function prelude_listOf(pred: L.ScamperFn): L.ScamperFn {
  return function (l: L.List): boolean {
    // N.B., list-of returns false if the input is _not_ a list
    if (!prelude_listQ(l)) {
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

export function prelude_cons(x: any, y: any): L.Value {
  return L.mkCons(x, y)
}

export function prelude_pair(x: any, y: any): L.Value {
  return L.mkPair(x, y)
}

export function prelude_car(x: L.Value): L.Value {
  if (L.isPair(x)) {
    return (x as any).fst
  } else {
    return (x as any).head
  }
}

export function prelude_cdr(x: L.Value): L.Value {
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
export const prelude_listAccessorFns: Record<string, (x: L.Value) => L.Value> = {}
listAccessors.forEach((name) => {
  const path = name.slice(1, name.length - 1)
  const fn = function (x: L.Value): L.Value {
    let ret = path.endsWith("a") ? prelude_car(x) : prelude_cdr(x)
    for (let i = path.length - 2; i >= 0; i--) {
      ret = path[i] === "a" ? prelude_car(ret) : prelude_cdr(ret)
    }
    return ret
  }
  prelude_listAccessorFns[`prelude_${name}`] = fn
})

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

export function prelude_nullQ(x: any): boolean {
  return x === null
}

export function prelude_listQ(x: any): boolean {
  return L.isList(x)
}

export function prelude_list(...xs: L.Value[]): L.List {
  let ret: L.List = null
  for (let i = xs.length - 1; i >= 0; i--) {
    ret = L.mkCons(xs[i], ret)
  }
  return ret
}

export function prelude_makeList(n: number, fill: L.Value): L.List {
  let ret = null
  for (let i = 0; i < n; i++) {
    ret = L.mkCons(fill, ret)
  }
  return ret
}

export function prelude_length(l: L.List): number {
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

export function prelude_append(l: L.List, ...ls: L.List[]): L.List {
  let ret = l
  for (let i = 0; i < ls.length; i++) {
    ret = appendOne_(ret, ls[i])
  }
  return ret
}

export function prelude_reverse(l: L.List): L.List {
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

export function prelude_listTail(l: L.List, k: number): L.List {
  while (l !== null && k > 0) {
    l = l.tail
    k -= 1
  }
  return l
}

export function prelude_listTake(l: L.List, k: number): L.List {
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

export function prelude_listDrop(l: L.List, k: number): L.List {
  while (l !== null && k > 0) {
    l = l.tail
    k -= 1
  }
  return l
}

export function prelude_listRef(l: L.List, n: number): L.Value {
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

export function prelude_indexOf(l: L.List, v: L.Value): number {
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

export function prelude_assocKey(v: L.Value, l: L.List): boolean {
  while (l !== null) {
    if (L.equals((l.head as L.Pair).fst, v)) {
      return true
    }
    l = l.tail
  }
  return false
}

export function prelude_assocRef(v: L.Value, l: L.List): L.Value {
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

export function prelude_assocSet(k: L.Value, v: L.Value, l: L.List): L.List {
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

export function prelude_sort(l: L.List, lt: L.ScamperFn): L.List {
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

export function prelude_charQ(x: any): boolean {
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

export const prelude_charCompareFns: Record<string, (...args: L.Value[]) => boolean> = {}
function mkCharCompareFn(
  name: string,
  f: (a: string, b: string) => boolean,
): void {
  const fn = function (...args: L.Value[]) {
    return pairwiseSatisfies(
      (a, b) => f((a as L.Char).value, (b as L.Char).value),
      args,
    )
  }
  L.nameFn(name, fn)
  prelude_charCompareFns[`prelude_${name}`] = fn
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

export const prelude_charPredicateFns: Record<string, (x: L.Char) => boolean> = {}
function mkCharPredicatePrim(name: string, f: (a: string) => boolean): void {
  const fn = function (x: L.Char) {
    return f(x.value)
  }
  L.nameFn(name, fn)
  prelude_charPredicateFns[`prelude_${name}`] = fn
}

mkCharPredicatePrim("char-alphabetic?", (a) => /\p{L}/gu.test(a))
mkCharPredicatePrim("char-numeric?", (a) => /\p{N}/gu.test(a))
mkCharPredicatePrim("char-whitespace?", (a) => /\p{Z}/gu.test(a))
mkCharPredicatePrim("char-upper-case?", (a) => /\p{Lu}/gu.test(a))
mkCharPredicatePrim("char-lower-case?", (a) => /\p{Ll}/gu.test(a))

export function prelude_digitalValue(c: L.Char): number {
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

export function prelude_charToInteger(c: L.Char): number {
  return c.value.codePointAt(0)!
}

export function prelude_integerToChar(n: number): L.Char {
  return L.mkChar(String.fromCodePoint(n))
}

export function prelude_charUpcase(c: L.Char): L.Char {
  return L.mkChar(c.value.toUpperCase())
}

export function prelude_charDowncase(c: L.Char): L.Char {
  return L.mkChar(c.value.toLowerCase())
}

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
export function prelude_charFoldcase(c: L.Char): L.Char {
  return L.mkChar(c.value.toLowerCase())
}

// Strings (6.7)

export function prelude_stringQ(x: any): boolean {
  return typeof x === "string"
}

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
export function prelude_makeString(k: number, c: L.Char): string {
  return c.value.repeat(k)
}

export function prelude_string(c: L.Char, ...cs: L.Char[]): string {
  return [c, ...cs].map((e) => e.value).join("")
}

export function prelude_stringLength(s: string): number {
  return s.length
}

export function prelude_stringRef(s: string, i: number): L.Char {
  return L.mkChar(s[i])
}

// N.B., string-set! is unimplemented since it is effectful.

export const prelude_stringCompareFns: Record<string, (...args: string[]) => boolean> = {}
function mkStringCompareFn(
  name: string,
  f: (a: string, b: string) => boolean,
): void {
  const fn = function (...args: string[]) {
    return pairwiseSatisfies((a, b) => f(a, b), args)
  }
  L.nameFn(name, fn)
  prelude_stringCompareFns[`prelude_${name}`] = fn
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

export function prelude_stringUpcase(s: string): string {
  return s.toUpperCase()
}

export function prelude_stringDowncase(s: string): string {
  return s.toLowerCase()
}

export function prelude_stringFoldcase(s: string): string {
  return s.toLowerCase()
}

export function prelude_substring(s: string, start: number, end: number): string {
  return s.substring(start, end)
}

export function prelude_stringAppend(...args: string[]): string {
  return args.join("")
}

// TODO: stringToList has a 3-argument version, too, that specifies
// a substring of s to turn into a list.
export function prelude_stringToList(s: string): L.List {
  let ret = null
  for (let i = s.length - 1; i >= 0; i--) {
    ret = L.mkCons(L.mkChar(s[i]), ret)
  }
  return ret
}

export function prelude_listToString(l: L.List): string {
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

export function prelude_stringToVector(s: string): L.Char[] {
  const ret = []
  for (let i = 0; i < s.length; i++) {
    ret.push(L.mkChar(s[i]))
  }
  return ret
}

export function prelude_vectorToString(v: L.Char[]): string {
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

export function prelude_stringContains(s: string, sub: string): boolean {
  return s.includes(sub)
}

export function prelude_stringSplit(s: string, sep: string): L.List {
  const splits = s.split(sep)
  let ret = null
  for (let i = splits.length - 1; i >= 0; i--) {
    ret = L.mkCons(splits[i], ret)
  }
  return ret
}

export function prelude_stringSplitVector(s: string, sep: string): string[] {
  return s.split(sep)
}

// Vectors (6.8)

export function prelude_vectorQ(x: any): boolean {
  return L.isArray(x)
}

export function prelude_vector(...xs: L.Value[]): L.Value[] {
  return xs
}

export function prelude_makeVector(n: number, fill: L.Value): L.Value[] {
  const ret = []
  for (let i = 0; i < n; i++) {
    ret.push(fill)
  }
  return ret
}

export function prelude_vectorLength(v: L.Value[]): number {
  return v.length
}

export function prelude_vectorRef(v: L.Value[], i: number): L.Value {
  return v[i]
}

export function prelude_vectorSet(v: L.Value[], i: number, x: L.Value): void {
  v[i] = x
}

export function prelude_vectorFill(v: L.Value[], x: L.Value): void {
  for (let i = 0; i < v.length; i++) {
    v[i] = x
  }
}

export function prelude_vectorToList(v: L.Value[]): L.List {
  let ret = null
  for (let i = v.length - 1; i >= 0; i--) {
    ret = L.mkCons(v[i], ret)
  }
  return ret
}

export function prelude_listToVector(l: L.List): L.Value[] {
  const ret = []
  while (l !== null) {
    ret.push(l.head)
    l = l.tail
  }
  return ret
}

export function prelude_vectorRange(...args: number[]): number[] {
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

export function prelude_vectorAppend(...vecs: L.Value[][]): L.Value[] {
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

export function prelude_procedureQ(x: any): boolean {
  return L.isClosure(x) || L.isJsFunction(x)
}

export function prelude_stringMap(f: L.ScamperFn, s: string): string {
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

export function prelude_map(f: L.ScamperFn, ...lsts: L.List[]): L.List {
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

export function prelude_filter(f: L.ScamperFn, lst: L.List): L.List {
  const values = []
  while (lst !== null) {
    if (L.callScamperFn(f, lst.head)) {
      values.push(lst.head)
    }
    lst = lst.tail
  }
  return L.vectorToList(values)
}

export function prelude_fold(f: L.ScamperFn, init: L.Value, lst: L.List): L.Value {
  let acc = init
  while (lst !== null) {
    acc = L.callScamperFn(f, acc, lst.head)
    lst = lst.tail
  }
  return acc
}

export function prelude_reduce(f: L.ScamperFn, lst: L.List): L.Value {
  let acc = lst!.head
  lst = lst!.tail
  while (lst !== null) {
    acc = L.callScamperFn(f, acc, lst.head)
    lst = lst.tail
  }
  return acc
}

export function prelude_foldLeft(
  f: L.ScamperFn,
  init: L.Value,
  lst: L.List,
): L.Value {
  let acc = init
  while (lst !== null) {
    acc = L.callScamperFn(f, lst.head, acc)
    lst = lst.tail
  }
  return acc
}

export function prelude_foldRight(
  f: L.ScamperFn,
  init: L.Value,
  lst: L.List,
): L.Value {
  const values = L.listToVector(lst)
  let acc = init
  for (let i = values.length - 1; i >= 0; i--) {
    // N.B., the type of the higher-order function is a -> b -> b!
    acc = L.callScamperFn(f, values[i], acc)
  }
  return acc
}

export function prelude_reduceRight(f: L.ScamperFn, lst: L.List): L.Value {
  const values = L.listToVector(lst)
  let acc = values.pop()
  for (let i = values.length - 1; i >= 0; i--) {
    // N.B., the type of the higher-order function is a -> b -> b!
    acc = L.callScamperFn(f, values[i], acc)
  }
  return acc
}

export function prelude_vectorMap(f: L.ScamperFn, ...vecs: L.Value[][]): L.Value[] {
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

export function prelude_vectorMapBang(f: L.ScamperFn, vec: L.Value[]): void {
  for (let i = 0; i < vec.length; i++) {
    vec[i] = L.callScamperFn(f, vec[i])
  }
}

export function prelude_vectorForEach(f: L.ScamperFn, vec: L.Value[]): void {
  for (let i = 0; i < vec.length; i++) {
    L.callScamperFn(f, vec[i])
  }
}

export function prelude_forRange(start: number, end: number, f: L.ScamperFn): void {
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

export function prelude_vectorFilter(f: L.ScamperFn, lst: L.Value[]): L.Value[] {
  const ret = []
  for (let i = 0; i < lst.length; i++) {
    if (L.callScamperFn(f, lst[i])) {
      ret.push(lst[i])
    }
  }
  return ret
}

// TODO: implement fold/reduce variants for vectors

export function prelude_voidQ(x: any): boolean {
  return x === undefined
}

export function prelude_qq(): never {
  throw new L.ScamperError("Runtime", "Hole encountered in program!")
}

export function prelude_compose(...fss: (L.ScamperFn)[]): L.ScamperFn {
  const first = fss[fss.length - 1]
  return (x: L.Value) => {
    let ret = L.callScamperFn(first, x)
    for (let i = fss.length - 2; i >= 0; i--) {
      ret = L.callScamperFn(fss[i], ret)
    }
    return ret
  }
}

export function prelude_pipe(init: L.Value, ...fs: (L.ScamperFn)[]): L.Value {
  let acc = init
  for (let i = 0; i < fs.length; i++) {
    acc = L.callScamperFn(fs[i], acc)
  }
  return acc
}

export function prelude_range(...args: number[]): L.List {
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

export function prelude_random(n: number): number {
  return Math.floor(Math.random() * n)
}

export function prelude_withHandler(
  handler: L.ScamperFn,
  fn: L.ScamperFn,
  ...args: L.Value[]
): L.Value {
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

export function prelude_ignore(_v: L.Value): HTMLElement {
  const ret = document.createElement("div")
  ret.style.display = "non"
  return ret
}

export function prelude_setMaximumRecursionDepth(n: number): any {
  return {
    [L.scamperTag]: "set-maximum-recursion-depth",
    value: n,
  }
}

export function prelude_stringToWords(s: string): L.List {
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

export function prelude_ref(v: L.Value): Ref {
  return {
    [L.scamperTag]: "struct",
    [L.structKind]: "ref",
    value: v,
  }
}

export function prelude_isRef(v: L.Value): boolean {
  return L.isStructKind(v, "ref")
}

export function prelude_deref(r: Ref): L.Value {
  return r.value
}

export function prelude_refSet(r: Ref, v: L.Value): void {
  r.value = v
}

// Additional constants

export const prelude_elseConst = true
export const prelude_nullConst = null
export const prelude_piConst = Math.PI
export const prelude_voidConst = undefined


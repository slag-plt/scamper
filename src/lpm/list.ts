import { ScamperError } from './error.js'

import * as L from './lang.js'
import * as U from './util.js'

// N.B., We follow Clojure's lead and distinguish between pairs and lists
// explicitly. While they are defined as algebraic datatypes, pairs and lists
// are common enough that are "built-in" datatypes to the runtime.

/**
 * A pair is an algebraic datatype with a first and second component.
 */
export interface Pair extends L.Struct {
  [L.scamperTag]: 'struct',
  [L.structKind]: 'pair',
  fst: L.Value,
  snd: L.Value
}

/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends L.Struct {
  [L.scamperTag]: 'struct',
  [L.structKind]: 'cons',
  head: L.Value,
  tail: List
}

/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons

export const isPair = (v: L.Value): v is Pair => U.isStructKind<Pair>(v, 'pair')
export const isList = (v: L.Value): v is List => v === null || U.isStructKind<Cons>(v, 'cons')

export const mkPair = (fst: L.Value, snd: L.Value): Pair => ({
  [L.scamperTag]: 'struct',
  [L.structKind]: 'pair',
  fst,
  snd
})

export const mkCons = (head: L.Value, tail: List): Cons => {
  if (!isList(tail)) {
    throw new ScamperError('Runtime', 'The second argument to cons should be a list')
  } else {
    return {
      [L.scamperTag]: 'struct',
      [L.structKind]: 'cons',
      head,
      tail
    }
  }
}

/** @return a vector (array) representation of the input list. */
export function listToVector (l: List): L.Value[] {
  const ret: L.Value[] = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.head)
    cur = cur.tail as List
  }
  return ret
}

/** @return a list representation of the input vector (array). */
export function vectorToList (arr: L.Value[]): List {
  let ret: List = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkCons(arr[i], ret)
  }
  return ret
}

export const mkList = (...values: L.Value[]): List => vectorToList(values)

/** @returns the nth element of the list */
export function listNth (n: number, l: List): L.Value {
  if (n < 0) {
    throw new ScamperError('Runtime', `Cannot access negative index ${n} in list`)
  }
  let cur = l
  for (let i = 0; i < n; i++) {
    if (cur === null) {
      throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
    }
    cur = cur.tail as List
  }
  if (cur == null) {
    throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
  } else {
    return cur.head
  }
}
import { ICE, ScamperError } from './error.js'
import { Range } from './range.js'
import * as L from './lang.js'

///// Predicates /////////////////////////////////////////////////////////////////

export const isNumber = (v: L.Value): v is number => typeof v === 'number'
export const isBoolean = (v: L.Value): v is boolean => typeof v === 'boolean'
export const isString = (v: L.Value): v is string => typeof v === 'string'
export const isNull = (v: L.Value): v is null => v === null
export const isVoid = (v: L.Value): v is undefined => v === undefined
export const isArray = (v: L.Value): v is L.Value[] => Array.isArray(v)
export const isTaggedObject = (v: L.Value): v is L.TaggedObject =>
  v !== null &&
  typeof v === 'object' &&
  Object.prototype.hasOwnProperty.call(v, L.scamperTag)
export const isJsFunction = (v: L.Value): v is L.JsFunction =>
  typeof v === 'function'
export const isClosure = (v: L.Value): v is L.Closure =>
  isTaggedObject(v) && v[L.scamperTag] === 'closure'
export const isFunction = (v: L.Value): v is L.ScamperFn =>
  isJsFunction(v) || isClosure(v)
export const isChar = (v: L.Value): v is L.Char =>
  isTaggedObject(v) && v[L.scamperTag] === 'char'
/**
 * A map value: a *plain* Javascript object, i.e. what a `{...}` literal builds
 * (and what JSON-shaped library data looks like). Deliberately excludes every
 * tagged value and every class instance -- an HTMLElement, an Error, a struct,
 * or a Chart.js option bag is not a map and keeps its own handling.
 */
export const isObj = (v: L.Value): v is Record<string, L.Value> =>
  typeof v === 'object' &&
  v !== null &&
  Object.getPrototypeOf(v) === Object.prototype &&
  !isTaggedObject(v)
export const isStruct = (v: L.Value): v is L.Struct =>
  isTaggedObject(v) && v[L.scamperTag] === 'struct'
// N.B., `T` appears only in the return type on purpose: it is how a caller
// says which struct it is testing for -- `isStructKind<Cons>(v, 'cons')` -- so
// that the narrowed value has that struct's fields. The kind string is what
// actually does the checking.
// eslint-disable-next-line @typescript-eslint/no-unnecessary-type-parameters
export const isStructKind = <T extends L.Struct>(
  v: L.Value,
  k: string,
): v is T => isStruct(v) && v[L.structKind] === k

export const isPair = (v: L.Value): v is L.Pair =>
  isStructKind<L.Pair>(v, 'pair')
export const isList = (v: L.Value): v is L.List =>
  v === null || isStructKind<L.Cons>(v, 'cons')

///// Constructors /////////////////////////////////////////////////////////////

// TODO: remove call
export const mkClosure = (
  params: L.Id[],
  code: L.Blk,
  env: L.Scope[],
  call: (...args: L.Value[]) => L.Value,
  name?: L.Id,
  restParam?: string,
  origin: L.CodeOrigin = 'user',
  home?: L.Env
  // Omit `home` when unset so an ordinary closure keeps the exact shape it had
  // before module-home resolution existed (only a qualified/private-module
  // closure carries one); see Closure.home.
): L.Closure => ({ [L.scamperTag]: 'closure', params, code, locals: env, call, name, restParam, origin, ...(home !== undefined ? { home } : {}) })
export const mkChar = (v: string): L.Char => ({
  [L.scamperTag]: 'char',
  value: v,
})
export const mkStruct = (
  kind: string,
  fields: string[],
  values: L.Value[],
): L.Struct => {
  const ret: L.Struct = { [L.scamperTag]: 'struct', [L.structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

export const mkPair = (fst: L.Value, snd: L.Value): L.Pair => ({
  [L.scamperTag]: 'struct',
  [L.structKind]: 'pair',
  fst,
  snd,
})

/**
 * @param tail is taken as a `Value` rather than a `List` because this checks it
 *        itself: the check is what enforces the invariant, so demanding the
 *        narrower type from callers only pushed a cast onto each of them.
 */
export const mkCons = (head: L.Value, tail: L.Value): L.Cons => {
  if (!isList(tail)) {
    throw new ScamperError(
      'Runtime',
      'The second argument to cons should be a list',
    )
  } else {
    return {
      [L.scamperTag]: 'struct',
      [L.structKind]: 'cons',
      head,
      tail,
    }
  }
}

export const mkList = (...values: L.Value[]): L.List => vectorToList(values)

// Op constructors
export const mkLit = (
  value: L.Value,
  range: Range = Range.none,
  provenance?: L.Provenance,
): L.Lit => ({
  tag: 'lit',
  value,
  range,
  provenance,
})
export const mkVar = (name: string, range: Range = Range.none): L.Var => ({
  tag: 'var',
  name,
  range,
})
export const mkCls = (
  params: string[],
  body: L.Blk,
  name?: string,
  range: Range = Range.none,
  restParam?: string,
  provenance?: L.Provenance,
): L.Cls => ({ tag: 'cls', params, body, name, range, restParam, provenance })
export const mkAp = (
  numArgs: number,
  range: Range = Range.none,
  provenance?: L.Provenance,
): L.Ap => ({
  tag: 'ap',
  numArgs,
  range,
  provenance,
})
export const mkMatch = (
  branches: [L.Pat, L.Blk][],
  range: Range = Range.none,
): L.Match => ({ tag: 'match', branches, range })
export const mkLet = (
  bindings: { pat: L.Pat; value: L.Blk; failMsg?: string }[],
  body: L.Blk,
  range: Range = Range.none,
  idx = 0,
  provenance?: L.Provenance,
): L.Let => ({ tag: 'let', bindings, body, range, idx, provenance })

/** @return the variable names bound by an LPM pattern (recursively). */
export const patVars = (pat: L.Pat): string[] => {
  switch (pat.tag) {
    case 'pvar':
      return [pat.name]
    case 'pctor':
    case 'pvec':
      return pat.args.flatMap(patVars)
    case 'pwild':
    case 'plit':
      return []
  }
}
export const mkIf = (
  thenB: L.Blk,
  elseB: L.Blk,
  range: Range = Range.none,
  provenance?: L.Provenance,
): L.If => ({ tag: 'if', thenB, elseB, range, provenance })
export const mkPopScope = (range: Range = Range.none): L.PopScope => ({
  tag: 'pop-scope',
  range,
})
export const mkDisp = (expr: L.Blk, range: Range = Range.none): L.Disp => ({
  tag: 'disp',
  expr,
  range,
})
export const mkDefine = (
  name: string,
  expr: L.Blk,
  range: Range = Range.none,
): L.Define => ({ tag: 'define', name, expr, range })
export const mkImport = (
  name: string,
  kind: 'builtin' | 'file',
  range: Range = Range.none,
  alias?: string,
): L.Import => ({
  tag: 'import',
  name,
  kind,
  range,
  ...(alias !== undefined ? { alias } : {}),
})
export const mkStmtExp = (
  expr: L.Blk,
  range: Range = Range.none,
): L.StmtExp => ({ tag: 'stmtexp', expr, range })
export const mkExport = (
  names: string[],
  range: Range = Range.none,
): L.Export => ({ tag: 'export', names, range })
export const mkApSpread = (range: Range = Range.none): L.ApSpread => ({
  tag: 'ap-spread',
  range,
})
export const mkPushHandler = (range: Range = Range.none): L.PushHandler => ({
  tag: 'push-handler',
  range,
})
export const mkPopHandler = (range: Range = Range.none): L.PopHandler => ({
  tag: 'pop-handler',
  range,
})

// Pattern constructors
export const mkPWild = (range: Range = Range.none): L.PWild => ({
  tag: 'pwild',
  range,
})
export const mkPLit = (value: L.Value, range: Range = Range.none): L.PLit => ({
  tag: 'plit',
  value,
  range,
})
export const mkPVar = (name: string, range: Range = Range.none): L.PVar => ({
  tag: 'pvar',
  name,
  range,
})
export const mkPCtor = (
  name: string,
  args: L.Pat[],
  range: Range = Range.none,
): L.PCtor => ({ tag: 'pctor', name, args, range })
export const mkPVec = (
  args: L.Pat[],
  range: Range = Range.none,
): L.PVec => ({ tag: 'pvec', args, range })

///// Utility Functions ////////////////////////////////////////////////////////

/**
 * Pattern matches value `v` against pattern `p`, producing a list of bindings
 * if successful, or `undefined` if the match fails.
 * @param v the scrutinee value
 * @param p the pattern value
 * @returns a list of bindings if successful, null if unsuccessful
 */
export function pMatch(v: L.Value, p: L.Pat): [string, L.Value][] | null {
  switch (p.tag) {
    case 'pwild': {
      return []
    }

    case 'plit': {
      if (equals(v, p.value)) {
        return []
      }
      return null
    }

    case 'pvar': {
      return [[p.name, v]]
    }

    case 'pctor': {
      if (isStructKind(v, p.name)) {
        const flds = getFieldsOfStruct(v)
        if (flds.length !== p.args.length) {
          return null
        }
        const bindings: [string, L.Value][] = []
        for (let i = 0; i < flds.length; i++) {
          const pat = p.args[i]
          const val = v[flds[i]]
          const match = pMatch(val, pat)
          if (!match) {
            return null
          }
          bindings.push(...match)
        }
        return bindings
      }
      return null
    }

    case 'pvec': {
      // A vector pattern matches an array of exactly the same length, element
      // by element. A length mismatch is an ordinary failed match (the branch
      // falls through), not an error.
      if (!isArray(v) || v.length !== p.args.length) {
        return null
      }
      const bindings: [string, L.Value][] = []
      for (let i = 0; i < p.args.length; i++) {
        const match = pMatch(v[i], p.args[i])
        if (!match) {
          return null
        }
        bindings.push(...match)
      }
      return bindings
    }
  }
}

/** @return true iff the given field name is a hidden field of a struct. */
export function isHiddenField(fld: string): boolean {
  return fld.startsWith('##') && fld.endsWith('##')
}

/**
 * Removes and returns the top of `stack`, raising an ICE if there is none.
 *
 * Codegen guarantees the operand is there, so an empty stack means the runtime
 * has gone wrong rather than the program -- worth saying loudly rather than
 * carrying an `undefined` forward to fail somewhere unrelated.
 *
 * N.B. the length is what is checked, not the popped value: `undefined` is a
 * perfectly good Scamper value (it is `void`), so a stack really can hold one.
 *
 * @param what names the stack, for the message.
 */
export function popRequired<T>(stack: T[], what: string): T {
  if (stack.length === 0) {
    throw new ICE('popRequired', `${what} was empty`)
  }
  const top = stack[stack.length - 1]
  stack.length -= 1
  return top
}

/**
 * Removes and returns the *front* of `queue`, raising an ICE if there is none.
 * The companion to {@link popRequired}, and empty for the same reason.
 */
export function shiftRequired<T>(queue: T[], what: string): T {
  if (queue.length === 0) {
    throw new ICE('shiftRequired', `${what} was empty`)
  }
  const front = queue[0]
  queue.splice(0, 1)
  return front
}

/** @return a list of the fields of the given struct. */
export function getFieldsOfStruct(s: L.Struct): string[] {
  const ret: string[] = []
  for (const f in s) {
    if (!isHiddenField(f)) {
      ret.push(f)
    }
  }
  return ret
}

/** Mutates a Javascript function to contain a `name` field with that function's name. */
export const nameFn = <T extends (...args: never[]) => unknown>(
  name: string,
  fn: T,
): T => Object.defineProperty(fn, 'name', { value: name })

// N.B., the char-value conversions are actually language specific,
// so there's probably a need to refactor this and all dependent
// code, e.g., printing to the language-specific modules in the future.

export const namedCharValues = new Map([
  ['alarm', String.fromCharCode(7)],
  ['backspace', String.fromCharCode(8)],
  ['delete', String.fromCharCode(127)],
  ['escape', String.fromCharCode(27)],
  ['newline', String.fromCharCode(10)],
  ['null', String.fromCharCode(0)],
  ['return', String.fromCharCode(13)],
  ['space', ' '],
  ['tab', String.fromCharCode(9)],
])

export const charNamedValues = new Map(
  [...namedCharValues.entries()].map(([k, v]) => [v, k]),
)

export function charToName(c: string): string {
  return charNamedValues.get(c) ?? c
}

/** Each character a string literal cannot hold verbatim, paired with the
 *  escape sequence `parseStringLiteral` reads back as that character. The
 *  backslash and the quote would end or reopen the literal; the rest are the
 *  control characters the reader names, which would otherwise print raw. */
const stringLiteralEscapes = new Map([
  ['\\', '\\\\'],
  ['"', '\\"'],
  ['\x07', '\\a'],
  ['\b', '\\b'],
  ['\t', '\\t'],
  ['\n', '\\n'],
  ['\v', '\\v'],
  ['\f', '\\f'],
  ['\r', '\\r'],
  ['\x1b', '\\e'],
])

/** @return `s` with every character that string-literal syntax cannot hold
 *  verbatim replaced by its escape sequence, so that wrapping the result in
 *  quotes yields a literal the reader reads back as `s`. Inverse of the
 *  reader's `parseStringLiteral`. */
export function escapeStringLiteral(s: string): string {
  // Spreading walks the string by code point, which is what is wanted here: an
  // astral character stays one piece and passes through the table untouched,
  // where indexing would split it into surrogates.
  // eslint-disable-next-line @typescript-eslint/no-misused-spread
  return [...s].map((c) => stringLiteralEscapes.get(c) ?? c).join('')
}

/** @return a vector (array) representation of the input list. */
export function listToVector(l: L.List): L.Value[] {
  const ret: L.Value[] = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.head)
    cur = cur.tail
  }
  return ret
}

/** @return a list representation of the input vector (array). */
export function vectorToList(arr: L.Value[]): L.List {
  let ret: L.List = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkCons(arr[i], ret)
  }
  return ret
}

/** @returns the nth element of the list */
export function listNth(n: number, l: L.List): L.Value {
  if (n < 0) {
    throw new ScamperError(
      'Runtime',
      `Cannot access negative index ${n} in list`,
    )
  }
  let cur = l
  for (let i = 0; i < n; i++) {
    if (cur === null) {
      throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
    }
    cur = cur.tail
  }
  if (cur == null) {
    throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
  } else {
    return cur.head
  }
}

/** @return true if the two L.Values are structurally equal to each other. */
export function equals(v: L.Value, u: L.Value): boolean {
  // N.B., performing a strict equality check covers atomic L.Values and pointer
  // equality without the need for excessive identity checks. We reserve the
  // identity checks for our aggregate L.Values.
  if (v === u) {
    return true
  } else if (isArray(v) && isArray(u)) {
    if (v.length !== u.length) {
      return false
    }
    for (let i = 0; i < v.length; i++) {
      if (!equals(v[i], u[i])) {
        return false
      }
    }
    return true
  } else if (isChar(v) && isChar(u)) {
    return v.value === u.value
  } else if (isStruct(v) && isStruct(u)) {
    if (v[L.structKind] !== u[L.structKind]) {
      return false
    }
    const vFields = getFieldsOfStruct(v)
    const uFields = getFieldsOfStruct(u)
    if (vFields.length !== uFields.length) {
      return false
    }
    for (const f of vFields) {
      if (!equals(v[f], u[f])) {
        return false
      }
    }
    return true
  } else if (isObj(v) && isObj(u)) {
    // Two maps are equal when they have the same keys bound to equal values.
    // Key *order* is not part of a map's identity, so it is not compared.
    const vKeys = Object.keys(v)
    if (vKeys.length !== Object.keys(u).length) {
      return false
    }
    return vKeys.every(
      (k) => Object.prototype.hasOwnProperty.call(u, k) && equals(v[k], u[k]),
    )
  } else {
    return false
  }
}

/**
 * @returns the printed form of a map value, `{ "k1" : v1, "k2" : v2 }` (`{}`
 * when empty), given a renderer for its values. Shared by every backend so the
 * text and web renderings cannot drift.
 */
export function objToString(
  v: Record<string, L.Value>,
  render: (v: L.Value) => string,
): string {
  const entries = Object.keys(v).map(
    (k) => `"${escapeStringLiteral(k)}" : ${render(v[k])}`,
  )
  return entries.length === 0 ? '{}' : `{ ${entries.join(', ')} }`
}

/** @returns the type of the given value as a string (for debugging purposes) */
export function typeOf(v: L.Value): string {
  if (isNumber(v)) {
    return 'number'
  } else if (isBoolean(v)) {
    return 'boolean'
  } else if (isString(v)) {
    return 'string'
  } else if (isNull(v)) {
    return 'null'
  } else if (isVoid(v)) {
    return 'void'
  } else if (isArray(v)) {
    return 'vector'
  } else if (isJsFunction(v)) {
    return `[Function: ${v.name}]`
  } else if (isClosure(v)) {
    return `[Function: ${v.name ?? '##anonymous##'}]`
  } else if (isChar(v)) {
    return 'char'
  } else if (isPair(v)) {
    return 'pair'
  } else if (isList(v)) {
    return 'list'
  } else if (isStruct(v)) {
    return `[Struct: ${v[L.structKind]}]`
  } else if (isObj(v)) {
    return 'object'
  } else {
    return typeof v
  }
}

/** @return a generic string representation of value v. */
export function toString(v: L.Value): string {
  switch (typeof v) {
    case 'boolean':
      return v ? '#t' : '#f'
    case 'number':
      return v.toString()
    case 'string':
      return `"${escapeStringLiteral(v)}"`
    case 'undefined':
      return 'void'
    default:
      if (v === null) {
        return 'null'
      } else if (isArray(v)) {
        return v.length === 0
          ? '(vector)'
          : `(vector ${v.map(toString).join(' ')})`
      } else if (isClosure(v)) {
        return `[Function: ${v.name ?? '##anonymous##'}]`
      } else if (isFunction(v)) {
        return `[Function: ${v.name ?? '##anonymous##'}]`
      } else if (isChar(v)) {
        return `#\\${charToName(v.value)}`
      } else if (isList(v)) {
        return `(list ${listToVector(v).map(toString).join(' ')})`
      } else if (isPair(v)) {
        return `(pair ${toString(v.fst)} ${toString(v.snd)})`
      } else if (v instanceof HTMLElement) {
        // N.B., shouldn't encounter this? Need to be in browser to render...
        return '[HTMLElement]'
      } else if (isStruct(v)) {
        const name = v[L.structKind]
        const fields = getFieldsOfStruct(v)
        if (fields.length === 0) {
          return `(${name})`
        } else {
          const args = fields.map((f) => toString(v[f])).join(' ')
          return `(${name} ${args})`
        }
      } else if (v instanceof ScamperError) {
        return v.toString()
      } else if (v instanceof ICE) {
        return v.toString()
      } else if (v instanceof Error) {
        return v.toString()
      } else if (isObj(v)) {
        return objToString(v, toString)
      } else {
        return `[Blob: ${JSON.stringify(v)}]`
      }
  }
}

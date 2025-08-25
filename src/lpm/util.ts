import * as L from './lang.js'

///// Predicates /////////////////////////////////////////////////////////////////

export const isNumber = (v: L.Value): v is number => typeof v === 'number'
export const isBoolean = (v: L.Value): v is boolean => typeof v === 'boolean'
export const isString = (v: L.Value): v is string => typeof v === 'string'
export const isSymName = (v: L.Value, name: string): boolean => isSym(v) && v.value === name
export const isNull = (v: L.Value): v is null => v === null
export const isVoid = (v: L.Value): v is undefined => v === undefined
export const isArray = (v: L.Value): v is Array<L.Value> => Array.isArray(v)
export const isTaggedObject = (v: L.Value): v is L.TaggedObject =>
  v !== null && typeof v === 'object' && v.hasOwnProperty(L.scamperTag)
export const isJsFunction = (v: L.Value): v is Function => typeof v === 'function'
export const isClosure = (v: L.Value): v is L.Closure => isTaggedObject(v) && v[L.scamperTag] === 'closure'
export const isFunction = (v: L.Value): v is L.ScamperFn => isJsFunction(v) || isClosure(v)
export const isChar = (v: L.Value): v is L.Char => isTaggedObject(v) && v[L.scamperTag] === 'char'
export const isSym = (v: L.Value): v is L.Sym  => isTaggedObject(v) && v[L.scamperTag] === 'sym'
export const isStruct = (v: L.Value): v is L.Struct => isTaggedObject(v) && v[L.scamperTag] === 'struct'
export const isStructKind = <T extends L.Struct> (v: L.Value, k: string): v is T => isStruct(v) && v[L.structKind] === k

///// Constructors /////////////////////////////////////////////////////////////

export const mkClosure = (params: L.Id[], code: L.Blk, env: L.Env, call: (...args: any) => any, name?: L.Id): L.Closure =>
  ({ [L.scamperTag]: 'closure', params, code, env, call, name })
export const mkChar = (v: string): L.Char => ({ [L.scamperTag]: 'char', value: v })
export const mkSym = (v: string): L.Sym => ({ [L.scamperTag]: 'sym', value: v })
export const mkStruct = (kind: string, fields: string[], values: L.Value[]): L.Struct => {
  const ret: L.Struct = { [L.scamperTag]: 'struct', [L.structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

// Op constructors
export const mkLit = (value: L.Value): L.Lit => ({ tag: 'lit', value })
export const mkVar = (name: string): L.Var => ({ tag: 'var', name })
export const mkCtor = (name: string, fields: string[]): L.Ctor => ({ tag: 'ctor', name, fields })
export const mkCls = (params: string[], body: L.Blk, name?: string): L.Cls => ({ tag: 'cls', params, body, name })
export const mkAp = (numArgs: number): L.Ap => ({ tag: 'ap', numArgs })
export const mkMatch = (branches: [L.Pat, L.Blk][]): L.Match => ({ tag: 'match', branches })
export const mkDisp = (): L.Disp => ({ tag: 'disp' })
export const mkDefine = (name: string): L.Define => ({ tag: 'define', name })
export const mkImport = (name: string): L.Import => ({ tag: 'import', name })
export const mkRaise = (msg: string): L.Raise => ({ tag: 'raise', msg })
export const mkPops = (): L.PopS => ({ tag: 'pops' })
export const mkPopv = (): L.PopV => ({ tag: 'popv' })

// Pattern constructors
export const mkPWild = (): L.PWild => ({ tag: 'pwild' })
export const mkPLit = (value: L.Value): L.PLit => ({ tag: 'plit', value })
export const mkPVar = (name: string): L.PVar => ({ tag: 'pvar', name })
export const mkPCtor = (name: string, args: L.Pat[]): L.PCtor => ({ tag: 'pctor', name, args })

///// Utility Functions ////////////////////////////////////////////////////////

/** @return true iff the given field name is a hidden field of a struct. */
export function isHiddenField (fld: string): boolean {
  return fld.startsWith('##') && fld.endsWith('##')
}

/** @return a list of the fields of the given struct. */
export function getFieldsOfStruct (s: L.Struct): string[] {
  const ret: string[] = []
  for (const f in s) {
    if (!isHiddenField(f)) {
      ret.push(f)
    }
  }
  return ret
}

/** Mutates a Javascript function to contain a `name` field with that function's name. */
export const nameFn = (name: string, fn: Function): Function =>
  Object.defineProperty(fn, 'name', { value: name })

/** @return true if the two L.Values are structurally equal to each other. */
export function equals (v: L.Value, u: L.Value): boolean {
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
  } else {
    return false
  }
}

/** @returns the type of the given value as a string (for debugging purposes) */
export function typeOf (v: L.Value): string {
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
    return `[Function: ${(v as any).name}]`
  } else if (isClosure(v)) {
    return `[Function: ${v.name ?? '##anonymous##'}]`
  } else if (isChar(v)) {
    return `char`
  } else if (isSym(v)) {
    return `symbol`
  } else if (isStruct(v)) {
    return `[Struct: ${v[L.structKind]}]`
  } else {
    return typeof v
  }
}

/** @return a generic string representation of value v. */
export function toString (v: L.Value) {
  if (isClosure(v)) {
    return `<closure (${(v as L.Closure).params.join(' ')})>`
  } else if (typeof v === 'function') {
    return `<jsfunc: (${v.name})>`
  } else {
    return `${String(v)}`
  }
}
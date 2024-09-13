export type Noop = { tag: 'noop' }
export function mkNoop (): Noop { return { tag: 'noop' } }

export type LLoad = {
  tag: 'lload',
  localIdx: number,
  nameIdx: number
}
export function mkLLoad (localIdx: number, nameIdx: number): LLoad {
  return { tag: 'lload', localIdx, nameIdx }
}

export type LStore = {
  tag: 'lstore',
  localIdx: number,
  nameIdx: number
}
export function mkLStore (localIdx: number, nameIdx: number): LStore {
  return { tag: 'lstore', localIdx, nameIdx }
}

export type GLoad =  {
  tag: 'gload',
  nameIdx: number
}
export function mkGLoad (nameIdx: number): GLoad {
  return { tag: 'gload', nameIdx }
}

export type GStore = {
  tag: 'gstore',
  nameIdx: number
}
export function mkGStore (nameIdx: number): GStore {
  return { tag: 'gstore', nameIdx }
}

export type Int = {
  tag: 'int',
  value: number
}
export function mkInt (value: number): Int {
  return { tag: 'int', value }
}

export type Bool = {
  tag: 'bool',
  value: boolean
}
export function mkBool (value: boolean): Bool {
  return { tag: 'bool', value }
}

export type Float = {
  tag: 'float',
  value: number
}
export function mkFloat (value: number): Float {
  return { tag: 'float', value }
}

export type Obj = {
  tag: 'obj',
  objIdx: number
}
export function mkObj (objIdx: number): Obj {
  return { tag: 'obj', objIdx }
}

type JmpR = {
  tag: 'jmpr',
  offset: number
}
function mkJmpR (offset: number): JmpR {
  return { tag: 'jmpr', offset }
}

export type JmpLbl = {
  tag: 'jmplbl',
  label: number
}
export function mkJmpLbl (label: number): JmpLbl {
  return { tag: 'jmplbl', label }
}

export type BJmp = {
  tag: 'bjmp',
  offset: number
}
export function mkBJmp (offset: number): BJmp {
  return { tag: 'bjmp', offset }
}

export type Match = {
  tag: 'match',
  idx: number
}
export function mkMatch (idx: number): Match {
  return { tag: 'match', idx }
}

export type Ap = {
  tag: 'ap',
  arity: number
}
export function mkAp (arity: number): Ap {
  return { tag: 'ap', arity }
}

export type Ret = {
  tag: 'ret'
}
export function mkRet (): Ret {
  return { tag: 'ret' }
}

export type Add = {
  tag: 'add'
}
export function mkAdd (): Add {
  return { tag: 'add' }
}

export type Sub = {
  tag: 'sub'
}
export function mkSub (): Sub {
  return { tag: 'sub' }
}

export type Mult = {
  tag: 'mult'
}
export function mkMult (): Mult {
  return { tag: 'mult' }
}

export type Div = {
  tag: 'div'
}
export function mkDiv (): Div {
  return { tag: 'div' }
}

export type Lt = {
  tag: 'lt'
}
export function mkLt (): Lt {
  return { tag: 'lt' }
}

export type Lte = {
  tag: 'lte'
}
export function mkLte (): Lte {
  return { tag: 'lte' }
}

export type Gt = {
  tag: 'gt'
}
export function mkGt (): Gt {
  return { tag: 'gt' }
}

export type Gte = {
  tag: 'gte'
}
export function mkGte (): Gte {
  return { tag: 'gte' }
}

export type Eq = {
  tag: 'eq'
}
export function mkEq (): Eq {
  return { tag: 'eq' }
}

export type Neq = {
  tag: 'neq'
}
export function mkNeq (): Neq {
  return { tag: 'neq' }
}

export type Op = Noop | LLoad | LStore | GLoad | GStore | Int | Bool | Float | Obj | JmpR | JmpLbl | BJmp | Match | Ap | Ret | Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

/** A mapping from opcodes to byte tags. */
export const Tag = {
  NOOP:   0x00,
  LLOAD:  0x01,
  LSTORE: 0x02,
  GLOAD:  0x03,
  GSTORE: 0x04,
  INT:    0x05,
  BOOL:   0x06,
  FLOAT:  0x07,
  OBJ:    0x08,
  JMP:    0x09,
  BJMP:   0x0a,
  MATCH:  0x0b,
  AP:     0x0c,
  RET:    0x0d,
  ADD:    0x0e,
  SUB:    0x0f,
  MULT:   0x10,
  DIV:    0x11,
  LT:     0x12,
  LTE:    0x13,
  GT:     0x14,
  GTE:    0x15,
  EQ:     0x16,
  NEQ:    0x17,
}

/** A mapping from opcodes to their instruction sizes. */
export const Size = {
  NOOP:   1,
  LLOAD:  4,
  LSTORE: 4,
  GLOAD:  3,
  GSTORE: 3,
  INT:    5,
  BOOL:   2,
  FLOAT:  5,
  OBJ:    3,
  JMP:    3,
  BJMP:   3,
  MATCH:  3,
  AP:     2,
  RET:    1,
  ADD:    1,
  SUB:    1,
  MULT:   1,
  DIV:    1,
  LT:     1,
  LTE:    1,
  GT:     1,
  GTE:    1,
  EQ:     1,
  NEQ:    1
}
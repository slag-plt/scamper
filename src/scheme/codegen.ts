import Ops from '../lpm/ops.js'
import { Id, Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

///// State Management /////////////////////////////////////////////////////////

type State = {
  code: Map<Id, { ops: number[], numLocals: number }>
  identifiers: string[]
  objects: Value[]
  curBlk: Id
  curCode: number[]
}

function getIdentifierIndex (state: State, id: string): number {
  const index = state.identifiers.indexOf(id)
  if (index === -1) {
    state.identifiers.push(id)
    return state.identifiers.length - 1
  } else {
    return index
  }
}

function getObjectIndex (state: State, obj: Value): number {
  const index = state.objects.indexOf(obj)
  if (index === -1) {
    state.objects.push(obj)
    return state.objects.length - 1
  } else {
    return index
  }
}

function pushOp (state: State, op: number, arg: number) {
  state.curCode.push(op, arg)
}

///// Code Generation //////////////////////////////////////////////////////////

function lowerSingle (state: State, v: Value) {
  const { value, metadata } = A.unpackSyntax(v)
  v = value
  if (R.isSym(v)) {
    const name = v.value
    // N.B., locals are distinguished by the presence of the 'local' metadata
    // field which denotes their De Brujin index. This metadata is computed
    // in the scope checker.
    if (metadata.has('local')) {
      pushOp(state, Ops.lload, metadata.get('local') as number)
    } else {
      pushOp(state, Ops.gload, getIdentifierIndex(state, name))
    }
  } else if (typeof v === 'number' && v >= 0 && v <= 255) {
    pushOp(state, Ops.int, v)
  } else if (typeof v === 'boolean') {
    pushOp(state, Ops.bool, v ? 1 : 0)
  } else {
    // N.B., all other values are objects stored in the object table
    pushOp(state, Ops.obj, getObjectIndex(state, v))
  }
}

function lowerExpr (state: State, v: Value) {
  if (A.isAtom(v)) {
    lowerSingle(state, v)
  } else if (A.isLambda(v)) {

  } else if (A.isLet(v)) {

  } else if (A.isBegin(v)) {
    const { values, metadata: _metadata } = A.asBegin(v)
    for (const exp of values) {
      lowerExpr(state, exp)
    }

  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata: _metadata } = A.asIf(v)
    lowerExpr(state, guard)
    const ifnbIdx = state.curCode.length
    pushOp(state, Ops.ifnb, -1)
    lowerExpr(state, ifB)
    const jmpIdx = state.curCode.length
    pushOp(state, Ops.jmp, -1)
    lowerExpr(state, elseB)
    // N.B., need to modify the argument fields to point to the code indices
    // _after_ the jumps, keeping in mind that ops are 2 bytes (2 slots) long.
    state.curCode[ifnbIdx + 1] = jmpIdx + 2
    state.curCode[jmpIdx + 1] = state.curCode.length
  } else if (A.isMatch(v)) {
    throw new R.ICE('lowerExpr', 'Match expressions are not yet supported in code generation')
  } else if (A.isQuote(v)) {
    const { value, metadata: _metadata } = A.asQuote(v)
    pushOp(state, Ops.obj, getObjectIndex(state, A.stripAllSyntax(value)))
  } else {
    // N.B., function application
    const { values, metadata: _metadata } = A.asApp(v)
    for (const arg of values) {
      lowerExpr(state, arg)
    }
    pushOp(state, Ops.ap, values.length - 1)
  }
}

function lowerStmt (state: State, v: Value) {
  if (A.isImport(v)) {
    const { name, metadata: _metadata } = A.asImport(v)
    const libname = getIdentifierIndex(state, A.stripSyntax(name) as string)
    // TODO: check to see if libname is a valid library name
    pushOp(state, Ops.loadlib, libname)
  } else if (A.isDefine(v)) {
    const { name, value, metadata: _metadata } = A.asDefine(v)
    lowerExpr(state, value)
    pushOp(state, Ops.gstore, getIdentifierIndex(state, A.stripSyntax(name) as string))
  } else if (A.isDisplay(v)) {
    const { value, metadata: _metadata } = A.asDisplay(v)
    lowerExpr(state, value)
    pushOp(state, Ops.disp, 0)
  } else {
    // N.B., statement-expression case
    lowerExpr(state, v)
  }
}

export function lowerProgram (vs: Value[]): R.Program {
  const state: State = {
    code: new Map<Id, { ops: number[], numLocals: number }>,
    identifiers: [],
    objects: [],
    curBlk: '##main##',
    curCode: [],
  }
  const mainCode: number[] = state.curCode
  state.code.set('##main##', { ops: mainCode, numLocals: 0 })

  for (const v of vs) {
    lowerStmt(state, v)
  }

  return {
    code: new Map<Id, R.Code>([...state.code.entries()].map(
      ([id, data]) => [id, { ops: new Uint8Array(data.ops), numLocals: data.numLocals }]
    )),
    identifiers: state.identifiers,
    objects: state.objects,
  }
}
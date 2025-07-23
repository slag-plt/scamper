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
  const { range, value } = A.unpackSyntax(v)
  v = value
  if (R.isSym(v)) {
    // TODO: need to distinguish between a local with
    // an index and a global with a name
  } else if (typeof v === 'number' && v >= 0 && v <= 255) {
    pushOp(state, Ops.int, v)
  } else if (typeof v === 'boolean') {
    pushOp(state, Ops.bool, v ? 1 : 0)
  } else {
    // N.B., all other values are objects that must be
    // stored on the object table
    pushOp(state, Ops.obj, getObjectIndex(state, v))
  }
}

function lowerExpr (state: State, v: Value) {
  if (A.isAtom(v)) {
    lowerSingle(state, v)
  } else if (A.isLambda(v)) {

  } else if (A.isLet(v)) {

  } else if (A.isLetStar(v)) {

  } else if (A.isAnd(v)) {

  } else if (A.isOr(v)) {

  } else if (A.isBegin(v)) {

  } else if (A.isIf(v)) {

  } else if (A.isCond(v)) {

  } else if (A.isMatch(v)) {

  } else if (A.isQuote(v)) {

  } else if (A.isSection(v)) {

  } else {
    // TODO: function application case!
  }
}

function lowerStmt (state: State, v: Value) {
  const code = state.curBlk
  if (A.isImport(v)) {
    const { name, range } = A.asImport(v)
    const libname = getIdentifierIndex(state, A.stripSyntax(name) as string)
    // TODO: check to see if libname is a valid library name
    pushOp(state, Ops.loadlib, libname)
  } else if (A.isDefine(v)) {
    const { name, value, range } = A.asDefine(v)
    // TODO: are we scope-checking the name?
    lowerExpr(state, value)
    pushOp(state, Ops.gstore, getIdentifierIndex(state, A.stripSyntax(name) as string))
  } else if (A.isDisplay(v)) {
    const { value, range } = A.asDisplay(v)
    lowerExpr(state, value)
    pushOp(state, Ops.disp, 0)
  } else if (A.isStruct(v)) {
    // TODO: steal this from sem.ts, just adds a bunch of
    // JS functions to global scope!
  } else {
    lowerExpr(state, v)
  }
}

/*
export function lowerProgram (vs: Value[]): Program {
  const state: State = {
    code: new Map<Id, { ops: number[], numLocals: number }>,
    identifiers: [],
    objects: [],
  }
  const mainCode: number[] = []
  state.code.set('##main##', { ops: mainCode, numLocals: 0 })

  for (const v of vs) {
    lowerStmt(state, v)
  }

  return {
    code: new Map<Id, Code>([...state.code.entries()].map(
      ([id, data]) => [id, { ops: new Uint8Array(data.ops), numLocals: data.numLocals }]
    )),
    identifiers: state.identifiers,
    objects: state.objects,
  }
}
*/
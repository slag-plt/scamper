import { ScamperError } from '../error.js'
import { Range } from '../range.js'
import { Value } from '../value.js'
import * as V from '../value.js'
import { Op } from '../ops.js'
import * as Ops from '../ops.js'

class LoweringState {
  objects: V.ObjectMap
  patterns: V.PatternMap
  identifiers: V.IdentifierMap
  segments: Map<number, Op[]>
  holeSymCounter: number
  
  constructor() {
    this.objects = []
    this.patterns = []
    this.identifiers = []
    this.segments = new Map()
    this.holeSymCounter = 0
  }

  genHoleSym(): string {
    return `_${this.holeSymCounter++}`
  }

  
}

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function parseBinding (v: Value): { name: string, ops: Op.T[] } {
  let { range, value } = V.unpackSyntax(v)
  v = value
  if (!V.isArray(v)) {
    throw new ScamperError('Parser', 'Binding pair must be given as a vector', undefined, range)
  }
  const vec = v as V.Vector
  if (vec.length !== 2 || !V.isSymbol(V.stripSyntax(vec[0]))) {
    throw new ScamperError('Parser', `Binding must be a pair of a name and value`, undefined, V.rangeOf(vec[0]))
  }
  return { name: V.getSymbolName(V.stripSyntax(vec[0]) as symbol)
         , ops: lower(vec[1]) }
}

function parseMatchBranch (v: Value): Op.MatchBranch {
  let { range, value } = V.unpackSyntax(v)
  v = value
  if (!V.isArray(v)) {
    throw new ScamperError('Parser', 'Match branches must be given as a vector', undefined, range)
  }
  const vec = v as V.Vector
  // TODO: should we be checking to see if the value is a valid pattern here?
  // Or do we defer to runtime at this point...? Probably depends on the
  // syntax of valid patterns and whether the set is small enough to warrant
  // a static check.
  if (vec.length !== 2) {
    throw new ScamperError('Parser', 'Match branches must be given as a pair of a pattern and an expression', undefined, V.rangeOf(vec[0]))
  }
  return { pattern: V.stripAllSyntax(vec[0]), body: lower(vec[1]) }
}

function parseCondBranch (v: Value): { cond: Op.T[], body: Op.T[]} {
  let { range, value } = V.unpackSyntax(v)
  v = value
  if (!V.isArray(v)) {
    throw new ScamperError('Parser', 'Cond branch must be given as a vector', undefined, range)
  }
  const vec = v as V.Vector
  if (vec.length !== 2) {
    throw new ScamperError('Parser', `Cond branch must be a pair of expressions`, undefined, range)
  }
  return { cond: lower(vec[0]), body: lower(vec[1]) }
}

function collectSectionHoles (bvars: string[], v: Value): Value {
  const orig = v
  let { range, value } = V.unpackSyntax(v)
  v = value
  if (V.isSymbol(v) && V.getSymbolName(V.stripSyntax(v) as symbol) === '_') {
    const x = genHoleSym()
    bvars.push(x)
    return V.mkSyntax(range, V.mkSymbol(x))
  } else if (v === null) {
    return orig
  } else if (V.isList(v)) {
    const values = V.listToVector(v as V.List)
    // N.B., do _not_ recursively collect holes in enclosed section forms
    if (V.isSymbol(v) && V.getSymbolName(V.stripSyntax(values[0]) as symbol) === 'section') {
      return orig
    } else {
      return V.mkSyntax(range, V.mkList(...values.map((v) => collectSectionHoles(bvars, v))))
    }
  } else if (V.isPair(v)) {
    return V.mkSyntax(range, V.mkPair(
      collectSectionHoles(bvars, (v as V.Pair).fst),
      collectSectionHoles(bvars, (v as V.Pair).snd)))
  } else if (V.isArray(v)) {
    return V.mkSyntax(range, (v as Value[]).map((v) => collectSectionHoles(bvars, v)))
  } else {
    return orig
  }
}

const specialForms: Map<string, (args: Value[], range: Range) => Op.T[]> = new Map([
  ['lambda', (args, range) => {
    if (args.length !== 2) {
      throw new ScamperError('Parser', 'Lambda expression must have 2 sub-components, an parameter list and a body', undefined, range)
    }
    const { range: esr, value: es } = V.unpackSyntax(args[0])
    if (!V.isList(es)) {
      throw new ScamperError('Parser', 'The first component of a lambda expression must be a parameter list', undefined, esr)
    }
    const params: string[] = []
    V.listToVector(es as V.List).forEach(arg => {
      let { range: r, value: x } = V.unpackSyntax(arg)
      if (!V.isSymbol(x)) {
        throw new ScamperError('Parser', 'Parameters must only be identifiers', undefined, r)
      }
      params.push(V.getSymbolName(x as symbol))
    })
    return [Op.mkCls(params, lower(args[1]))]
  }],
  
  ['let', (args, range) => {
    if (args.length !== 2) {
      throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
    }
    const { range: bsr, value: bs } = V.unpackSyntax(args[0])
    if (!V.isList(bs)) {
      throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
    }
    const bindings = V.listToVector(bs as V.List).map(parseBinding)
    const valOps = bindings.flatMap((b) => b.ops)
    return valOps.concat([Op.mkLet(bindings.map((b) => b.name), lower(args[1]))])    
  }],

  ['let*', (args, range) => {
    if (args.length !== 2) {
      throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
    }
    const { range: bsr, value: bs } = V.unpackSyntax(args[0])
    if (!V.isList(bs)) {
      throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
    }
    const bindings = V.listToVector(bs as V.List)
    let val = V.mkSyntax(range, V.mkList(
      V.mkSymbol('let'),
      V.mkList(bindings[bindings.length - 1]),
      args[1]))
    for (let i = bindings.length-2; i >= 0; i--) {
      val = V.mkSyntax(range, V.mkList(
        V.mkSymbol('let'),
        V.mkList(bindings[i]),
        val))
    } 
    return lower(val)
  }],

  ['and', (args, range) => {
    const label = Op.freshLabel()
    return args
      .flatMap((arg) => lower(arg).concat([Op.mkAnd(label, range)]))
      .concat([Op.mkValue(true), Op.mkLbl(label)]) 
  }],

  ['or', (args, range) => {
    const label = Op.freshLabel()
    return args
      .flatMap((arg) => lower(arg).concat([Op.mkOr(label, range)]))
      .concat([Op.mkValue(false), Op.mkLbl(label)]) 
  }],

  ['if', (args, range) => {
    if (args.length !== 3) {
      throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, range)
    } else {
      return lower(args[0]).concat([
        Op.mkIf(lower(args[1]), lower(args[2]), range)
      ])
    }  
  }],

  ['begin', (args, range) => {
    if (args.length === 0) {
      throw new ScamperError('Parser', 'Begin expression must have at least 1 sub-expression', undefined, range)
    } else {
      return args.flatMap((arg) => lower(arg)).concat([Op.mkSeq(args.length)])
    } 
  }],

  ['match', (args, range) => {
    if (args.length < 2) {
      throw new ScamperError('Parser', 'Match expression must have at least two sub-expressions, a scrutinee at least one branch', undefined, range)
    }
    const scrutinee = args[0]
    const branches = args.slice(1).map(parseMatchBranch)
    return lower(scrutinee).concat([Op.mkMatch(branches, range)])  
  }],

  ['cond', (args, range) => {
    if (args.length < 1) {
      throw new ScamperError('Parser', 'Cond expression must have at least one branch', undefined, range)
    }
    const label = Op.freshLabel()
    const branches = args.map(parseCondBranch)
    return branches
      .flatMap((b) => b.cond.concat([Op.mkCond(b.body, label, range)]))
      .concat([
        Op.mkExn('No branches of "cond" expression matched', undefined, range),
        Op.mkLbl(label)
      ])  
  }],

  ['quote', (args, range) => {
    if (args.length !== 1) {
      throw new ScamperError('Parser', 'Quote expression must have exactly one sub-expression', undefined, range)
    }
    return [Op.mkValue(V.stripAllSyntax(args[0]))] 
  }],

  ['section', (args, range) => {
    if (args.length === 0) {
      throw new ScamperError('Parser', 'Section expression must have at least one sub-expression', undefined, range)
    }
    const params: string[] = []
    const app = V.mkList(...args.map((arg) => collectSectionHoles(params, arg)))
    return lower(V.mkSyntax(range, V.mkList(
      V.mkSymbol('lambda'),
      V.mkList(...params.map((p) => V.mkSymbol(p))),
      app,
    )))
  }]
])

export function lower (v: Value): Op.T[] {
  let { range, value } = V.unpackSyntax(v)
  v = value

  if (V.isSymbol(v)) {
    return [Op.mkVar(V.getSymbolName(v as symbol), range)]
  } else if (!V.isList(v)) {
    return [Op.mkValue(v)]
  } else {
    const values = V.listToVector(v as V.List)
    if (values.length === 0) {
      return [Op.mkValue(null)]
    }
    const head = V.stripSyntax(values[0])
    const args = values.slice(1)
    if (V.isSymbol(head) && specialForms.has(V.getSymbolName(head as symbol))) {
      return specialForms.get(V.getSymbolName(head as symbol))!(args, range)
    } else {
      return values.flatMap(lower).concat([
        Op.mkAp(args.length, range)
      ])
    }
  }
}

export function compile (v: Value): void {
  let { range, value } = V.unpackSyntax(v)
  v = value
}

///// Top-level/program parsing ////////////////////////////////////////////////

export function parseStmt (v: Value.T): Stmt.T {
  let { range, value: uv } = Value.unpackSyntax(v)

  if (!Value.isList(uv)) {
    return Stmt.mkStmtExp(lower(v), v, range)
  } else {
    const values = Value.listToVector(uv as Value.List)
    if (values.length === 0) {
      return Stmt.mkStmtExp([Op.mkValue(null)], v, range)
    }

    const head = values[0]
    const args = values.slice(1)

    if (Value.isSymName(Value.stripSyntax(head), 'define')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, range)
      }
      const { range: r, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
        throw new ScamperError('Parser', 'The first component of a define statement must be an identifier', undefined, r)
      }
      return Stmt.mkStmtBinding((name as Value.Sym).value, lower(args[1]), v, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'import')) {
      if (args.length !== 1) {
        throw new ScamperError('Parser', 'Import statements must have 1 argument, the name of a module', undefined, range)
      }
      const { range: r, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
          throw new ScamperError('Parser', 'The argument of an import statement must be a module name', undefined, r)
      }
      return Stmt.mkImport((name as Value.Sym).value, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'display')) {
      if (args.length !== 1) {
        throw new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, range)
      }
      return Stmt.mkDisplay(lower(args[0]), v, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'struct')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, range)
      } 
      const { range: nr, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
        throw new ScamperError('Parser', 'The first argument of a struct statement must be a struct name', undefined, nr )
      }
      const { range: sfr, value: sfields } = Value.unpackSyntax(args[1])
      if (!Value.isList(sfields)) {
        throw new ScamperError('Parser', 'The second argument of a struct statement must be a list of fields', undefined, range)
      }
      const fields: string[] = []
      Value.listToVector(sfields as Value.List).forEach((fld) => {
        const { range: r, value: f } = Value.unpackSyntax(fld)
        if (!Value.isSym(f)) {
          throw new ScamperError('Parser', 'Struct fields must be identifiers', undefined, r)
        }
        fields.push((f as Value.Sym).value)
      })
      return Stmt.mkStruct((name as Value.Sym).value, fields, range)
    } else {
      return Stmt.mkStmtExp(lower(v), v, range)
    }
  }
}
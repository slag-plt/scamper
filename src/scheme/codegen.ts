// import * as R from '../lpm/runtime.js'
// import Ops from '../lpm/ops.js'

// let holeSymCounter = 0
// function genHoleSym(): string {
//   return `_${holeSymCounter++}`
// }

// function collectSectionHoles (bvars: string[], v: Value.T): Value.T {
//   const orig = v
//   let { range, value } = Value.unpackSyntax(v)
//   v = value
//   if (Value.isSymName(Value.stripSyntax(v), '_')) {
//     const x = genHoleSym()
//     bvars.push(x)
//     return Value.mkSyntax(range, Value.mkSym(x))
//   } else if (v === null) {
//     return orig
//   } else if (Value.isList(v)) {
//     const values = Value.listToVector(v as Value.List)
//     // N.B., do _not_ recursively collect holes in enclosed section forms
//     if (Value.isSymName(Value.stripSyntax(values[0]), 'section')) {
//       return orig
//     } else {
//       return Value.mkSyntax(range, Value.mkList(...values.map((v) => collectSectionHoles(bvars, v))))
//     }
//   } else if (Value.isPair(v)) {
//     return Value.mkSyntax(range, Value.mkPair(
//       collectSectionHoles(bvars, (v as Value.Pair).fst),
//       collectSectionHoles(bvars, (v as Value.Pair).snd)))
//   } else if (Value.isArray(v)) {
//     return Value.mkSyntax(range, (v as Value.T[]).map((v) => collectSectionHoles(bvars, v)))
//   } else {
//     return orig
//   }
// }

// function lowerBinding (v: Value.T): { name: string, ops: Op.T[] } {
//   let { range, value } = Value.unpackSyntax(v)
//   v = value
//   if (!Value.isArray(v)) {
//     throw new ScamperError('Parser', 'Binding pair must be given as a vector', undefined, range)
//   }
//   const vec = v as Value.Vector
//   if (vec.length !== 2 || !Value.isSym(Value.stripSyntax(vec[0]))) {
//     throw new ScamperError('Parser', `Binding must be a pair of a name and value`, undefined, Value.rangeOf(vec[0]))
//   }
//   return { name: (Value.stripSyntax(vec[0]) as Value.Sym).value
//          , ops: lower(vec[1]) }
// }

// function parseMatchBranch (v: Value.T): Op.MatchBranch {
//   let { range, value } = Value.unpackSyntax(v)
//   v = value
//   if (!Value.isArray(v)) {
//     throw new ScamperError('Parser', 'Match branches must be given as a vector', undefined, range)
//   }
//   const vec = v as Value.Vector
//   // TODO: should we be checking to see if the value is a valid pattern here?
//   // Or do we defer to runtime at this point...? Probably depends on the
//   // syntax of valid patterns and whether the set is small enough to warrant
//   // a static check.
//   if (vec.length !== 2) {
//     throw new ScamperError('Parser', 'Match branches must be given as a pair of a pattern and an expression', undefined, Value.rangeOf(vec[0]))
//   }
//   return { pattern: Value.stripAllSyntax(vec[0]), body: lower(vec[1]) }
// }

// function parseCondBranch (v: Value.T): { cond: Op.T[], body: Op.T[]} {
//   let { range, value } = Value.unpackSyntax(v)
//   v = value
//   if (!Value.isArray(v)) {
//     throw new ScamperError('Parser', 'Cond branch must be given as a vector', undefined, range)
//   }
//   const vec = v as Value.Vector
//   if (vec.length !== 2) {
//     throw new ScamperError('Parser', `Cond branch must be a pair of expressions`, undefined, range)
//   }
//   return { cond: lower(vec[0]), body: lower(vec[1]) }
// }


// const specialForms: Map<string, (args: R.Value[], vars: Map<string, number>, ops: number[], state: State, range: R.Range) => void> = new Map([
//   ['lambda', (args, range) => {
//     // TODO: the way we compile a lambda:
//     // 1. Scope check to gather up variable -> index mapping
//     // 2. Create a new entry in the program for the lambda's body (with a fresh label)
//     // 3. Recursively lower the body
//     // 4. Return a closure that points to this lambda's body
//     // (Will there only ever be one pointer to each lambda body, i.e., can two closures point to the same body?)

//     // if (args.length !== 2) {
//     //   throw new ScamperError('Parser', 'Lambda expression must have 2 sub-components, an parameter list and a body', undefined, range)
//     // }
//     // const { range: esr, value: es } = Value.unpackSyntax(args[0])
//     // if (!Value.isList(es)) {
//     //   throw new ScamperError('Parser', 'The first component of a lambda expression must be a parameter list', undefined, esr)
//     // }
//     // const params: string[] = []
//     // Value.listToVector(es as Value.List).forEach(arg => {
//     //   let { range: r, value: x } = Value.unpackSyntax(arg)
//     //   if (!Value.isSym(x)) {
//     //     throw new ScamperError('Parser', 'Parameters must only be identifiers', undefined, r)
//     //   }
//     //   params.push((x as Value.Sym).value)
//     // })
//     // return [Op.mkCls(params, lower(args[1]))]
//   }],
  
//   ['let', (args, range) => {
//     if (args.length !== 2) {
//       throw new R.ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
//     }
//     const { range: bsr, value: bs } = R.unpackSyntax(args[0])
//     if (!R.isList(bs)) {
//       throw new R.ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
//     }
//     R.listToVector(bs as R.List).forEach((b) => {
//       lowerBinding(b)
//       pushOp(Ops.lload, vars.get((b.name)))
//     }
//     // const bindings = R.listToVector(bs as R.List).map(lowerBinding)
//     const valOps = bindings.flatMap((b) => b.ops)
//     return valOps.concat([Op.mkLet(bindings.map((b) => b.name), lower(args[1]))])    
//   }],

//   ['let*', (args, range) => {
//     if (args.length !== 2) {
//       throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
//     }
//     const { range: bsr, value: bs } = Value.unpackSyntax(args[0])
//     if (!Value.isList(bs)) {
//       throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
//     }
//     const bindings = Value.listToVector(bs as Value.List)
//     let val = Value.mkSyntax(range, Value.mkList(
//       Value.mkSym('let'),
//       Value.mkList(bindings[bindings.length - 1]),
//       args[1]))
//     for (let i = bindings.length-2; i >= 0; i--) {
//       val = Value.mkSyntax(range, Value.mkList(
//         Value.mkSym('let'),
//         Value.mkList(bindings[i]),
//         val))
//     } 
//     return lower(val)
//   }],

//   ['and', (args, range) => {
//     const label = Op.freshLabel()
//     return args
//       .flatMap((arg) => lower(arg).concat([Op.mkAnd(label, range)]))
//       .concat([Op.mkValue(true), Op.mkLbl(label)]) 
//   }],

//   ['or', (args, range) => {
//     const label = Op.freshLabel()
//     return args
//       .flatMap((arg) => lower(arg).concat([Op.mkOr(label, range)]))
//       .concat([Op.mkValue(false), Op.mkLbl(label)]) 
//   }],

//   ['if', (args, range) => {
//     if (args.length !== 3) {
//       throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, range)
//     } else {
//       return lower(args[0]).concat([
//         Op.mkIf(lower(args[1]), lower(args[2]), range)
//       ])
//     }  
//   }],

//   ['begin', (args, range) => {
//     if (args.length === 0) {
//       throw new ScamperError('Parser', 'Begin expression must have at least 1 sub-expression', undefined, range)
//     } else {
//       return args.flatMap((arg) => lower(arg)).concat([Op.mkSeq(args.length)])
//     } 
//   }],

//   ['match', (args, range) => {
//     if (args.length < 2) {
//       throw new ScamperError('Parser', 'Match expression must have at least two sub-expressions, a scrutinee at least one branch', undefined, range)
//     }
//     const scrutinee = args[0]
//     const branches = args.slice(1).map(parseMatchBranch)
//     return lower(scrutinee).concat([Op.mkMatch(branches, range)])  
//   }],

//   ['cond', (args, range) => {
//     if (args.length < 1) {
//       throw new ScamperError('Parser', 'Cond expression must have at least one branch', undefined, range)
//     }
//     const label = Op.freshLabel()
//     const branches = args.map(parseCondBranch)
//     return branches
//       .flatMap((b) => b.cond.concat([Op.mkCond(b.body, label, range)]))
//       .concat([
//         Op.mkExn('No branches of "cond" expression matched', undefined, range),
//         Op.mkLbl(label)
//       ])  
//   }],

//   ['quote', (args, range) => {
//     if (args.length !== 1) {
//       throw new ScamperError('Parser', 'Quote expression must have exactly one sub-expression', undefined, range)
//     }
//     return [Op.mkValue(Value.stripAllSyntax(args[0]))] 
//   }],

//   ['section', (args, range) => {
//     if (args.length === 0) {
//       throw new ScamperError('Parser', 'Section expression must have at least one sub-expression', undefined, range)
//     }
//     const params: string[] = []
//     const app = Value.mkList(...args.map((arg) => collectSectionHoles(params, arg)))
//     return lower(Value.mkSyntax(range, Value.mkList(
//       Value.mkSym('lambda'),
//       Value.mkList(...params.map((p) => Value.mkSym(p))),
//       app,
//     )))
//   }]
// ])
// }

// class State {
//   code: Map<string, number>
//   identifiers: Map<string, number>
//   objects: Map<R.Value, number>

//   constructor () {
//     this.code = new Map()
//     this.identifiers = new Map()
//     this.objects = new Map()
//   }

//   getIdentifier (id: string): number {
//     if (!this.identifiers.has(id)) {
//       this.identifiers.set(id, this.identifiers.size)
//     }
//     return this.identifiers.get(id)!
//   }

//   getObject (obj: R.Value): number {
//     if (!this.objects.has(obj)) {
//       this.objects.set(obj, this.objects.size)
//     }
//     return this.objects.get(obj)!
//   }
// }

// function pushOp (op: number, arg: number, ops: number[]) {
//   ops.push(op)
//   ops.push(arg)
// }

// export function lowerValue (v: R.Value, ops: number[], state: State) {
//   if (typeof v === 'number') {
//     if (0 <= v && v <= 255) {
//       pushOp(Ops.int, v, ops)
//     } else {
//       const i = state.getObject(v)
//       pushOp(Ops.obj, i, ops)
//     }
//   } else if (typeof v === 'boolean') {
//     pushOp(Ops.bool, v ? 1 : 0, ops)
//   } else if (typeof v === 'string') {
//     const i = state.getObject(v)
//     pushOp(Ops.str, i, ops)
//   } else {
//     throw new R.ICE('lowerValue', `Unexpected value type: ${typeof v}`)
//   }
// }

// export function lowerExp (v: R.Value, locals: Map<string, number>, ops: number[], state: State) {
//   let { range, value } = R.unpackSyntax(v)
//   v = value

//   if (R.isSym(v)) {
//     pushOp(Ops.lload, locals.get((v as R.Sym).value)!, ops)
//   } else if (!R.isList(v)) {
//     lowerValue(v, ops, state)

//   } else {
//     const values = R.listToVector(v as R.List)
//     if (values.length === 0) {
//       const i = state.getObject(null)
//       pushOp(Ops.obj, i, ops)
//     }
//     const head = R.stripSyntax(values[0])
//     const args = values.slice(1)
//     if (R.isSym(head) && specialForms.has((head as R.Sym).value)) {
//       return specialForms.get((head as R.Sym).value)!(args, R.Range.none)
//     } else {
//       return values.flatMap(lowerExp).concat([
//         Op.mkAp(args.length, range)
//       ])
//     }
//   }

// export function parseStmt (v: Value): Stmt.T {
//   let { range, value: uv } = R.unpackSyntax(v)

//   if (!R.isList(uv)) {
//     return Stmt.mkStmtExp(lowerExp(v), v, range)
//   } else {
//     const values = R.listToVector(uv as R.List)
//     if (values.length === 0) {
//       return Stmt.mkStmtExp([Op.mkValue(null)], v, range)
//     }

//     const head = values[0]
//     const args = values.slice(1)

//     if (R.isSymName(R.stripSyntax(head), 'define')) {
//       if (args.length !== 2) {
//         throw new ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, range)
//       }
//       const { range: r, value: name } = R.unpackSyntax(args[0])
//       if (!R.isSym(name)) {
//         throw new ScamperError('Parser', 'The first component of a define statement must be an identifier', undefined, r)
//       }
//       return Stmt.mkStmtBinding((name as R.Sym).value, lowerExp(args[1]), v, range)

//     } else if (R.isSymName(R.stripSyntax(head), 'import')) {
//       if (args.length !== 1) {
//         throw new ScamperError('Parser', 'Import statements must have 1 argument, the name of a module', undefined, range)
//       }
//       const { range: r, value: name } = R.unpackSyntax(args[0])
//       if (!R.isSym(name)) {
//           throw new ScamperError('Parser', 'The argument of an import statement must be a module name', undefined, r)
//       }
//       return Stmt.mkImport((name as R.Sym).value, range)

//     } else if (R.isSymName(R.stripSyntax(head), 'display')) {
//       if (args.length !== 1) {
//         throw new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, range)
//       }
//       return Stmt.mkDisplay(lowerExp(args[0]), v, range)

//     } else if (R.isSymName(R.stripSyntax(head), 'struct')) {
//       if (args.length !== 2) {
//         throw new ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, range)
//       } 
//       const { range: nr, value: name } = R.unpackSyntax(args[0])
//       if (!R.isSym(name)) {
//         throw new ScamperError('Parser', 'The first argument of a struct statement must be a struct name', undefined, nr )
//       }
//       const { range: _sfr, value: sfields } = R.unpackSyntax(args[1])
//       if (!R.isList(sfields)) {
//         throw new ScamperError('Parser', 'The second argument of a struct statement must be a list of fields', undefined, range)
//       }
//       const fields: string[] = []
//       R.listToVector(sfields as R.List).forEach((fld) => {
//         const { range: r, value: f } = R.unpackSyntax(fld)
//         if (!R.isSym(f)) {
//           throw new ScamperError('Parser', 'Struct fields must be identifiers', undefined, r)
//         }
//         fields.push((f as R.Sym).value)
//       })
//       return Stmt.mkStruct((name as R.Sym).value, fields, range)
//     } else {
//       return Stmt.mkStmtExp(lowerExp(v), v, range)
//     }
//   }
// }
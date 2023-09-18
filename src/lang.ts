export class Loc {
  line: number
  col: number
  idx: number

  constructor (line: number, col: number, idx: number) {
    this.line = line
    this.col = col
    this.idx = idx
  }

  public toString (): string {
    return `${this.line}:${this.col}`
  }
}

export class Range {
  begin: Loc
  end: Loc

  constructor (startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number) {
    this.begin = new Loc(startLine, startCol, startIdx)
    this.end = new Loc(endLine, endCol, endIdx)
  }

  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }
}

export const mkRange = (beg: Loc, end: Loc): Range => new Range(beg.line, beg.col, beg.idx, end.line, end.col, end.idx)

export const noLoc = new Loc(-1, -1, -1)
export const noRange = new Range(-1, -1, -1, -1, -1, -1)

type Phase = 'Parser' | 'Runtime'

export class ScamperError extends Error {
  phase: Phase
  modName?: string
  range?: Range
  source?: string

  constructor (phase: Phase, msg: string, modName?: string, range?: Range, source?: string) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
    this.source = source
  }

  toString(): string {
    const detail = `${this.modName ? this.modName : ''}${this.range ? this.range.toString() : ''}`
    const src = this.source ? `(${this.source}) ` : ''
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${src}${this.message}`
  }
}

export class ICE extends Error {
  funcName: string

  constructor (funcName: string, msg: string) {
    super(msg)
    this.funcName = funcName
  }

  toString(): string {
    return `ICE (${this.funcName}): ${this.message}\n${this.stack}`
  }
}

export type Id = string
export type Bracket = '(' | '[' | '{'

const specialCharToNameMap: Map<string, string> = new Map([
  [String.fromCharCode(9), 'alarm'],
  [String.fromCharCode(7), 'backspace'],
  [String.fromCharCode(126), 'delete'],
  [String.fromCharCode(26), 'escape'],
  [String.fromCharCode(9), 'newline'],
  [String.fromCharCode(-1), 'null'],
  [String.fromCharCode(12), 'return'],
  [' ', 'space'],
  [String.fromCharCode(8), 'tab']
])

export function charToName (c: string): string {
  if (specialCharToNameMap.has(c)) {
    return specialCharToNameMap.get(c)!
  } else {
    return c
  }
}

export namespace Sexp {
  export type T = Atom | List
  export type Atom = { _scamperTag: 'struct', kind: 'atom', value: string, range: Range }
  export type List = { _scamperTag: 'struct', kind: 'list', bracket: Bracket, value: T[], range: Range }

  export const mkAtom = (value: string, range: Range): Atom =>
    ({ _scamperTag: 'struct', kind: 'atom', value, range })
  export const mkList = (value: T[], bracket: Bracket, range: Range): List =>
    ({ _scamperTag: 'struct', kind: 'list', bracket, value, range }) 

  function surround (s: string, bracket: Bracket): string {
    switch (bracket) {
      case '(':
        return `(${s})`
      case '[':
        return `[${s}]`
      case '{':
        return `{${s}}`
    }
  }

  export function sexpToString (s: T): string {
    switch (s.kind) {
      case 'atom':
        return s.value
      case 'list':
        return `${surround(s.value.map(sexpToString).join(' '), s.bracket)}`
    }
  }
}

export namespace Pat {
  export type MatchBranch = { pattern: T, body: Exp.T }

  export type T = Var | Wild | Num | Bool | Char | Str | Null | Ctor
  export type Var = { _scamperTag: 'struct', kind: 'var', name: string, range: Range }
  export type Wild = { _scamperTag: 'struct', kind: 'wild', range: Range }
  export type Num = { _scamperTag: 'struct', kind: 'num', value: number, range: Range }
  export type Bool = { _scamperTag: 'struct', kind: 'bool', value: boolean, range: Range }
  export type Char = { _scamperTag: 'struct', kind: 'char', value: string, range: Range }
  export type Str = { _scamperTag: 'struct', kind: 'str', value: string, range: Range }
  export type Null = { _scamperTag: 'struct', kind: 'null', range: Range }
  export type Ctor = { _scamperTag: 'struct', kind: 'ctor', ctor: string, args: T[], range: Range }

  export const mkVar = (name: string, range: Range): T => ({ _scamperTag: 'struct', kind: 'var', name, range })
  export const mkWild = (range: Range): T => ({ _scamperTag: 'struct', kind: 'wild', range })
  export const mkNum = (value: number, range: Range): T => ({ _scamperTag: 'struct', kind: 'num', value, range })
  export const mkBool = (value: boolean, range: Range): T => ({ _scamperTag: 'struct', kind: 'bool', value, range })
  export const mkChar = (value: string, range: Range): T => ({ _scamperTag: 'struct', kind: 'char', value, range })
  export const mkStr = (value: string, range: Range): T => ({ _scamperTag: 'struct', kind: 'str', value, range })
  export const mkNull = (range: Range): T => ({ _scamperTag: 'struct', kind: 'null', range })
  export const mkCtor = (ctor: string, args: T[], range: Range): T => ({ _scamperTag: 'struct', kind: 'ctor', ctor, args, range })

  export function patToSexp (p: T): Sexp.T {
    switch (p.kind) {
      case 'var':
        return Sexp.mkAtom(p.name, p.range)
      case 'wild':
        return Sexp.mkAtom('_', p.range)
      case 'num':
        return Sexp.mkAtom(p.value.toString(), p.range)
      case 'bool':
        return Sexp.mkAtom(p.value ? "#t" : "#f", p.range)
      case 'char':
        return Sexp.mkAtom(`#\\${charToName(p.value)}`, p.range)
      case 'str':
        return Sexp.mkAtom(`"${p.value}"`, p.range)
      case 'null':
        return Sexp.mkAtom('null', p.range)
      case 'ctor':
        return Sexp.mkList([Sexp.mkAtom(p.ctor, noRange), ...p.args.map(patToSexp)], '(', p.range)
    }
  }

  export function patToString (p: T): string {
    return Sexp.sexpToString(patToSexp(p))
  }
}

export namespace Exp {
  export type T = Var | Val | Lam | Let | App | And | Or | If | Begin | Match | Cond
  
  export type Binding = { name: Id, body: T }
  export type CondBranch = { guard: T, body: T }
  export type MatchBranch = { pattern: Pat.T, body: T }

  export type Var = { _scamperTag: 'struct', kind: 'var', name: string, range: Range }
  export type Val = { _scamperTag: 'struct', kind: 'val', value: Value.T, range: Range } 
  export type Lam = { _scamperTag: 'struct', kind: 'lam', args: Id[], body: T, bracket: Bracket, range: Range }
  export type Let = { _scamperTag: 'struct', kind: 'let', bindings: Binding[], body: T, bracket: Bracket, range: Range }
  export type App = { _scamperTag: 'struct', kind: 'app', head: T, args: T[], bracket: Bracket, range: Range }
  export type And = { _scamperTag: 'struct', kind: 'and', exps: T[], bracket: Bracket, range: Range }
  export type Or = { _scamperTag: 'struct', kind: 'or', exps: T[], bracket: Bracket, range: Range }
  export type If = { _scamperTag: 'struct', kind: 'if', guard: T, ifb: T, elseb: T, bracket: Bracket, range: Range }
  export type Begin = { _scamperTag: 'struct', kind: 'begin', exps: T[], bracket: Bracket, range: Range }
  export type Match = { _scamperTag: 'struct', kind: 'match', scrutinee: T, branches: MatchBranch[], bracket: Bracket, range: Range }
  export type Cond = { _scamperTag: 'struct', kind: 'cond', branches: CondBranch[], range: Range }

  export const mkVar = (name: string, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'var', name, range })
  export const mkVal = (value: Value.T, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'val', value, range })
  export const mkLam = (args: Id[], body: T, bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'lam', args, body, bracket, range })
  export const mkLet = (bindings: Binding[], body: T, bracket: Bracket, range: Range): T => 
    ({ _scamperTag: 'struct', kind: 'let', bindings, body, bracket, range })
  export const mkApp = (head: T, args: T[], bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'app', head, args, bracket, range })
  export const mkAnd = (exps: T[], bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'and', exps, bracket, range })
  export const mkOr = (exps: T[], bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'or', exps, bracket, range })
  export const mkIf = (guard: T, ifb: T, elseb: T, bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'if', guard, ifb, elseb, bracket, range })
  export const mkBegin = (exps: T[], bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'begin', exps, bracket, range })
  export const mkMatch = (scrutinee: T, branches: MatchBranch[], bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'match', scrutinee, branches, bracket, range })
  export const mkCond = (branches: CondBranch[], range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'cond', branches, range })

  export function expToSexp(e: T): Sexp.T {
    switch (e.kind) {
      case 'var':
        return Sexp.mkAtom(e.name, e.range)
      case 'val':
        return Value.toSexp(e.value, e.range)
      case 'lam':
        return Sexp.mkList([
          Sexp.mkAtom('lambda', noRange),
          Sexp.mkList(e.args.map((arg) => Sexp.mkAtom(arg, noRange)), '(', noRange), expToSexp(e.body)],
          e.bracket, e.range)
      case 'let':
        return Sexp.mkList([
          Sexp.mkAtom('let', noRange),
          Sexp.mkList(e.bindings.map(
            b => Sexp.mkList([Sexp.mkAtom(b.name, noRange), expToSexp(b.body)], '[', noRange)),
            '(', noRange),
          expToSexp(e.body)], e.bracket, e.range)
      case 'app':
        return Sexp.mkList([expToSexp(e.head), ...e.args.map(expToSexp)], '(', e.range)
      case 'and':
        return Sexp.mkList([Sexp.mkAtom('and', noRange), ...e.exps.map(expToSexp)], e.bracket, e.range)
      case 'or':
        return Sexp.mkList([Sexp.mkAtom('or', noRange), ...e.exps.map(expToSexp)], e.bracket, e.range)
      case 'if':
        return Sexp.mkList([Sexp.mkAtom('if', noRange), expToSexp(e.guard), expToSexp(e.ifb), expToSexp(e.elseb)], '(', e.range)
      case 'begin':
        return Sexp.mkList([Sexp.mkAtom('begin', noRange), ...e.exps.map(expToSexp)], '(', e.range)
      case 'match':
        return Sexp.mkList([
          Sexp.mkAtom('match', noRange),
          expToSexp(e.scrutinee),
          Sexp.mkList(e.branches.map((b) => Sexp.mkList([Pat.patToSexp(b.pattern), expToSexp(b.body)], '[', noRange)), '(', noRange)],
          e.bracket, e.range)
      case 'cond':
        return Sexp.mkList([
          Sexp.mkAtom('cond', noRange),
          ...e.branches.map((b) => Sexp.mkList([expToSexp(b.guard), expToSexp(b.body)], '[', noRange))],
          '(', e.range)
    }
  }

  export function expToString(e: T): string {
    return Sexp.sexpToString(expToSexp(e))
  }
}

export namespace Stmt {
  export type T = StmtBinding | StmtExp | Import | Display | Struct
  export type StmtBinding = { _scamperTag: 'struct', kind: 'binding', name: Id, body: Exp.T, bracket: Bracket, range: Range }
  export type StmtExp = { _scamperTag: 'struct', kind: 'stmtexp', body: Exp.T }
  export type Import = { _scamperTag: 'struct', kind: 'import', modName: string, bracket: Bracket, range: Range }
  export type Display = { _scamperTag: 'struct', kind: 'display', body: Exp.T, bracket: Bracket, range: Range }
  export type Struct = { _scamperTag: 'struct', kind: 'struct', id: string, fields: string[], bracket: Bracket, range: Range }

  export const mkStmtBinding = (name: Id, body: Exp.T, bracket: Bracket, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'binding', name, body, bracket, range })
  export const mkStmtExp = (body: Exp.T): T => ({ _scamperTag: 'struct', kind: 'stmtexp', body })
  export const mkImport = (modName: string, bracket: Bracket, range: Range): T => ({ _scamperTag: 'struct', kind: 'import', modName, bracket, range })
  export const mkDisplay = (body: Exp.T, bracket: Bracket, range: Range): T => ({ _scamperTag: 'struct', kind: 'display', body, bracket, range })
  export const mkStruct = (id: string, fields: string[], bracket: Bracket, range: Range): T => ({ _scamperTag: 'struct', kind: 'struct', id, fields, bracket, range })

  export function stmtToSexp(s: T): Sexp.T {
    switch (s.kind) {
      case 'binding':
        return Sexp.mkList([Sexp.mkAtom('define', noRange), Sexp.mkAtom(s.name, noRange), Exp.expToSexp(s.body)], s.bracket, s.range)
      case 'stmtexp':
        return Exp.expToSexp(s.body)
      case 'import':
        return Sexp.mkList([Sexp.mkAtom('import', noRange), Sexp.mkAtom(s.modName, noRange)], s.bracket, s.range)
      case 'display':
        return Sexp.mkList([Sexp.mkAtom('define', noRange), Exp.expToSexp(s.body)], s.bracket, s.range)
      case 'struct':
        return Sexp.mkList([Sexp.mkAtom('struct', noRange), Sexp.mkAtom(s.id, noRange), ...s.fields.map((f) => Sexp.mkAtom(f, noRange))], '(', s.range)
    }
  }

  export function stmtToString(s: T): string {
    return Sexp.sexpToString(stmtToSexp(s))
  }
}

export type Prog = Stmt.T[]

export function progToSexps(p: Prog): Sexp.T[] {
  return p.map(Stmt.stmtToSexp)
}

export function progToString(p: Prog, sep: string = '\n\n'): string {
  return progToSexps(p).map(Sexp.sexpToString).join(sep)
}

export namespace Op {
  export type Label = string
  export const freshLabel: () => Label = (() => {
    let counter = 0
    return () => `lbl_${counter++}`
  })()

  export type MatchBranch = { pattern: Pat.T, body: T[] }

  export type T    = Var | Val | Cls | Ap | If | Let | Seq | Match | And | Or | Cond | Lbl | Exn
  export type Var   = { tag: 'var', name: string, range: Range }
  export type Val   = { tag: 'val', value: Value.T }
  export type Cls   = { tag: 'cls', params: Id[], ops: T[] }
  export type Ap    = { tag: 'ap', arity: number, range: Range }
  export type If    = { tag: 'if', ifb: T[], elseb: T[], range: Range }
  export type Let   = { tag: 'let', names: Id[] }
  export type Seq   = { tag: 'seq', numSubexps: number }
  export type Match = { tag: 'match', branches: MatchBranch[], range: Range }
  export type And   = { tag: 'and', jmpTo: Label, range: Range }
  export type Or    = { tag: 'or', jmpTo: Label, range: Range }
  export type Cond  = { tag: 'cond', body: T[], end: Label, range: Range }
  export type Lbl   = { tag: 'lbl', name: string }
  export type Exn   = { tag: 'exn', msg: string, modName?: string, range?: Range, source?: string }

  export const mkVar = (name: string, range: Range): T => ({ tag: 'var', name, range })
  export const mkValue = (value: Value.T): T => ({ tag: 'val', value })
  export const mkCls = (params: Id[], ops: T[]): T => ({ tag: 'cls', params, ops })
  export const mkAp = (arity: number, range: Range): T => ({ tag: 'ap', arity, range })
  export const mkIf = (ifb: T[], elseb: T[], range: Range): T => ({ tag: 'if', ifb, elseb, range })
  export const mkLet = (names: Id[]): T => ({ tag: 'let', names })
  export const mkSeq = (numSubexps: number): T => ({ tag: 'seq', numSubexps })
  export const mkMatch = (branches: MatchBranch[], range: Range): T => ({ tag: 'match', branches, range })
  export const mkAnd = (jmpTo: Label, range: Range): T => ({ tag: 'and', jmpTo, range })
  export const mkOr = (jmpTo: Label, range: Range): T => ({ tag: 'or', jmpTo, range })
  export const mkCond = (body: T[], end: Label, range: Range): T => ({ tag: 'cond', body, end, range })
  export const mkLbl = (name: string): T => ({ tag: 'lbl', name })
  export const mkExn = (msg: string, modName?: string, range?: Range, source?: string): T => ({ tag: 'exn', msg, modName, range, source })

  export function opToString (op: T): string {
    switch (op.tag) {
      case 'var':
        return `var ${op.name}`
      case 'val':
        return `val ${Value.valueToString(op.value)}`
      case 'cls':
        return `cls (${op.params.join(' ')})`
      case 'ap':
        return `ap ${op.arity}`
      case 'if':
        return `if (${op.ifb.map(opToString).join('; ')}) else (${op.elseb.map(opToString).join('; ')}))`
      case 'let':
        return `let (${op.names.join(' ')})`
      case 'seq':
        return `seq ${op.numSubexps}`
      case 'match':
        return `match (${op.branches.map((b) => Pat.patToString(b.pattern) + ' => ' + b.body.map(opToString).join('; ')).join(' | ')})`
      case 'and':
        return `and (jmpto ${op.jmpTo})`
      case 'or':
        return `or (jmpto ${op.jmpTo})`
      case 'cond':
        return `cond (${op.end}, ${op.body.map(opToString).join('; ')})`
      case 'lbl':
        return `lbl ${op.name}`
      case 'exn':
        return `exn "${op.msg}"`
    }
  }
}

export namespace Value {
  export type TaggedObject = Closure | Char | Pair | Struct
  export type Closure = { _scamperTag: 'closure', params: Id[], ops: Op.T[], env: Env, name?: string }
  export type Char = { _scamperTag: 'char', value: string }
  export type Pair = { _scamperTag: 'pair', fst: T, snd: T, isList: boolean }

  // NOTE: to maximize interoperability, a struct is an object with at least
  // a _scamperTag and kind field. The rest of the fields are the fields of the
  // the struct, all required to be string keys (i.e., no index or symbol keys).
  // The order of arguments of a struct's constructor is thus property order of
  // the corresponding object, i.e., the order in which the fields are defined.
  export interface Struct { _scamperTag: 'struct', kind: string, [key: string]: any }

  export type List = null | Pair
  export type T = boolean | number | string | List | undefined | T[] | TaggedObject | Function | Object

  export const isNumber = (v: T): boolean => typeof v === 'number'
  export const isBoolean = (v: T): boolean => typeof v === 'boolean'
  export const isString = (v: T): boolean => typeof v === 'string'
  export const isNull = (v: T): boolean => v === null
  export const isVoid = (v: T): boolean => v === undefined
  export const isArray = (v: T): boolean => Array.isArray(v)
  export const isClosure = (v: T): boolean => v !== null && typeof v === 'object' && (v as any)._scamperTag === 'closure'
  export const isJsFunction = (v: T): boolean => typeof v === 'function'
  export const isFunction = (v: T): boolean => isJsFunction(v) || isClosure(v)
  export const isChar = (v: T): boolean => v !== null && typeof v === 'object' && (v as any)._scamperTag === 'char'
  export const isPair = (v: T): boolean => v !== null && typeof v === 'object' && (v as any)._scamperTag === 'pair'
  export const isList = (v: T): boolean => v === null || (isPair(v) && (v as Pair).isList)
  export const isStruct = (v: T): boolean => v !== null && typeof v === 'object' && (v as any)._scamperTag === 'struct'
  export const isStructKind = (v: T, k: string): boolean => isStruct(v) && (v as Struct).kind === k

  export const mkClosure = (arity: number, params: Id[], ops: Op.T[], env: Env): T =>
    ({ _scamperTag: 'closure', arity, params, ops, env })
  export const mkChar = (v: string): Char => ({ _scamperTag: 'char', value: v })
  export const mkPair = (fst: T, snd: T): Pair => ({
    _scamperTag: 'pair', fst, snd,
    isList: snd === null || ((isPair(snd) && (snd as Pair).isList))
  })
  export const mkStruct = (kind: string, fields: string[], values: T[]): T => {
    const ret: Struct = { _scamperTag: 'struct', kind }
    for (let i = 0; i < fields.length; i++) {
      ret[fields[i]] = values[i]
    }
    return ret
  }

  export const nameFn = (name: string, fn: Function): Function =>
    Object.defineProperty(fn, 'name', { value: name })

  export function listToArray (l: List): T[] {
    const ret = []
    let cur = l
    while (cur !== null) {
      ret.push(cur.fst)
      cur = cur.snd as Pair
    }
    return ret
  }

  export function arrayToList (arr: T[]): Pair | null {
    let ret = null
    for (let i = arr.length - 1; i >= 0; i--) {
      ret = mkPair(arr[i], ret)
    }
    return ret
  }

  export function valueToString (v: T) {
    if (isClosure(v)) {
      return `<closure (${(v as Closure).params.join(' ')})>`
    } else if (typeof v === 'function') {
      return `<jsfunc: (${v.name})>`
    } else {
      return `${v}`
    }
  }

  export function getFieldsOfStruct (s: Struct): string[] {
    const ret = []
    for (const f in s) {
      if (f !== '_scamperTag' && f !== 'kind') {
        ret.push(f)
      }
    }
    return ret
  }

  export function toSexp (v: T, range: Range): Sexp.T {
    switch (typeof v) {
      case 'number':
        return Sexp.mkAtom(v.toString(), range)
      case 'boolean':
        return Sexp.mkAtom(v ? "#t" : "#f", range)
      case 'string':
        return Sexp.mkAtom(`#\\${charToName(v)}`, range)
      case 'undefined':
        return Sexp.mkAtom('void', range)
      default:
        if (v === null) {
          return Sexp.mkAtom('null', range)
        } else if (Array.isArray(v)) {
          Sexp.mkList([
            Sexp.mkAtom('vector', noRange),
            ...(v as T[]).map((e) => toSexp(e, noRange))
          ], '(', range)
        } else if (isClosure(v)) {
          const cls = v as Closure
          Sexp.mkList([
            Sexp.mkAtom('lambda', noRange),
            Sexp.mkList(cls.params.map((p) => Sexp.mkAtom(p, noRange)), '(', noRange),
            // TODO: we could raise the Ops back to an Exp but then
            // we would need to port all of the raising code from
            // Sem to here, too! Perhaps pretty-printing should be
            // regelated to another file after all...
            Sexp.mkAtom('<closure>', noRange)
          ], '(', range)
        } else if (isJsFunction(v)) {
          return Sexp.mkAtom('[Function (JS)]', range)
        } else if (isChar(v)) {
          const ch = v as Char
          return Sexp.mkAtom(`#\\${charToName(ch.value)}`, range)
        } else if (isList(v)) {
          const values = listToArray(v as List)
          return Sexp.mkList([
            Sexp.mkAtom('list', noRange),
            ...(values.map((e) => toSexp(e, noRange)))
          ], '(', range)
        } else if (isPair(v)) {
          const p = v as Pair
          return Sexp.mkList([
            Sexp.mkAtom('pair', noRange),
            toSexp(p.fst, noRange),
            toSexp(p.snd, noRange)
          ], '(', range)
        } else if (isStruct(v)) {
          const s = v as Struct
          const fields = getFieldsOfStruct(s)
          return Sexp.mkList([
            Sexp.mkAtom('struct', noRange),
            Sexp.mkAtom(s.kind, noRange),
            ...fields.map((f) => toSexp(s[f], noRange))
          ], '(', range)
        }
    }
    throw new ICE('valueToSexp', `unknown value ${v}`)
  }

  export function valuesEqual (v1: T, v2: T): boolean {
    const t1 = typeof v1
    const t2 = typeof v2
    if ((v1 === null && v2 === null) ||
        (v1 === undefined && v2 === undefined)) {
          return true
    } else if ((t1 === 'number' && t2 === 'number') ||
        (t1 === 'boolean' && t2 === 'boolean') ||
        (t1 === 'string' && t2 === 'string') ||
        (t1 === 'undefined' && t2 === 'undefined') ||
        // N.B., function equality is reference-ish equality
        (isFunction(t1) && isFunction(t2))) {
      return v1 === v2
    } else if (isPair(v1) && isPair(v2)) {
      return valuesEqual((v1 as Pair).fst, (v2 as Pair).fst) &&
        valuesEqual((v1 as Pair).snd, (v2 as Pair).snd)
    } else if (isChar(v1) && isChar(v2)) {
      return (v1 as Char).value === (v2 as Char).value
    } else if (isStruct(v1) && isStruct(v2)) {
      const s1 = v1 as Struct
      const s2 = v2 as Struct
      const fieldsS1 = getFieldsOfStruct(s1)
      const fieldsS2 = getFieldsOfStruct(s2)
      return s1.kind === s2.kind && fieldsS1.length === fieldsS2.length &&
        fieldsS1.every((f) => valuesEqual(s1[f], s2[f]))
    } else {
      return false
    }
  }

  export function typeOfValue (v: T): string {
    const t = typeof v
    if (t === 'boolean' || t === 'number' || t === 'string') { return t }
    if (v === 'null') { return 'null' }
    if (v === 'undefined') { return 'void'}
    if (Array.isArray(v)) { return 'vector' }
    if (isChar(v)) { return 'char' }
    if (isList(v)) { return 'list' }
    if (isStruct(v)) { return `struct (${(v as Struct).kind})` }
    if (t === 'function' || isClosure(v)) { return 'function' }
    return 'object'
  }
}

export class Env {
  private bindings: Map<Id, Value.T>
  private parent?: Env

  constructor (bindings: [Id, Value.T][], parent?: Env) {
    this.bindings = new Map(bindings)
    this.parent = parent
  }

  has (name: Id): boolean {
    return this.bindings.has(name) ||
      (this.parent !== undefined && this.parent.has(name))
  }

  get (name: Id): Value.T | undefined {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)
    } else if (this.parent !== undefined && this.parent.has(name)) {
      return this.parent.get(name)
    }
  }

  extend (bindings: [Id, Value.T][]): Env {
    return new Env(bindings, this)
  }

  set (name: Id, v: Value.T): void {
    this.bindings.set(name, v)
  }
}
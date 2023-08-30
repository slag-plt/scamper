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

  constructor (phase: Phase, msg: string, modName?: string, range?: Range) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
  }

  toString(): string {
    const detail = `${this.modName ? this.modName : ''}${this.range ? this.range.toString() : ''}`
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${this.message}`
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

export type Bracket = '(' | '[' | '{'
export type Sexp = Atom | List
export type Atom = { tag: 'atom', value: string, range: Range }
export type List = { tag: 'list', bracket: Bracket, value: Sexp[], range: Range }

export const mkAtom = (value: string, range: Range): Atom =>
  ({ tag: 'atom', value, range })
export const mkList = (value: Sexp[], bracket: Bracket, range: Range): List =>
  ({ tag: 'list', bracket, value, range }) 

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

export function sexpToString (s: Sexp): string {
  switch (s.tag) {
    case 'atom':
      return s.value
    case 'list':
      return `${surround(s.value.map(sexpToString).join(' '), s.bracket)}`
  }
}

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

export type Id = string
export type Binding = { name: Id, body: Exp }

export type MatchBranch = { pattern: Pat, body: Exp }

export type Pat = PVar | PWild | PNum | PBool | PChar | PStr | PNull | PCtor
export type PVar = { tag: 'var', name: string, range: Range }
export type PWild = { tag: 'wild', range: Range }
export type PNum = { tag: 'num', value: number, range: Range }
export type PBool = { tag: 'bool', value: boolean, range: Range }
export type PChar = { tag: 'char', value: string, range: Range }
export type PStr = { tag: 'str', value: string, range: Range }
export type PNull = { tag: 'null', range: Range }
export type PCtor = { tag: 'ctor', ctor: string, args: Pat[], range: Range }

export const mkPVar = (name: string, range: Range): Pat => ({ tag: 'var', name, range })
export const mkPWild = (range: Range): Pat => ({ tag: 'wild', range })
export const mkPNum = (value: number, range: Range): Pat => ({ tag: 'num', value, range })
export const mkPBool = (value: boolean, range: Range): Pat => ({ tag: 'bool', value, range })
export const mkPChar = (value: string, range: Range): Pat => ({ tag: 'char', value, range })
export const mkPStr = (value: string, range: Range): Pat => ({ tag: 'str', value, range })
export const mkPNull = (range: Range): Pat => ({ tag: 'null', range })
export const mkPCtor = (ctor: string, args: Pat[], range: Range): Pat => ({ tag: 'ctor', ctor, args, range })

export function patToSexp (p: Pat): Sexp {
  switch (p.tag) {
    case 'var':
      return mkAtom(p.name, p.range)
    case 'wild':
      return mkAtom('_', p.range)
    case 'num':
      return mkAtom(p.value.toString(), p.range)
    case 'bool':
      return mkAtom(p.value ? "#t" : "#f", p.range)
    case 'char':
      return mkAtom(`#\\${charToName(p.value)}`, p.range)
    case 'str':
      return mkAtom(`"${p.value}"`, p.range)
    case 'null':
      return mkAtom('null', p.range)
    case 'ctor':
      return mkList([mkAtom(p.ctor, noRange), ...p.args.map(patToSexp)], '(', p.range)
  }
}

export function patToString (p: Pat): string {
  return sexpToString(patToSexp(p))
}

export type Exp = Var | Num | Bool | Char | Str | Lam | Let | App | If | Begin | Match
export type Var = { tag: 'var', name: string, range: Range }
export type Num = { tag: 'num', value: number, range: Range }
export type Bool = { tag: 'bool', value: boolean, range: Range }
export type Char = { tag: 'char', value: string, range: Range }
export type Str = { tag: 'str', value: string, range: Range }
export type Lam = { tag: 'lam', args: Id[], body: Exp, bracket: Bracket, range: Range }
export type Let = { tag: 'let', bindings: Binding[], body: Exp, bracket: Bracket, range: Range }
export type App = { tag: 'app', head: Exp, args: Exp[], bracket: Bracket, range: Range }
export type If = { tag: 'if', guard: Exp, ifb: Exp, elseb: Exp, bracket: Bracket, range: Range }
export type Begin = { tag: 'begin', exps: Exp[], bracket: Bracket, range: Range }
export type Match = { tag: 'match', scrutinee: Exp, branches: MatchBranch[], bracket: Bracket, range: Range }

export const mkVar = (name: string, range: Range): Exp =>
  ({ tag: 'var', name, range })
export const mkNum = (value: number, range: Range): Exp =>
  ({ tag: 'num', value, range })
export const mkBool = (value: boolean, range: Range): Exp =>
  ({ tag: 'bool', value, range })
export const mkChar = (value: string, range: Range): Exp =>
  ({ tag: 'char', value, range })  
export const mkStr = (value: string, range: Range): Exp =>
  ({ tag: 'str', value, range })
export const mkLam = (args: Id[], body: Exp, bracket: Bracket, range: Range): Exp =>
  ({ tag: 'lam', args, body, bracket, range })
export const mkLet = (bindings: Binding[], body: Exp, bracket: Bracket, range: Range): Exp => 
  ({ tag: 'let', bindings, body, bracket, range })
export const mkApp = (head: Exp, args: Exp[], bracket: Bracket, range: Range): Exp =>
  ({ tag: 'app', head, args, bracket, range })
export const mkIf = (guard: Exp, ifb: Exp, elseb: Exp, bracket: Bracket, range: Range): Exp =>
  ({ tag: 'if', guard, ifb, elseb, bracket, range })
export const mkBegin = (exps: Exp[], bracket: Bracket, range: Range): Exp =>
  ({ tag: 'begin', exps, bracket, range })
export const mkMatch = (scrutinee: Exp, branches: MatchBranch[], bracket: Bracket, range: Range): Exp =>
  ({ tag: 'match', scrutinee, branches, bracket, range })

export function expToSexp(e: Exp): Sexp {
  switch (e.tag) {
    case 'var':
      return mkAtom(e.name, e.range)
    case 'num':
      return mkAtom(e.value.toString(), e.range)
    case 'bool':
      return mkAtom(e.value ? "#t" : "#f", e.range)
    case 'char':
      return mkAtom(`#\\${charToName(e.value)}`, e.range)
    case 'str':
      return mkAtom(`"${e.value}"`, e.range)
    case 'lam':
      return mkList([
        mkAtom('lambda', noRange),
        mkList(e.args.map((arg) => mkAtom(arg, noRange)), '(', noRange), expToSexp(e.body)],
        e.bracket, e.range)
    case 'let':
      return mkList([
        mkAtom('let', noRange),
        mkList(e.bindings.map(
          b => mkList([mkAtom(b.name, noRange), expToSexp(b.body)], '[', noRange)),
          '(', noRange),
        expToSexp(e.body)], e.bracket, e.range)
    case 'app':
      return mkList([expToSexp(e.head), ...e.args.map(expToSexp)], '(', e.range)
    case 'if':
      return mkList([mkAtom('if', noRange), expToSexp(e.guard), expToSexp(e.ifb), expToSexp(e.elseb)], '(', e.range)
    case 'begin':
      return mkList([mkAtom('begin', noRange), ...e.exps.map(expToSexp)], '(', e.range)
    case 'match':
      return mkList([
        mkAtom('match', noRange),
        expToSexp(e.scrutinee),
        mkList(e.branches.map((b) => mkList([patToSexp(b.pattern), expToSexp(b.body)], '[', noRange)), '(', noRange)],
        e.bracket, e.range)
  }
}

export function expToString(e: Exp): string {
  return sexpToString(expToSexp(e))
}

export type Stmt = StmtBinding | StmtExp | Import | Display | Struct
export type StmtBinding = { tag: 'binding', name: Id, body: Exp, bracket: Bracket, range: Range }
export type StmtExp = { tag: 'stmtexp', body: Exp }
export type Import = { tag: 'import', modName: string, bracket: Bracket, range: Range }
export type Display = { tag: 'display', body: Exp, bracket: Bracket, range: Range }
export type Struct = { tag: 'struct', id: string, fields: string[], bracket: Bracket, range: Range }

export const mkStmtBinding = (name: Id, body: Exp, bracket: Bracket, range: Range): Stmt =>
  ({ tag: 'binding', name, body, bracket, range })
export const mkStmtExp = (body: Exp): Stmt => ({ tag: 'stmtexp', body })
export const mkImport = (modName: string, bracket: Bracket, range: Range): Stmt => ({ tag: 'import', modName, bracket, range })
export const mkDisplay = (body: Exp, bracket: Bracket, range: Range): Stmt => ({ tag: 'display', body, bracket, range })
export const mkStruct = (id: string, fields: string[], bracket: Bracket, range: Range): Stmt => ({ tag: 'struct', id, fields, bracket, range })

export function stmtToSexp(s: Stmt): Sexp {
  switch (s.tag) {
    case 'binding':
      return mkList([mkAtom('define', noRange), mkAtom(s.name, noRange), expToSexp(s.body)], s.bracket, s.range)
    case 'stmtexp':
      return expToSexp(s.body)
    case 'import':
      return mkList([mkAtom('import', noRange), mkAtom(s.modName, noRange)], s.bracket, s.range)
    case 'display':
      return mkList([mkAtom('define', noRange), expToSexp(s.body)], s.bracket, s.range)
    case 'struct':
      return mkList([mkAtom('struct', noRange), mkAtom(s.id, noRange), ...s.fields.map((f) => mkAtom(f, noRange))], '(', s.range)
  }
}

export function stmtToString(s: Stmt): string {
  return sexpToString(stmtToSexp(s))
}

export type Prog = Stmt[]

export function progToSexps(p: Prog): Sexp[] {
  return p.map(stmtToSexp)
}

export function progToString(p: Prog, sep: string = '\n\n'): string {
  return progToSexps(p).map(sexpToString).join(sep)
}

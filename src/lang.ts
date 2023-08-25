export class Loc {
  line: number
  col: number

  constructor (line: number, col: number) {
    this.line = line
    this.col = col
  }

  public toString (): string {
    return `${this.line}:${this.col}`
  }
}

export class Range {
  begin: Loc
  end: Loc

  constructor (startLine: number, startCol: number, endLine: number, endCol: number) {
    this.begin = new Loc(startLine, startCol)
    this.end = new Loc(endLine, endCol)
  }

  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }
}

export const mkRange = (beg: Loc, end: Loc): Range => new Range(beg.line, beg.col, end.line, end.col)

export const noLoc = new Loc(-1, -1)
export const noRange = new Range(-1, -1, -1, -1)

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

export type Id = string
export type Binding = { name: Id, body: Exp }

export type Exp = Var | Num | Bool | Str | Lam | Let | App | If
export type Var = { tag: 'var', name: string, range: Range }
export type Num = { tag: 'num', value: number, range: Range }
export type Bool = { tag: 'bool', value: boolean, range: Range }
export type Str = { tag: 'str', value: string, range: Range }
export type Lam = { tag: 'lam', args: Id[], body: Exp, bracket: Bracket, range: Range }
export type Let = { tag: 'let', bindings: Binding[], body: Exp, bracket: Bracket, range: Range }
export type App = { tag: 'app', head: Exp, args: Exp[], bracket: Bracket, range: Range }
export type If = { tag: 'if', guard: Exp, ifb: Exp, elseb: Exp, bracket: Bracket, range: Range }

export const mkVar = (name: string, range: Range): Exp =>
  ({ tag: 'var', name, range })
export const mkNum = (value: number, range: Range): Exp =>
  ({ tag: 'num', value, range })
export const mkBool = (value: boolean, range: Range): Exp =>
  ({ tag: 'bool', value, range })
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

export function expToSexp(e: Exp): Sexp {
  switch (e.tag) {
    case 'var':
      return mkAtom(e.name, e.range)
    case 'num':
      return mkAtom(e.value.toString(), e.range)
    case 'bool':
      return mkAtom(e.value ? "#t" : "#f", e.range)
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
  }
}

export function expToString(e: Exp): string {
  return sexpToString(expToSexp(e))
}

export type Stmt = StmtBinding | StmtExp | Import | Display
export type StmtBinding = { tag: 'binding', name: Id, body: Exp, bracket: Bracket, range: Range }
export type StmtExp = { tag: 'stmtexp', body: Exp }
export type Import = { tag: 'import', modName: string, bracket: Bracket, range: Range }
export type Display = { tag: 'display', body: Exp, bracket: Bracket, range: Range }

export const mkStmtBinding = (name: Id, body: Exp, bracket: Bracket, range: Range): Stmt =>
  ({ tag: 'binding', name, body, bracket, range })
export const mkStmtExp = (body: Exp): Stmt => ({ tag: 'stmtexp', body })
export const mkImport = (modName: string, bracket: Bracket, range: Range): Stmt => ({ tag: 'import', modName, bracket, range })
export const mkDisplay = (body: Exp, bracket: Bracket, range: Range): Stmt => ({ tag: 'display', body, bracket, range })

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

import * as L from '../lpm'

///// Language Definition //////////////////////////////////////////////////////

// e ::= n | b | s | c
//     | null | void
//     | (e1 ... ek)
//
//     -- Special forms
//     | (lambda (x1 ... xk) 
//         e)
//     | (let
//         ([x1 e1]
//          ...
//          [xk ek])
//         e)
//     | (begin
//         e1
//         ...
//         ek)
//     | (if e1
//         e2
//         e3)
//     | (match e
//         [p1 e1]
//         ...
//         [pk ek])
//     | (quote e)
//
//     -- Sugared forms
//     | (let*
//         ([x1 e1]
//          ...
//          [xk ek])
//    e)
//     | (and e1 ... ek)
//     | (or e1 ... ek)
//     | (cond [e11 e12] ... [e1k e2k])
//     | (section e1 ... ek)
//
// s ::= e
//     | (import m)
//     | (define x e)
//     | (display e)
//     | e
//
//     -- Sugared form
//     | (struct S (f1 ... fk))

///// Patterns /////

export type PWild = { tag: 'pwild', range: L.Range }
export type PVar  = { tag: 'pvar', name: string, range: L.Range }
export type PLit  = { tag: 'plit', value: L.Value, range: L.Range }
export type PCtor = { tag: 'pctor', name: string, args: Pat[], range: L.Range }
export type Pat = PWild | PVar | PLit | PCtor

///// Expressions /////

// Core Forms
export type Lit   = { tag: 'lit', value: L.Value, range: L.Range }
export type Var   = { tag: 'var', name: string, range: L.Range }
export type App   = { tag: 'app', head: Exp, args: Exp[], range: L.Range }
export type Lam   = { tag: 'lam', params: string[], body: Exp, range: L.Range }
export type Let   = { tag: 'let', bindings: { name: string, value: Exp }[], body: Exp, range: L.Range }
export type Begin = { tag: 'begin', exps: Exp[], range: L.Range }
export type If    = { tag: 'if', guard: Exp, ifB: Exp, elseB: Exp, range: L.Range }
export type Match = { tag: 'match', scrutinee: Exp, branches: { pat: Pat, body: Exp }[], range: L.Range }
export type Quote = { tag: 'quote', value: L.Value, range: L.Range }

// Sugared Forms
export type LetS  = { tag: 'let*', bindings: { name: string, value: Exp }[], body: Exp, range: L.Range }
export type And   = { tag: 'and', exps: Exp[], range: L.Range }
export type Or    = { tag: 'or', exps: Exp[], range: L.Range }
export type Cond  = { tag: 'cond', branches: { test: Exp, body: Exp }[], range: L.Range }
export type Section = { tag: 'section', exps: Exp[], range: L.Range }

export type Exp = Lit | Var | App | Lam | Let | Begin | If | Match | Quote | LetS | And | Or | Cond | Section

///// Statements /////

// Core Forms
export type Import  = { tag: 'import', module: string, range: L.Range }
export type Define  = { tag: 'define', name: string, value: Exp, range: L.Range }
export type Disp    = { tag: 'display', value: Exp, range: L.Range }
export type StmtExp = { tag: 'stmtexp', expr: Exp, range: L.Range }

// Sugared Forms
export type Struct  = { tag: 'struct', name: string, fields: string[], range: L.Range }

export type Stmt = Import | Define | Disp | StmtExp | Struct

///// Programs /////

export type Prog = Stmt[]

///// Constructor Functions ////////////////////////////////////////////////////

// Patterns (pat)
export const mkPWild = (range: L.Range = L.Range.none): PWild => ({ tag: 'pwild', range })
export const mkPVar = (name: string, range: L.Range = L.Range.none): PVar => ({ tag: 'pvar', name, range })
export const mkPLit = (value: L.Value, range: L.Range = L.Range.none): PLit => ({ tag: 'plit', value, range })
export const mkPCtor = (name: string, args: Pat[], range: L.Range = L.Range.none): PCtor => ({ tag: 'pctor', name, args, range })

// Expressions (exp)
export const mkLit = (value: L.Value, range: L.Range = L.Range.none): Lit => ({ tag: 'lit', value, range })
export const mkVar = (name: string, range: L.Range = L.Range.none): Var => ({ tag: 'var', name, range })
export const mkApp = (head: Exp, args: Exp[], range: L.Range = L.Range.none): App => ({ tag: 'app', head, args, range })
export const mkLam = (params: string[], body: Exp, range: L.Range = L.Range.none): Lam => ({ tag: 'lam', params, body, range })
export const mkLet = (bindings: { name: string, value: Exp }[], body: Exp, range: L.Range = L.Range.none): Let => ({ tag: 'let', bindings, body, range })
export const mkBegin = (exps: Exp[], range: L.Range = L.Range.none): Begin => ({ tag: 'begin', exps, range })
export const mkIf = (guard: Exp, ifB: Exp, elseB: Exp, range: L.Range = L.Range.none): If => ({ tag: 'if', guard, ifB, elseB, range })
export const mkMatch = (scrutinee: Exp, branches: { pat: Pat, body: Exp }[], range: L.Range = L.Range.none): Match => ({ tag: 'match', scrutinee, branches, range })
export const mkQuote = (value: L.Value, range: L.Range = L.Range.none): Quote => ({ tag: 'quote', value, range })
export const mkLetS = (bindings: { name: string, value: Exp }[], body: Exp, range: L.Range = L.Range.none): LetS => ({ tag: 'let*', bindings, body, range })
export const mkAnd = (exps: Exp[], range: L.Range = L.Range.none): And => ({ tag: 'and', exps, range })
export const mkOr = (exps: Exp[], range: L.Range = L.Range.none): Or => ({ tag: 'or', exps, range })
export const mkCond = (branches: { test: Exp, body: Exp }[], range: L.Range = L.Range.none): Cond => ({ tag: 'cond', branches, range })
export const mkSection = (exps: Exp[], range: L.Range = L.Range.none): Section => ({ tag: 'section', exps, range })

// Statements (stmt)
export const mkImport = (module: string, range: L.Range = L.Range.none): Import => ({ tag: 'import', module, range })
export const mkDefine = (name: string, value: Exp, range: L.Range = L.Range.none): Define => ({ tag: 'define', name, value, range })
export const mkDisp = (value: Exp, range: L.Range = L.Range.none): Disp => ({ tag: 'display', value, range })
export const mkStmtExp = (expr: Exp, range: L.Range = L.Range.none): StmtExp => ({ tag: 'stmtexp', expr, range })
export const mkStruct = (name: string, fields: string[], range: L.Range = L.Range.none): Struct => ({ tag: 'struct', name, fields, range })

///// Utility Functions ////////////////////////////////////////////////////////

export function patToString (pat: Pat): string {
  switch (pat.tag) {
    case 'pwild': return '_'
    case 'pvar': return pat.name
    case 'plit': return JSON.stringify(pat.value)
    case 'pctor': {
      if (pat.args.length === 0) {
        return `(${pat.name})`
      } else {
        return `(${pat.name} ${pat.args.map(patToString).join(' ')})`
      }
    }
  }
}

export function expToString (e: Exp): string {
  switch (e.tag) {
    case 'lit': return JSON.stringify(e)
    case 'var': return e.name
    case 'app': {
      if (e.args.length === 0) {
        return `(${e.head})`
      } else {
        return `(${e.head} ${e.args.map(expToString).join(' ')})`
      }
    }
    case 'lam': return `(lambda (${e.params.join(' ')}) ${expToString(e.body)})`
    case 'let':
      return `(let (${e.bindings.map(({name, value}) => `[${name} ${expToString(value)}]`).join(' ')}) ${expToString(e.body)})`
    case 'begin':
      return `(begin ${e.exps.map(expToString).join(' ')})`
    case 'if':
      return `(if ${expToString(e.guard)} ${expToString(e.ifB)} ${expToString(e.elseB)})`
    case 'match':
      return `(match ${expToString(e.scrutinee)} ${e.branches.map(({pat, body}) => `[${patToString(pat)} ${expToString(body)}]`).join(' ')})`
    case 'quote':
      return `(quote ${JSON.stringify(e.value)})`
    case 'let*':
      return `(let* (${e.bindings.map(({name, value}) => `[${name} ${expToString(value)}]`).join(' ')}) ${expToString(e.body)})`
    case 'and':
      return `(and ${e.exps.map(expToString).join(' ')})`
    case 'or':
      return `(or ${e.exps.map(expToString).join(' ')})`
    case 'cond':
      return `(cond ${e.branches.map(({test, body}) => `[${expToString(test)} ${expToString(body)}]`).join(' ')})`
    case 'section':
      return `(section ${e.exps.map(expToString).join(' ')})`
  }
}

export function stmtToString (s: Stmt): string {
  switch (s.tag) {
    case 'import': return `(import ${s.module})`
    case 'define': return `(define ${s.name} ${expToString(s.value)})`
    case 'display': return `(display ${expToString(s.value)})`
    case 'stmtexp': return expToString(s.expr)
    case 'struct': return `(struct ${s.name} (${s.fields.join(' ')}))`
  }
}

export function progToString (p: Prog): string {
  return `${p.map(stmtToString).join('\n')}`
}
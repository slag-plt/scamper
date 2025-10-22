import * as L from '../lpm'
import TextRenderer from '../lpm/renderers/text-renderer.js'

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

///// Query Functions //////////////////////////////////////////////////////////

export function isPat (v: any): v is Pat {
  return typeof v === 'object' && v !== null && [
    'pwild', 'pvar', 'plit', 'pctor'
  ].includes(v.tag)
}

export function isExp (v: any): v is Exp {
  return typeof v === 'object' && v !== null && [
    'lit', 'var', 'app', 'lam', 'let', 'begin', 'if', 'match',
    'quote', 'let*', 'and', 'or', 'cond', 'section'
  ].includes(v.tag)
}

export function isStmt (v: any): v is Stmt {
  return typeof v === 'object' && v !== null && [
    'import', 'define', 'display', 'stmtexp', 'struct'
  ].includes(v.tag)
}

///// Stringifying Functions ///////////////////////////////////////////////////

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
    case 'lit': return JSON.stringify(e.value)
    case 'var': return e.name
    case 'app': {
      if (e.args.length === 0) {
        return `(${expToString(e.head)})`
      } else {
        return `(${expToString(e.head)} ${e.args.map(expToString).join(' ')})`
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

TextRenderer.registerCustomRenderer(isPat, (v) => patToString(v as Pat))
TextRenderer.registerCustomRenderer(isExp, (v) => expToString(v as Exp))
TextRenderer.registerCustomRenderer(isStmt, (v) => stmtToString(v as Stmt))

///// Equality /////////////////////////////////////////////////////////////////

export function patEquals (p1: Pat, p2: Pat): boolean {
  if (p1.tag === 'pwild' && p2.tag === 'pwild') {
    return true
  } else if (p1.tag === 'pvar' && p2.tag === 'pvar') {
    return p1.name === p2.name
  } else if (p1.tag === 'plit' && p2.tag === 'plit') {
    return L.equals(p1.value, p2.value)
  } else if (p1.tag === 'pctor' && p2.tag === 'pctor') {
    return p1.name === p2.name &&
           p1.args.length === p2.args.length &&
           p1.args.every((arg, i) => patEquals(arg, p2.args[i]))
  } else {
    return false
  }
}

export function expEquals (e1: Exp, e2: Exp): boolean {
  if (e1.tag === 'lit' && e2.tag === 'lit') {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === 'var' && e2.tag === 'var') {
    return e1.name === e2.name
  } else if (e1.tag === 'app' && e2.tag === 'app') {
    return expEquals(e1.head, e2.head) &&
           e1.args.length === e2.args.length &&
           e1.args.every((arg, i) => expEquals(arg, e2.args[i]))
  } else if (e1.tag === 'lam' && e2.tag === 'lam') {
    return e1.params.length === e2.params.length &&
           e1.params.every((param, i) => param === e2.params[i]) &&
           expEquals(e1.body, e2.body)
  } else if (e1.tag === 'let' && e2.tag === 'let') {
    return e1.bindings.length === e2.bindings.length &&
           e1.bindings.every(({name}, i) => name === e2.bindings[i].name) && 
           e1.bindings.every(({value}, i) => expEquals(value, e2.bindings[i].value)) &&
           expEquals(e1.body, e2.body)
  } else if (e1.tag === 'begin' && e2.tag === 'begin') {
    return e1.exps.length === e2.exps.length &&
           e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
  } else if (e1.tag === 'if' && e2.tag === 'if') {
    return expEquals(e1.guard, e2.guard) &&
           expEquals(e1.ifB, e2.ifB) &&
           expEquals(e1.elseB, e2.elseB)
  } else if (e1.tag === 'match' && e2.tag === 'match') {
    return expEquals(e1.scrutinee, e2.scrutinee) &&
           e1.branches.length === e2.branches.length &&
           e1.branches.every(({pat}, i) => patEquals(pat, e2.branches[i].pat)) &&
           e1.branches.every(({body}, i) => expEquals(body, e2.branches[i].body))
  } else if (e1.tag === 'quote' && e2.tag === 'quote') {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === 'let*' && e2.tag === 'let*') {
    return e1.bindings.length === e2.bindings.length &&
           e1.bindings.every(({name}, i) => name === e2.bindings[i].name) && 
           e1.bindings.every(({value}, i) => expEquals(value, e2.bindings[i].value)) &&
           expEquals(e1.body, e2.body)
  } else if (e1.tag === 'and' && e2.tag === 'and') {
    return e1.exps.length === e2.exps.length &&
           e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
  } else if (e1.tag === 'or' && e2.tag === 'or') {
    return e1.exps.length === e2.exps.length &&
           e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
  } else if (e1.tag === 'cond' && e2.tag === 'cond') {
    return e1.branches.length === e2.branches.length &&
           e1.branches.every(({test}, i) => expEquals(test, e2.branches[i].test)) &&
           e1.branches.every(({body}, i) => expEquals(body, e2.branches[i].body))
  } else if (e1.tag === 'section' && e2.tag === 'section') {
    return e1.exps.length === e2.exps.length &&
           e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
  } else {
    return false
  }
}

export function stmtEquals (s1: Stmt, s2: Stmt): boolean {
  if (s1.tag === 'import' && s2.tag === 'import') {
    return s1.module === s2.module
  } else if (s1.tag === 'define' && s2.tag === 'define') {
    return s1.name === s2.name && expEquals(s1.value, s2.value)
  } else if (s1.tag === 'display' && s2.tag === 'display') {
    return expEquals(s1.value, s2.value)
  } else if (s1.tag === 'stmtexp' && s2.tag === 'stmtexp') {
    return expEquals(s1.expr, s2.expr)
  } else if (s1.tag === 'struct' && s2.tag === 'struct') {
    return s1.name === s2.name &&
           s1.fields.length === s2.fields.length &&
           s1.fields.every((field, i) => field === s2.fields[i])
  } else {
    return false
  }
}
import * as L from "../lpm"
import TextRenderer from "../lpm/renderers/text.js"
import { FunctionDoc } from "./docstring/docstring"

export interface Tagged {
  tag: string
}
export interface Node {
  range: L.Range
}

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
//     -- Internal form, produced only by the query system (query.ts) to
//        wrap the expression under a cursor for tooltip evaluation; not
//        user-facing surface syntax
//     | (report e)
//
// s ::= e
//     | (import m)
//     | (define x e)
//     | ;;; <docstring>
//       (define x e)
//     | (display e)
//     | e
//
//     -- Sugared form
//     | (struct S (f1 ... fk))

///// Patterns /////

export interface PWild extends Tagged, Node {
  tag: "pwild"
}
export interface PVar extends Tagged, Node {
  tag: "pvar"
  name: string
}
export interface PLit extends Tagged, Node {
  tag: "plit"
  value: L.Value
}
export interface PCtor extends Tagged, Node {
  tag: "pctor"
  name: string
  args: Pat[]
}
export type Pat = PWild | PVar | PLit | PCtor

///// Expressions /////

// Core Forms
export interface Lit extends Tagged, Node {
  tag: "lit"
  value: L.Value
}
export interface Var extends Tagged, Node {
  tag: "var"
  name: string
}
export interface App extends Tagged, Node {
  tag: "app"
  head: Exp
  args: Exp[]
}
export interface Lam extends Tagged, Node {
  tag: "lam"
  params: string[]
  body: Exp
}
export interface Let extends Tagged, Node {
  tag: "let"
  bindings: { name: string; value: Exp }[]
  body: Exp
}
export interface Begin extends Tagged, Node {
  tag: "begin"
  exps: Exp[]
}
export interface If extends Tagged, Node {
  tag: "if"
  guard: Exp
  ifB: Exp
  elseB: Exp
}
export interface Match extends Tagged, Node {
  tag: "match"
  scrutinee: Exp
  branches: { pat: Pat; body: Exp }[]
}
export interface Quote extends Tagged, Node {
  tag: "quote"
  value: L.Value
}

// Sugared Forms
export interface LetS extends Tagged, Node {
  tag: "let*"
  bindings: { name: string; value: Exp }[]
  body: Exp
}
export interface And extends Tagged, Node {
  tag: "and"
  exps: Exp[]
}
export interface Or extends Tagged, Node {
  tag: "or"
  exps: Exp[]
}
export interface Cond extends Tagged, Node {
  tag: "cond"
  branches: { test: Exp; body: Exp }[]
}
export interface Section extends Tagged, Node {
  tag: "section"
  exps: Exp[]
}
export interface Report extends Tagged, Node {
  tag: "report"
  exp: Exp
}

export type Exp =
  | Lit
  | Var
  | App
  | Lam
  | Let
  | Begin
  | If
  | Match
  | Quote
  | LetS
  | And
  | Or
  | Cond
  | Section
  | Report

///// Statements /////

// Core Forms
export interface Import extends Tagged, Node {
  tag: "import"
  module: string
}
export interface Define extends Tagged, Node {
  tag: "define"
  name: string
  value: Exp
  doc?: FunctionDoc
}
export interface Disp extends Tagged, Node {
  tag: "display"
  value: Exp
}
export interface StmtExp extends Tagged, Node {
  tag: "stmtexp"
  expr: Exp
}

// Sugared Forms
export interface Struct extends Tagged, Node {
  tag: "struct"
  name: string
  fields: string[]
}

export type Stmt = Import | Define | Disp | StmtExp | Struct

///// Programs /////

export type Prog = Stmt[]
export interface ProgNode extends Node {
  tag: "prog"
  body: Prog
}

/** Union of every AST node type — the Prettier plugin's canonical node type. */
export type SchemeNode = ProgNode | Stmt | Exp | Pat

///// Helper functions /////////////////////////////////////////////////////////

export function progToNode(prog: Prog): ProgNode {
  const range =
    prog.length === 0
      ? L.Range.none
      : new L.Range(prog[0].range.begin, prog[prog.length - 1].range.end)
  return { tag: "prog", body: prog, range }
}

///// Constructor Functions ////////////////////////////////////////////////////

// Patterns (pat)
export const mkPWild = (range: L.Range = L.Range.none): PWild => ({
  tag: "pwild",
  range,
})
export const mkPVar = (name: string, range: L.Range = L.Range.none): PVar => ({
  tag: "pvar",
  name,
  range,
})
export const mkPLit = (
  value: L.Value,
  range: L.Range = L.Range.none,
): PLit => ({ tag: "plit", value, range })
export const mkPCtor = (
  name: string,
  args: Pat[],
  range: L.Range = L.Range.none,
): PCtor => ({ tag: "pctor", name, args, range })

// Expressions (exp)
export const mkLit = (value: L.Value, range: L.Range = L.Range.none): Lit => ({
  tag: "lit",
  value,
  range,
})
export const mkVar = (name: string, range: L.Range = L.Range.none): Var => ({
  tag: "var",
  name,
  range,
})
export const mkApp = (
  head: Exp,
  args: Exp[],
  range: L.Range = L.Range.none,
): App => ({ tag: "app", head, args, range })
export const mkLam = (
  params: string[],
  body: Exp,
  range: L.Range = L.Range.none,
): Lam => ({ tag: "lam", params, body, range })
export const mkLet = (
  bindings: { name: string; value: Exp }[],
  body: Exp,
  range: L.Range = L.Range.none,
): Let => ({ tag: "let", bindings, body, range })
export const mkBegin = (exps: Exp[], range: L.Range = L.Range.none): Begin => ({
  tag: "begin",
  exps,
  range,
})
export const mkIf = (
  guard: Exp,
  ifB: Exp,
  elseB: Exp,
  range: L.Range = L.Range.none,
): If => ({ tag: "if", guard, ifB, elseB, range })
export const mkMatch = (
  scrutinee: Exp,
  branches: { pat: Pat; body: Exp }[],
  range: L.Range = L.Range.none,
): Match => ({ tag: "match", scrutinee, branches, range })
export const mkQuote = (
  value: L.Value,
  range: L.Range = L.Range.none,
): Quote => ({ tag: "quote", value, range })
export const mkLetS = (
  bindings: { name: string; value: Exp }[],
  body: Exp,
  range: L.Range = L.Range.none,
): LetS => ({ tag: "let*", bindings, body, range })
export const mkAnd = (exps: Exp[], range: L.Range = L.Range.none): And => ({
  tag: "and",
  exps,
  range,
})
export const mkOr = (exps: Exp[], range: L.Range = L.Range.none): Or => ({
  tag: "or",
  exps,
  range,
})
export const mkCond = (
  branches: { test: Exp; body: Exp }[],
  range: L.Range = L.Range.none,
): Cond => ({ tag: "cond", branches, range })
export const mkSection = (
  exps: Exp[],
  range: L.Range = L.Range.none,
): Section => ({ tag: "section", exps, range })
export const mkReport = (exp: Exp, range: L.Range = L.Range.none): Report => ({
  tag: "report",
  exp,
  range,
})

// Statements (stmt)
export const mkImport = (
  module: string,
  range: L.Range = L.Range.none,
): Import => ({ tag: "import", module, range })
export const mkDefine = (
  name: string,
  value: Exp,
  range: L.Range = L.Range.none,
  doc?: FunctionDoc,
): Define => ({ tag: "define", name, value, range, doc })
export const mkDisp = (value: Exp, range: L.Range = L.Range.none): Disp => ({
  tag: "display",
  value,
  range,
})
export const mkStmtExp = (
  expr: Exp,
  range: L.Range = L.Range.none,
): StmtExp => ({ tag: "stmtexp", expr, range })
export const mkStruct = (
  name: string,
  fields: string[],
  range: L.Range = L.Range.none,
): Struct => ({ tag: "struct", name, fields, range })

///// Query Functions //////////////////////////////////////////////////////////

function isTagged(v: unknown): v is Tagged {
  return typeof v === "object" && v !== null && "tag" in v
}

export function isPat(v: unknown): v is Pat {
  return isTagged(v) && ["pwild", "pvar", "plit", "pctor"].includes(v.tag)
}

export function isExp(v: unknown): v is Exp {
  return (
    isTagged(v) &&
    [
      "lit",
      "var",
      "app",
      "lam",
      "let",
      "begin",
      "if",
      "match",
      "quote",
      "let*",
      "and",
      "or",
      "cond",
      "section",
      "report",
    ].includes(v.tag)
  )
}

export function isStmt(v: unknown): v is Stmt {
  return (
    isTagged(v) &&
    ["import", "define", "display", "stmtexp", "struct"].includes(v.tag)
  )
}

export const isStmtExp = (s: Stmt): s is StmtExp =>
  isTagged(s) && s.tag === "stmtexp"

export const isVar = (e: Exp): e is Var => isTagged(e) && e.tag === "var"

export const isApp = (e: Exp): e is App =>
  isTagged(e) && e.tag === "app" && isExp(e.head)

export const isLam = (e: Exp): e is Lam => isTagged(e) && e.tag === "lam"

export const isLit = (e: Exp): e is Lit => isTagged(e) && e.tag === "lit"

///// Stringifying Functions ///////////////////////////////////////////////////

export function patToString(pat: Pat): string {
  switch (pat.tag) {
    case "pwild":
      return "_"
    case "pvar":
      return pat.name
    case "plit":
      // TODO: should we use TextRenderer.render for this too?
      return JSON.stringify(pat.value)
    case "pctor": {
      if (pat.args.length === 0) {
        return `(${pat.name})`
      } else {
        return `(${pat.name} ${pat.args.map(patToString).join(" ")})`
      }
    }
  }
}

export function expToString(e: Exp): string {
  switch (e.tag) {
    case "lit":
      return TextRenderer.render(e.value)
    case "var":
      return e.name
    case "app": {
      if (e.args.length === 0) {
        return `(${expToString(e.head)})`
      } else {
        return `(${expToString(e.head)} ${e.args.map(expToString).join(" ")})`
      }
    }
    case "lam":
      return `(lambda (${e.params.join(" ")}) ${expToString(e.body)})`
    case "let":
      return `(let (${e.bindings.map(({ name, value }) => `[${name} ${expToString(value)}]`).join(" ")}) ${expToString(e.body)})`
    case "begin":
      return `(begin ${e.exps.map(expToString).join(" ")})`
    case "if":
      return `(if ${expToString(e.guard)} ${expToString(e.ifB)} ${expToString(e.elseB)})`
    case "match":
      return `(match ${expToString(e.scrutinee)} ${e.branches.map(({ pat, body }) => `[${patToString(pat)} ${expToString(body)}]`).join(" ")})`
    case "quote":
      return `(quote ${JSON.stringify(e.value)})`
    case "let*":
      return `(let* (${e.bindings.map(({ name, value }) => `[${name} ${expToString(value)}]`).join(" ")}) ${expToString(e.body)})`
    case "and":
      return `(and ${e.exps.map(expToString).join(" ")})`
    case "or":
      return `(or ${e.exps.map(expToString).join(" ")})`
    case "cond":
      return `(cond ${e.branches.map(({ test, body }) => `[${expToString(test)} ${expToString(body)}]`).join(" ")})`
    case "section":
      return `(section ${e.exps.map(expToString).join(" ")})`
    case "report":
      return `(report ${expToString(e.exp)})`
  }
}

export function stmtToString(s: Stmt): string {
  switch (s.tag) {
    case "import":
      return `(import ${s.module})`
    case "define":
      return `(define ${s.name} ${expToString(s.value)})`
    case "display":
      return `(display ${expToString(s.value)})`
    case "stmtexp":
      return expToString(s.expr)
    case "struct":
      return `(struct ${s.name} (${s.fields.join(" ")}))`
  }
}

export function progToString(p: Prog): string {
  return p.map(stmtToString).join("\n")
}

TextRenderer.registerCustomRenderer(isPat, (v) => patToString(v as Pat))
TextRenderer.registerCustomRenderer(isExp, (v) => expToString(v as Exp))
TextRenderer.registerCustomRenderer(isStmt, (v) => stmtToString(v as Stmt))

///// AST argument shapes (consumed by the Vue-based renderers) /////////////////

export type ASTArg = string | Pat | Exp | Stmt

export interface HljsBindings {
  head: string
  pairs: { lhs: string | Pat | Exp; rhs: Exp }[]
  body?: Exp
  scrutinee?: Exp
}

///// Equality /////////////////////////////////////////////////////////////////

export function patEquals(p1: Pat, p2: Pat): boolean {
  if (p1.tag === "pwild" && p2.tag === "pwild") {
    return true
  } else if (p1.tag === "pvar" && p2.tag === "pvar") {
    return p1.name === p2.name
  } else if (p1.tag === "plit" && p2.tag === "plit") {
    return L.equals(p1.value, p2.value)
  } else if (p1.tag === "pctor" && p2.tag === "pctor") {
    return (
      p1.name === p2.name &&
      p1.args.length === p2.args.length &&
      p1.args.every((arg, i) => patEquals(arg, p2.args[i]))
    )
  } else {
    return false
  }
}

export function expEquals(e1: Exp, e2: Exp): boolean {
  if (e1.tag === "lit" && e2.tag === "lit") {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === "var" && e2.tag === "var") {
    return e1.name === e2.name
  } else if (e1.tag === "app" && e2.tag === "app") {
    return (
      expEquals(e1.head, e2.head) &&
      e1.args.length === e2.args.length &&
      e1.args.every((arg, i) => expEquals(arg, e2.args[i]))
    )
  } else if (e1.tag === "lam" && e2.tag === "lam") {
    return (
      e1.params.length === e2.params.length &&
      e1.params.every((param, i) => param === e2.params[i]) &&
      expEquals(e1.body, e2.body)
    )
  } else if (e1.tag === "let" && e2.tag === "let") {
    return (
      e1.bindings.length === e2.bindings.length &&
      e1.bindings.every(({ name }, i) => name === e2.bindings[i].name) &&
      e1.bindings.every(({ value }, i) =>
        expEquals(value, e2.bindings[i].value),
      ) &&
      expEquals(e1.body, e2.body)
    )
  } else if (e1.tag === "begin" && e2.tag === "begin") {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === "if" && e2.tag === "if") {
    return (
      expEquals(e1.guard, e2.guard) &&
      expEquals(e1.ifB, e2.ifB) &&
      expEquals(e1.elseB, e2.elseB)
    )
  } else if (e1.tag === "match" && e2.tag === "match") {
    return (
      expEquals(e1.scrutinee, e2.scrutinee) &&
      e1.branches.length === e2.branches.length &&
      e1.branches.every(({ pat }, i) => patEquals(pat, e2.branches[i].pat)) &&
      e1.branches.every(({ body }, i) => expEquals(body, e2.branches[i].body))
    )
  } else if (e1.tag === "quote" && e2.tag === "quote") {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === "let*" && e2.tag === "let*") {
    return (
      e1.bindings.length === e2.bindings.length &&
      e1.bindings.every(({ name }, i) => name === e2.bindings[i].name) &&
      e1.bindings.every(({ value }, i) =>
        expEquals(value, e2.bindings[i].value),
      ) &&
      expEquals(e1.body, e2.body)
    )
  } else if (e1.tag === "and" && e2.tag === "and") {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === "or" && e2.tag === "or") {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === "cond" && e2.tag === "cond") {
    return (
      e1.branches.length === e2.branches.length &&
      e1.branches.every(({ test }, i) =>
        expEquals(test, e2.branches[i].test),
      ) &&
      e1.branches.every(({ body }, i) => expEquals(body, e2.branches[i].body))
    )
  } else if (e1.tag === "section" && e2.tag === "section") {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === "report" && e2.tag === "report") {
    return expEquals(e1.exp, e2.exp)
  } else {
    return false
  }
}

export function stmtEquals(s1: Stmt, s2: Stmt): boolean {
  if (s1.tag === "import" && s2.tag === "import") {
    return s1.module === s2.module
  } else if (s1.tag === "define" && s2.tag === "define") {
    return s1.name === s2.name && expEquals(s1.value, s2.value)
  } else if (s1.tag === "display" && s2.tag === "display") {
    return expEquals(s1.value, s2.value)
  } else if (s1.tag === "stmtexp" && s2.tag === "stmtexp") {
    return expEquals(s1.expr, s2.expr)
  } else if (s1.tag === "struct" && s2.tag === "struct") {
    return (
      s1.name === s2.name &&
      s1.fields.length === s2.fields.length &&
      s1.fields.every((field, i) => field === s2.fields[i])
    )
  } else {
    return false
  }
}

import * as L from '../lpm'
import TextRenderer from '../lpm/renderers/text.js'

export interface Tagged {
  tag: string
}
export interface Node {
  range: L.Range
  // Set when this node was inserted by expanding a derived form (expansion.ts);
  // threaded through to the LPM op and back on raise so sugaring can recover the
  // derived form exactly. Undefined on parsed nodes. See L.Provenance.
  provenance?: L.Provenance
  // Source comments attached to this node (issue #304). Populated only on demand
  // (see attachComments in comments.ts) -- normal compilation leaves them unset,
  // so the compiler pays nothing. Structural equality ignores them.
  //   leading:  comments on their own line(s) immediately before the node.
  //   trailing: a comment on the same line, after the node.
  //   dangling: comments inside a form with no following child (e.g. before its
  //             closing paren, or the whole content of a comment-only program).
  leading?: Comment[]
  trailing?: Comment[]
  dangling?: Comment[]
}

/** A single line comment, tracked so docstrings can be reassembled from it. */
export interface Comment {
  line: string
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
//     | (lambda (x1 ... xk-1 & xk)
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
//     | (and e1 ... ek)
//     | (or e1 ... ek)
//     | (cond [e11 e12] ... [e1k e2k])
//     | #(e1 ... ek)
//
// s ::= e
//     | (import m)
//     | (import m x)     -- m imported under the qualified name (alias) x
//     | (define x e)
//     | <docstring>
//       (define x e)
//     | (display e)
//     | e
//
//     -- Sugared form
//     | (struct S (f1 ... fk))
//
// <docstring> ::= ;;; (fn x1 ... xk) -> typred
//                 ;;;   x1 : typred - <description>
//                 ;;;   ...
//                 ;;;   xk : typred - <description>
//                 ;;; @tag1 <description>
//                 ;;; ...
//                 ;;; @tagk <description>
//                 ;;; <description>
//
// typred ::= f | (f1 ... fk)

// An identifier: a name together with its source range. Because a variable
// reference and a pattern variable are structurally nothing more than an
// identifier (their node range always coincides with the identifier's), the
// Identifier node itself plays both roles -- it is a member of both the Exp
// and Pat unions (tag 'id'), and is also used directly in binder positions
// (lambda params, let bindings, define/struct names).
//
// `name` may be a one-level *qualified* name (`mod.member`) -- but only when the
// identifier is a variable *reference*. Binder positions reject qualified names
// (see lezer-bridge.ts), so a name in any binding/pattern/top-level slot is
// always simple. Use isQualifiedName/splitQualifiedName to inspect one.
export interface Identifier extends Tagged, Node {
  tag: 'id'
  name: string
}

/** The separator joining the two halves of a qualified name (`mod.member`). */
export const QUALIFIER_SEP = '.'

/** @returns whether `name` is a qualified reference of the form `mod.member` */
export function isQualifiedName(name: string): boolean {
  return name.includes(QUALIFIER_SEP)
}

/**
 * Splits a qualified `mod.member` name into its `qualifier` (the module alias)
 * and `member` halves. The grammar admits exactly one separator, so this splits
 * on the first (and only) one.
 */
export function splitQualifiedName(name: string): {
  qualifier: string
  member: string
} {
  const i = name.indexOf(QUALIFIER_SEP)
  return { qualifier: name.slice(0, i), member: name.slice(i + 1) }
}

///// Patterns /////

export interface PWild extends Tagged, Node {
  tag: 'pwild'
}
export interface PLit extends Tagged, Node {
  tag: 'plit'
  value: L.Value
}
export interface PCtor extends Tagged, Node {
  tag: 'pctor'
  name: Identifier
  args: Pat[]
}
export type Pat = PWild | Identifier | PLit | PCtor

///// Expressions /////

// Core Forms
export interface Lit extends Tagged, Node {
  tag: 'lit'
  value: L.Value
}
export interface App extends Tagged, Node {
  tag: 'app'
  head: Exp
  args: Exp[]
}
export interface Lam extends Tagged, Node {
  tag: 'lam'
  params: Identifier[]
  restParam?: Identifier
  body: Exp
}
export interface Let extends Tagged, Node {
  tag: 'let'
  bindings: { pat: Pat; value: Exp }[]
  body: Exp
}
export interface Begin extends Tagged, Node {
  tag: 'begin'
  exps: Exp[]
}
export interface If extends Tagged, Node {
  tag: 'if'
  guard: Exp
  ifB: Exp
  elseB: Exp
}
export interface Match extends Tagged, Node {
  tag: 'match'
  scrutinee: Exp
  branches: { pat: Pat; body: Exp }[]
}
export interface Quote extends Tagged, Node {
  tag: 'quote'
  value: L.Value
}
// Sugared Forms
export interface And extends Tagged, Node {
  tag: 'and'
  exps: Exp[]
}
export interface Or extends Tagged, Node {
  tag: 'or'
  exps: Exp[]
}
export interface Cond extends Tagged, Node {
  tag: 'cond'
  branches: { test: Exp; body: Exp }[]
}
// A Clojure-style anonymous function `#(body)` -- the `#` marker followed by a
// parenthesized expression. Expansion (expansion.ts) rewrites it to a `lambda`
// (tagged `provenance:'anon-fn'`) whose body is that expression verbatim and
// whose parameters come from the `%`, `%1`, ..., `%k`, `%&` identifiers
// referenced within; nothing downstream of expansion ever sees this node.
export interface AnonFn extends Tagged, Node {
  tag: 'anonfn'
  body: Exp
}
export type Exp =
  | Lit
  | Identifier
  | App
  | Lam
  | Let
  | Begin
  | If
  | Match
  | Quote
  | And
  | Or
  | Cond
  | AnonFn

///// Statements /////

// Core Forms
export interface Import extends Tagged, Node {
  tag: 'import'
  module: string
  // "builtin" for a bare identifier (e.g. (import prelude)), resolved
  // against the standard library; "file" for a quoted string (e.g.
  // (import "my-file.scm")), resolved against the user's file system. A
  // module name is a simple identifier or a quoted string, so the two import
  // kinds need not be disambiguated by probing existence at runtime.
  kind: 'builtin' | 'file'
  // An optional qualified name (alias): (import image img) binds the module
  // under `img`, and its exports are reachable only as `img.<name>` (never
  // injected unqualified into the top level). Absent for the one-argument
  // form (import image), which injects the module's names into global scope.
  alias?: string
}
export interface Define extends Tagged, Node {
  tag: 'define'
  name: Identifier
  value: Exp
  // N.B., raw, unparsed comments -- parsing them into a FunctionDoc is
  // deferred until something actually needs the documentation (the IDE's
  // live linter, or the query/example-tag feature), via
  // docstring.ts's parseFunctionDocFromComments. A malformed docstring is a
  // documentation-quality issue, not a reason to fail compiling otherwise-
  // valid code, so it shouldn't be parsed (and thus can't error) as part of
  // the main parse pass.
  docComments?: Comment[]
}
export interface Disp extends Tagged, Node {
  tag: 'display'
  value: Exp
}
export interface StmtExp extends Tagged, Node {
  tag: 'stmtexp'
  expr: Exp
}

// Sugared Forms
export interface Struct extends Tagged, Node {
  tag: 'struct'
  name: Identifier
  fields: Identifier[]
}

export type Stmt = Import | Define | Disp | StmtExp | Struct

///// Programs /////

export type Prog = Stmt[]
export interface ProgNode extends Node {
  tag: 'prog'
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
  return { tag: 'prog', body: prog, range }
}

///// Constructor Functions ////////////////////////////////////////////////////

// Identifiers -- also serve as variable references (Exp) and pattern
// variables (Pat), so mkId is the sole constructor for all three.
export const mkId = (name: string, range: L.Range = L.Range.none): Identifier => ({
  tag: 'id',
  name,
  range,
})

// Patterns (pat)
export const mkPWild = (range: L.Range = L.Range.none): PWild => ({
  tag: 'pwild',
  range,
})
export const mkPLit = (
  value: L.Value,
  range: L.Range = L.Range.none,
): PLit => ({ tag: 'plit', value, range })
export const mkPCtor = (
  name: Identifier,
  args: Pat[],
  range: L.Range = L.Range.none,
): PCtor => ({ tag: 'pctor', name, args, range })

// Expressions (exp)
// Omit `provenance` when unset so it is a truly optional key (absent, not
// `undefined`); keeps structural `toStrictEqual` comparisons against parsed
// nodes clean.
export const mkLit = (
  value: L.Value,
  range: L.Range = L.Range.none,
  provenance?: L.Provenance,
): Lit => ({
  tag: 'lit',
  value,
  range,
  ...(provenance !== undefined ? { provenance } : {}),
})
export const mkApp = (
  head: Exp,
  args: Exp[],
  range: L.Range = L.Range.none,
  provenance?: L.Provenance,
): App => ({
  tag: 'app',
  head,
  args,
  range,
  ...(provenance !== undefined ? { provenance } : {}),
})
export const mkLam = (
  params: Identifier[],
  body: Exp,
  range: L.Range = L.Range.none,
  restParam?: Identifier,
  provenance?: L.Provenance,
): Lam => ({
  tag: 'lam',
  params,
  body,
  range,
  restParam,
  ...(provenance !== undefined ? { provenance } : {}),
})
export const mkLet = (
  bindings: { pat: Pat; value: Exp }[],
  body: Exp,
  range: L.Range = L.Range.none,
  provenance?: L.Provenance,
): Let => ({
  tag: 'let',
  bindings,
  body,
  range,
  ...(provenance !== undefined ? { provenance } : {}),
})
export const mkBegin = (exps: Exp[], range: L.Range = L.Range.none): Begin => ({
  tag: 'begin',
  exps,
  range,
})
export const mkIf = (
  guard: Exp,
  ifB: Exp,
  elseB: Exp,
  range: L.Range = L.Range.none,
  provenance?: L.Provenance,
): If => ({
  tag: 'if',
  guard,
  ifB,
  elseB,
  range,
  ...(provenance !== undefined ? { provenance } : {}),
})
export const mkMatch = (
  scrutinee: Exp,
  branches: { pat: Pat; body: Exp }[],
  range: L.Range = L.Range.none,
): Match => ({ tag: 'match', scrutinee, branches, range })
export const mkQuote = (
  value: L.Value,
  range: L.Range = L.Range.none,
): Quote => ({ tag: 'quote', value, range })
export const mkAnd = (exps: Exp[], range: L.Range = L.Range.none): And => ({
  tag: 'and',
  exps,
  range,
})
export const mkOr = (exps: Exp[], range: L.Range = L.Range.none): Or => ({
  tag: 'or',
  exps,
  range,
})
export const mkCond = (
  branches: { test: Exp; body: Exp }[],
  range: L.Range = L.Range.none,
): Cond => ({ tag: 'cond', branches, range })
export const mkAnonFn = (
  body: Exp,
  range: L.Range = L.Range.none,
): AnonFn => ({ tag: 'anonfn', body, range })

// Statements (stmt)
export const mkImport = (
  module: string,
  kind: 'builtin' | 'file',
  range: L.Range = L.Range.none,
  alias?: string,
): Import => ({ tag: 'import', module, kind, range, ...(alias !== undefined ? { alias } : {}) })
export const mkDefine = (
  name: Identifier,
  value: Exp,
  range: L.Range = L.Range.none,
  docComments?: Comment[],
): Define => ({ tag: 'define', name, value, range, docComments })
export const mkDisp = (value: Exp, range: L.Range = L.Range.none): Disp => ({
  tag: 'display',
  value,
  range,
})
export const mkStmtExp = (
  expr: Exp,
  range: L.Range = L.Range.none,
): StmtExp => ({ tag: 'stmtexp', expr, range })
export const mkStruct = (
  name: Identifier,
  fields: Identifier[],
  range: L.Range = L.Range.none,
): Struct => ({ tag: 'struct', name, fields, range })

///// Query Functions //////////////////////////////////////////////////////////

function isTagged(v: unknown): v is Tagged {
  return typeof v === 'object' && v !== null && 'tag' in v
}

export function isPat(v: unknown): v is Pat {
  return isTagged(v) && ['pwild', 'id', 'plit', 'pctor'].includes(v.tag)
}

export function isExp(v: unknown): v is Exp {
  return (
    isTagged(v) &&
    [
      'lit',
      'id',
      'app',
      'lam',
      'let',
      'begin',
      'if',
      'match',
      'quote',
      'and',
      'or',
      'cond',
      'anonfn',
    ].includes(v.tag)
  )
}

export function isStmt(v: unknown): v is Stmt {
  return (
    isTagged(v) &&
    ['import', 'define', 'display', 'stmtexp', 'struct'].includes(v.tag)
  )
}

export const isStmtExp = (s: Stmt): s is StmtExp =>
  isTagged(s) && s.tag === 'stmtexp'

/**
 * Maps each qualified import's alias to its Import statement, so a qualified
 * reference `alias.member` can be resolved back to the module it names. The last
 * import wins if an alias is (erroneously) reused.
 */
export function qualifiedImportMap(prog: Prog): Map<string, Import> {
  const m = new Map<string, Import>()
  for (const s of prog) {
    if (s.tag === 'import' && s.alias !== undefined) {
      m.set(s.alias, s)
    }
  }
  return m
}

export const isVar = (e: Exp): e is Identifier => isTagged(e) && e.tag === 'id'

export const isApp = (e: Exp): e is App =>
  isTagged(e) && e.tag === 'app' && isExp(e.head)

export const isLam = (e: Exp): e is Lam => isTagged(e) && e.tag === 'lam'

export const isLit = (e: Exp): e is Lit => isTagged(e) && e.tag === 'lit'

///// Surface-syntax layout ////////////////////////////////////////////////////

// The surface syntax of every AST node is uniformly a tree of delimited groups
// over token and value leaves -- `(f a b)`, `[x 1]`, and `(let (...) body)` are
// all just groups, nested groups, and groups-of-groups. `Layout` captures that
// one structure so both backends walk it: layoutToString renders it to text and
// LayoutRenderer.vue renders it to DOM. Keeping a single description is what
// keeps the text and web renderings from drifting (previously hand-synced, #318)
// and is the natural place to later hang line-breaking/indentation for traces.
// A token's syntactic role, used only for syntax highlighting: LayoutRenderer
// maps it to a CSS class; layoutToString ignores it. Identifiers/punctuation are
// left untagged (default color). Extend as finer highlighting is wanted.
export type Highlight = 'keyword'

export type Layout =
  // A literal token: a keyword, an identifier, "&", "_". `hl` tags its
  // highlight role (see Highlight); absent = default, unhighlighted text.
  | { kind: 'tok'; text: string; hl?: Highlight }
  // A runtime value (a lit/quote/plit payload) rendered by the value renderer --
  // TextRenderer for text, ValueRenderer for the web (so images, lists, etc.
  // substituted into a trace render correctly). Highlighted by runtime type.
  | { kind: 'val'; value: L.Value }
  // A delimited group whose children are space-separated.
  | { kind: 'group'; delim: 'paren' | 'bracket'; children: Layout[] }
  // A "#" written immediately (no space) before its child -- the anonymous
  // function `#(body)`, whose child is the parenthesized body layout.
  | { kind: 'hash'; child: Layout }

const tok = (text: string): Layout => ({ kind: 'tok', text })
const kw = (text: string): Layout => ({ kind: 'tok', text, hl: 'keyword' })
const val = (value: L.Value): Layout => ({ kind: 'val', value })
const parens = (children: Layout[]): Layout => ({
  kind: 'group',
  delim: 'paren',
  children,
})
const brackets = (children: Layout[]): Layout => ({
  kind: 'group',
  delim: 'bracket',
  children,
})
const hash = (child: Layout): Layout => ({ kind: 'hash', child })

export function patToLayout(pat: Pat): Layout {
  switch (pat.tag) {
    case 'pwild':
      return tok('_')
    case 'id':
      return tok(pat.name)
    case 'plit':
      return val(pat.value)
    case 'pctor':
      return parens([tok(pat.name.name), ...pat.args.map(patToLayout)])
  }
}

export function expToLayout(e: Exp): Layout {
  switch (e.tag) {
    case 'lit':
      return val(e.value)
    case 'id':
      return tok(e.name)
    case 'app':
      return parens([expToLayout(e.head), ...e.args.map(expToLayout)])
    case 'lam': {
      const params = e.params.map((p) => tok(p.name))
      if (e.restParam) params.push(tok('&'), tok(e.restParam.name))
      return parens([kw('lambda'), parens(params), expToLayout(e.body)])
    }
    case 'let':
      return parens([
        kw('let'),
        parens(
          e.bindings.map(({ pat, value }) =>
            brackets([patToLayout(pat), expToLayout(value)]),
          ),
        ),
        expToLayout(e.body),
      ])
    case 'begin':
      return parens([kw('begin'), ...e.exps.map(expToLayout)])
    case 'if':
      return parens([
        kw('if'),
        expToLayout(e.guard),
        expToLayout(e.ifB),
        expToLayout(e.elseB),
      ])
    case 'match':
      return parens([
        kw('match'),
        expToLayout(e.scrutinee),
        ...e.branches.map(({ pat, body }) =>
          brackets([patToLayout(pat), expToLayout(body)]),
        ),
      ])
    case 'quote':
      return parens([kw('quote'), val(e.value)])
    case 'and':
      return parens([kw('and'), ...e.exps.map(expToLayout)])
    case 'or':
      return parens([kw('or'), ...e.exps.map(expToLayout)])
    case 'cond':
      return parens([
        kw('cond'),
        ...e.branches.map(({ test, body }) =>
          brackets([expToLayout(test), expToLayout(body)]),
        ),
      ])
    case 'anonfn':
      // #(body): "#" then the body's own parenthesized layout. An empty #()
      // (whose body parsed to the `null` literal) is rendered literally.
      return e.body.tag === 'lit' && e.body.value === null
        ? tok('#()')
        : hash(expToLayout(e.body))
  }
}

export function stmtToLayout(s: Stmt): Layout {
  switch (s.tag) {
    case 'import':
      return parens([
        kw('import'),
        tok(s.kind === 'file' ? JSON.stringify(s.module) : s.module),
        ...(s.alias !== undefined ? [tok(s.alias)] : []),
      ])
    case 'define':
      return parens([kw('define'), tok(s.name.name), expToLayout(s.value)])
    case 'display':
      return parens([kw('display'), expToLayout(s.value)])
    case 'stmtexp':
      return expToLayout(s.expr)
    case 'struct':
      return parens([
        kw('struct'),
        tok(s.name.name),
        parens(s.fields.map((f) => tok(f.name))),
      ])
  }
}

///// Stringifying Functions ///////////////////////////////////////////////////

const DELIMS = { paren: ['(', ')'], bracket: ['[', ']'] } as const

/** Render a {@link Layout} to text: the text backend of the surface syntax. */
export function layoutToString(l: Layout): string {
  switch (l.kind) {
    case 'tok':
      return l.text
    case 'val':
      return TextRenderer.render(l.value)
    case 'group': {
      const [open, close] = DELIMS[l.delim]
      return `${open}${l.children.map(layoutToString).join(' ')}${close}`
    }
    case 'hash':
      return `#${layoutToString(l.child)}`
  }
}

export const patToString = (pat: Pat): string => layoutToString(patToLayout(pat))
export const expToString = (e: Exp): string => layoutToString(expToLayout(e))
export const stmtToString = (s: Stmt): string => layoutToString(stmtToLayout(s))

export function progToString(p: Prog): string {
  return p.map(stmtToString).join('\n')
}

TextRenderer.registerCustomRenderer(isPat, (v) => patToString(v as Pat))
TextRenderer.registerCustomRenderer(isExp, (v) => expToString(v as Exp))
TextRenderer.registerCustomRenderer(isStmt, (v) => stmtToString(v as Stmt))

///// Equality /////////////////////////////////////////////////////////////////

export function patEquals(p1: Pat, p2: Pat): boolean {
  if (p1.tag === 'pwild' && p2.tag === 'pwild') {
    return true
  } else if (p1.tag === 'id' && p2.tag === 'id') {
    return p1.name === p2.name
  } else if (p1.tag === 'plit' && p2.tag === 'plit') {
    return L.equals(p1.value, p2.value)
  } else if (p1.tag === 'pctor' && p2.tag === 'pctor') {
    return (
      p1.name.name === p2.name.name &&
      p1.args.length === p2.args.length &&
      p1.args.every((arg, i) => patEquals(arg, p2.args[i]))
    )
  } else {
    return false
  }
}

export function expEquals(e1: Exp, e2: Exp): boolean {
  if (e1.tag === 'lit' && e2.tag === 'lit') {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === 'id' && e2.tag === 'id') {
    return e1.name === e2.name
  } else if (e1.tag === 'app' && e2.tag === 'app') {
    return (
      expEquals(e1.head, e2.head) &&
      e1.args.length === e2.args.length &&
      e1.args.every((arg, i) => expEquals(arg, e2.args[i]))
    )
  } else if (e1.tag === 'lam' && e2.tag === 'lam') {
    return (
      e1.params.length === e2.params.length &&
      e1.params.every((param, i) => param.name === e2.params[i].name) &&
      e1.restParam?.name === e2.restParam?.name &&
      expEquals(e1.body, e2.body)
    )
  } else if (e1.tag === 'let' && e2.tag === 'let') {
    return (
      e1.bindings.length === e2.bindings.length &&
      e1.bindings.every(({ pat }, i) => patEquals(pat, e2.bindings[i].pat)) &&
      e1.bindings.every(({ value }, i) =>
        expEquals(value, e2.bindings[i].value),
      ) &&
      expEquals(e1.body, e2.body)
    )
  } else if (e1.tag === 'begin' && e2.tag === 'begin') {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === 'if' && e2.tag === 'if') {
    return (
      expEquals(e1.guard, e2.guard) &&
      expEquals(e1.ifB, e2.ifB) &&
      expEquals(e1.elseB, e2.elseB)
    )
  } else if (e1.tag === 'match' && e2.tag === 'match') {
    return (
      expEquals(e1.scrutinee, e2.scrutinee) &&
      e1.branches.length === e2.branches.length &&
      e1.branches.every(({ pat }, i) => patEquals(pat, e2.branches[i].pat)) &&
      e1.branches.every(({ body }, i) => expEquals(body, e2.branches[i].body))
    )
  } else if (e1.tag === 'quote' && e2.tag === 'quote') {
    return L.equals(e1.value, e2.value)
  } else if (e1.tag === 'and' && e2.tag === 'and') {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === 'or' && e2.tag === 'or') {
    return (
      e1.exps.length === e2.exps.length &&
      e1.exps.every((exp, i) => expEquals(exp, e2.exps[i]))
    )
  } else if (e1.tag === 'cond' && e2.tag === 'cond') {
    return (
      e1.branches.length === e2.branches.length &&
      e1.branches.every(({ test }, i) =>
        expEquals(test, e2.branches[i].test),
      ) &&
      e1.branches.every(({ body }, i) => expEquals(body, e2.branches[i].body))
    )
  } else if (e1.tag === 'anonfn' && e2.tag === 'anonfn') {
    return expEquals(e1.body, e2.body)
  } else {
    return false
  }
}

export function stmtEquals(s1: Stmt, s2: Stmt): boolean {
  if (s1.tag === 'import' && s2.tag === 'import') {
    return (
      s1.module === s2.module && s1.kind === s2.kind && s1.alias === s2.alias
    )
  } else if (s1.tag === 'define' && s2.tag === 'define') {
    return s1.name.name === s2.name.name && expEquals(s1.value, s2.value)
  } else if (s1.tag === 'display' && s2.tag === 'display') {
    return expEquals(s1.value, s2.value)
  } else if (s1.tag === 'stmtexp' && s2.tag === 'stmtexp') {
    return expEquals(s1.expr, s2.expr)
  } else if (s1.tag === 'struct' && s2.tag === 'struct') {
    return (
      s1.name.name === s2.name.name &&
      s1.fields.length === s2.fields.length &&
      s1.fields.every((field, i) => field.name === s2.fields[i].name)
    )
  } else {
    return false
  }
}

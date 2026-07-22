import type { Range } from "./range.js"
import type { App, Var } from "../scheme/ast.js"

// N.B., these types describe the *shape* of a parsed docstring -- Module
// (lang.ts) needs this shape to type a binding's optional doc field without
// falling back to `unknown`. The actual parsing lives in
// scheme/docstring/docstring.ts (and its siblings), since it depends on the
// scheme-level parser (tokenizeAndParse, isVar/isApp, etc.); only the type
// imports here are from scheme/ast.js, which TypeScript erases entirely
// (this file has no runtime code of its own), so this doesn't create a real
// runtime dependency from lpm/ onto scheme/.

/** A tag (e.g. `@example`) attached to the end of a docstring. */
export interface DocTag<T = unknown> {
  tag: string
  contents: T
  range: Range
}

interface SimplePred extends Var {
  range: Range
}
export interface ComplexPred extends App {
  head: Var
  args: Pred[]
  range: Range
}
/** A contract predicate: either a simple identifier (`number?`) or an
 * application of one (`(or number? string?)`). */
export type Pred = SimplePred | ComplexPred

/** An application of a variable to variable arguments, e.g. `(f x y)`. */
export interface VarApp extends App {
  head: Var
  args: Var[]
}

/** A docstring's `(fn x1 ... xk) -> typred` signature line. */
export interface Signature {
  function: VarApp
  predicate: Pred
  range: Range
}

/** A single documented parameter: `xi : typred` plus its description. */
export interface Param {
  name: string
  predicate: Pred
  description?: string
  range: Range
}

/** A fully-parsed docstring, as attached to a module binding. */
export interface FunctionDoc {
  signature: Signature
  params: Param[]
  description: string
  tags: DocTag[]
  range: Range
}

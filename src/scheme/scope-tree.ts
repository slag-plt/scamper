import * as A from './ast.js'
import { Range } from '../lpm/range.js'
import * as SymbolDB from './symbol-db.js'

/** A collection of symbol tables, organized by nested scopes. */
export class ScopeTree {
  /**
   * The range of this scope. Must be contained within the range of the parent scope (if any)
   * and its children must be entirely contained within this range.
   */
  private range: Range
  /** The parent scope or undefined if this is the top-level (global) scope */
  private parent: ScopeTree | undefined
  /** The identifiers bound by this current scope */
  private identifiers: A.Identifier[]
  /** The children scopes, ordered by (non-overlapping) ranges */
  private children: ScopeTree[]

  /**
   * Constructs a new scope tree. Note that the parent link of this scope is set
   * when it this scope tree is made the child of another.
   * @param identifiers the identifiers contained in this scope (but not subscopes)
   * @param children the child scopes of this scope, ordered by (non-overlapping) ranges
   */
  constructor (range: Range, identifiers: A.Identifier[], children: ScopeTree[]) {
    this.range = range
    this.parent = undefined
    this.children = children
    this.identifiers = identifiers

    // N.B., ensure that parent (back links) are set
    for (const child of this.children) {
      child.parent = this
    }
  }

  /**
   * @return the innermost scope that is wholly encompassed by the given range or
   *         undefined if no such scope exists. 
   */
  public getInnermostScope (range: Range): ScopeTree | undefined {
    // If range is not contained within this scope, then bail
    if (!this.range.containsRange(range)) {
      return undefined
    } else {
      // If it is, try to find a child scope that contains this range
      for (const child of this.children) {
        const innermost = child.getInnermostScope(range)
        if (innermost !== undefined) {
          return innermost
        }
      }
    }
    // Otherwise, this is the innermost scope that contains the range
    return this
  }

  /**
   * @return all the identifiers visible in this scope in order of local-to-global scope.
   */
  public getVisibleIdentifiers (): A.Identifier[] {
    return [...this.identifiers, ...this.parent?.getVisibleIdentifiers() ?? []]
  }
}

/***** Scope tree creation from ASTs ******************************************/

/**
 * @returns the pattern variables (binders) introduced by the pattern
 */
function patternIdentifiers(pat: A.Pat): A.Identifier[] {
  switch (pat.tag) {
    case 'id':
      return [pat]
    // N.B., a constructor's head is a reference, not a binder -- only its
    // arguments contribute pattern variables.
    case 'pctor':
      return pat.args.flatMap(patternIdentifiers)
    case 'pwild':
    case 'plit':
      return []
  }
}

/**
 * A let* telescopes like nested lets -- each binding scopes the later values and body.
 * @returns the scopes it introduces
 */
function letStarScopes(exp: A.LetS): ScopeTree[] {
  const { bindings, body } = exp
  if (bindings.length === 0) {
    return scopesInExp(body)
  }
  const build = (i: number): ScopeTree => {
    const isLast = i === bindings.length - 1
    // Binding i is visible from the next value (or the body, if last) through
    // the end of the body.
    const next = isLast ? body : bindings[i + 1].value
    const range = Range.union(next.range, body.range)
    const children = isLast
      ? scopesInExp(body)
      : [...scopesInExp(bindings[i + 1].value), build(i + 1)]
    return new ScopeTree(range, patternIdentifiers(bindings[i].pat), children)
  }
  return [...scopesInExp(bindings[0].value), build(0)]
}

/**
 * @returns the child scopes introduced within the expression, in source order, for the enclosing scope
 */
function scopesInExp(exp: A.Exp): ScopeTree[] {
  // N.B., binding forms build a ScopeTree; every other form is transparent and
  // just forwards its sub-expressions' scopes. Sub-expressions are visited
  // left-to-right (source order), so the result is already ordered by
  // non-overlapping range as ScopeTree requires.
  switch (exp.tag) {
    // Leaves: no sub-expressions, no scopes.
    case 'id':
    case 'lit':
    case 'quote':
      return []

    // Transparent forms: forward the scopes of every sub-expression.
    case 'app':
      return [exp.head, ...exp.args].flatMap(scopesInExp)
    case 'if':
      return [exp.guard, exp.ifB, exp.elseB].flatMap(scopesInExp)
    case 'begin':
    case 'and':
    case 'or':
    case 'section':
      // N.B., a section's `_` holes only become bound parameters after
      // expansion, so at this (surface) level a section binds nothing.
      return exp.exps.flatMap(scopesInExp)
    case 'cond':
      return exp.branches.flatMap((b) => [
        ...scopesInExp(b.test),
        ...scopesInExp(b.body),
      ])

    // Binding forms.
    case 'lam': {
      // Parameters (and the rest parameter, if any) are visible throughout the
      // whole lambda form.
      const bound = exp.restParam
        ? [...exp.params, exp.restParam]
        : exp.params
      return [new ScopeTree(exp.range, bound, scopesInExp(exp.body))]
    }
    case 'let': {
      // Bindings are visible in the body only; the values are evaluated in the
      // enclosing scope, so their scopes bubble up as siblings of the let.
      const valueScopes = exp.bindings.flatMap((b) => scopesInExp(b.value))
      const letScope = new ScopeTree(
        exp.body.range,
        exp.bindings.flatMap((b) => patternIdentifiers(b.pat)),
        scopesInExp(exp.body),
      )
      return [...valueScopes, letScope]
    }
    case 'let*':
      return letStarScopes(exp)
    case 'match': {
      // The scrutinee is evaluated in the enclosing scope; each branch's
      // pattern variables are visible only within that branch's body.
      const scrutineeScopes = scopesInExp(exp.scrutinee)
      const branchScopes = exp.branches.map(
        (b) =>
          new ScopeTree(
            b.body.range,
            patternIdentifiers(b.pat),
            scopesInExp(b.body),
          ),
      )
      return [...scrutineeScopes, ...branchScopes]
    }
  }
}

/**
 * @returns the scope tree for the program, rooted at the global scope
 */
export async function makeScopeTreeFromProgram(
  prog: A.Prog,
): Promise<ScopeTree> {
  // Ensure every imported file's symbols are in the DB before we build the tree.
  await SymbolDB.loadTransitiveImports(prog)
  const identifiers: A.Identifier[] = [
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    ...SymbolDB.get('runtime')!,
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    ...SymbolDB.get('prelude')!,
  ]
  const children: ScopeTree[] = []

  for (const stmt of prog) {
    switch (stmt.tag) {
      case 'define':
        identifiers.push(stmt.name)
        children.push(...scopesInExp(stmt.value))
        break
      case 'struct':
        // The constructor name is bound directly. The derived predicate
        // (`${name}?`) and field accessors (`${name}-${field}`) are
        // synthesized during expansion and have no identifier node here.
        // TODO: surface those generated bindings once we can attach source
        // ranges to derived identifiers.
        identifiers.push(stmt.name)
        break
      case 'import':
        // TODO: do we need to scope imports? Are later imports visible in earlier code?
        SymbolDB.get(stmt.module)?.forEach((id) => identifiers.push(id))
        break
      case 'display':
        children.push(...scopesInExp(stmt.value))
        break
      case 'stmtexp':
        children.push(...scopesInExp(stmt.expr))
        break
    }
  }
  return new ScopeTree(Range.union(...prog.map((s) => s.range)), identifiers, children)
}
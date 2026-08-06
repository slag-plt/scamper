import * as L from '../lpm'
import { ICE } from '../lpm'
import { ScamperDiagnostic, mkDiagnostic } from './diagnostic.js'
import * as A from './ast.js'
import {
  ComplexPred,
  FunctionDoc,
  Pred,
  parseFunctionDocFromComments,
} from './docstring/docstring'
import * as SymbolDB from './symbol-db.js'

/**
 * Reports a "duplicate variable" diagnostic for each name that appears more
 * than once in a binding list (lambda parameters, let bindings).
 * @param range the source range to attach to any diagnostic
 */
function checkDuplicateVars(
  diagnostics: ScamperDiagnostic[],
  vars: string[],
  range: L.Range,
) {
  const seen = new Set<string>()
  for (const v of vars) {
    if (seen.has(v)) {
      diagnostics.push(
        mkDiagnostic(
          'Scope',
          'warning',
          `Duplicate variable '${v}' encountered in binding list`,
          range,
        ),
      )
    }
    seen.add(v)
  }
}

/**
 * Scope-checks a match pattern, adding its binders to `locals` and reporting
 * any variable bound more than once within the pattern.
 */
function scopeCheckPat(
  diagnostics: ScamperDiagnostic[],
  locals: Set<string>,
  p: A.Pat,
) {
  switch (p.tag) {
    case 'id': {
      if (locals.has(p.name)) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `Duplicate binding variable '${p.name}' encountered in pattern`,
            p.range,
          ),
        )
      } else {
        locals.add(p.name)
      }
      return
    }

    case 'pwild':
      return
    case 'plit':
      return

    case 'pctor':
    case 'pvec': {
      p.args.forEach((p) => {
        scopeCheckPat(diagnostics, locals, p)
      })
      return
    }
  }
}

/**
 * A qualified-import table: each imported module's alias (the qualified name it
 * was given, e.g. `img` from `(import image img)`) mapped to the source module
 * it names and the set of binding names it exports. Aliases live in their own
 * namespace, distinct from value bindings -- `img` names the module for
 * `img.member` references and never collides with a `(define img ...)`.
 */
type QualifiedModules = Map<string, { module: string; exports: Set<string> }>

/**
 * Scope-checks an expression, reporting references to names bound in neither
 * `locals` (lexical scope) nor `globals` (top level), plus duplicate binders. A
 * qualified reference (`mod.member`) is resolved against `qualified` instead.
 */
function scopeCheckExp(
  diagnostics: ScamperDiagnostic[],
  globals: string[],
  qualified: QualifiedModules,
  locals: string[],
  e: A.Exp,
) {
  switch (e.tag) {
    case 'id': {
      if (A.isQualifiedName(e.name)) {
        // A qualified reference resolves only through its module's alias -- never
        // against locals/globals (a binder can never be qualified).
        const { qualifier, member } = A.splitQualifiedName(e.name)
        const mod = qualified.get(qualifier)
        if (mod === undefined) {
          diagnostics.push(
            mkDiagnostic(
              'Scope',
              'warning',
              `No imported module is qualified as '${qualifier}'`,
              e.range,
            ),
          )
        } else if (!mod.exports.has(member)) {
          diagnostics.push(
            mkDiagnostic(
              'Scope',
              'warning',
              `Module '${qualifier}' (${mod.module}) has no exported binding '${member}'`,
              e.range,
            ),
          )
        }
        return
      }
      if (!locals.includes(e.name) && !globals.includes(e.name)) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `Undefined variable '${e.name}'`,
            e.range,
          ),
        )
      }
      return
    }

    case 'lit':
      return

    case 'app': {
      scopeCheckExp(diagnostics, globals, qualified, locals, e.head)
      e.args.forEach((e) => {
        scopeCheckExp(diagnostics, globals, qualified, locals, e)
      })
      return
    }

    case 'lam': {
      // N.B., do we want to warn in the case of shadowed variables?
      const allParams = (
        e.restParam ? [...e.params, e.restParam] : e.params
      ).map((p) => p.name)
      checkDuplicateVars(diagnostics, allParams, e.range)
      scopeCheckExp(diagnostics, globals, qualified, [...locals, ...allParams], e.body)
      return
    }
    case 'let': {
      // letrec: every binder is in scope throughout -- across all binding
      // values and the body. A forward reference to a not-yet-evaluated binding
      // is an eager-evaluation *runtime* error, not a scope error, so it is not
      // flagged here. A name bound by more than one binding has no single
      // letrec slot, so it is a hard error.
      const bindingVars = new Set<string>()
      e.bindings.forEach((b) => {
        const patVars = new Set<string>()
        scopeCheckPat(diagnostics, patVars, b.pat)
        for (const v of patVars) {
          if (bindingVars.has(v)) {
            diagnostics.push(
              mkDiagnostic('Scope', 'error', `Duplicate binding '${v}' in let`, e.range),
            )
          }
          bindingVars.add(v)
        }
      })
      const scope = [...locals, ...bindingVars]
      e.bindings.forEach((b) => {
        scopeCheckExp(diagnostics, globals, qualified, scope, b.value)
      })
      scopeCheckExp(diagnostics, globals, qualified, scope, e.body)
      return
    }
    case 'if': {
      scopeCheckExp(diagnostics, globals, qualified, locals, e.guard)
      scopeCheckExp(diagnostics, globals, qualified, locals, e.ifB)
      scopeCheckExp(diagnostics, globals, qualified, locals, e.elseB)
      return
    }
    case 'match': {
      scopeCheckExp(diagnostics, globals, qualified, locals, e.scrutinee)
      e.branches.forEach((b) => {
        const bindingVars = new Set<string>()
        scopeCheckPat(diagnostics, bindingVars, b.pat)
        scopeCheckExp(diagnostics, globals, qualified, [...locals, ...bindingVars], b.body)
      })
      return
    }
    default:
      throw new ICE('scopeCheckExp', `Non-core expression encountered ${e.tag}`)
  }
}

// TODO: test this
/**
 * Scope-checks a docstring predicate, reporting a "Docstring" warning if it
 * names a global that is not defined.
 */
function scopeCheckPred(
  diagnostics: ScamperDiagnostic[],
  predicate: Pred,
  globals: string[],
) {
  if (A.isVar(predicate)) {
    if (!globals.includes(predicate.name)) {
      diagnostics.push(
        mkDiagnostic('Docstring', 'warning', `Undefined predicate "${predicate.name}"`,
          predicate.range,
        ),
      )
    }
  } else {
    scopeCheckComplexPred(diagnostics, predicate, globals)
  }
}

// TODO: test this
/** Scope-checks an applied docstring predicate and each of its arguments. */
function scopeCheckComplexPred(
  diagnostics: ScamperDiagnostic[],
  { head: { name }, args, range }: ComplexPred,
  globals: string[],
) {
  if (!globals.includes(name)) {
    diagnostics.push(
      mkDiagnostic('Docstring', 'warning', `Undefined predicate "${name}"`, range),
    )
  }
  for (const arg of args) {
    scopeCheckPred(diagnostics, arg, globals)
  }
}

// example function doc + function definition combo
// ;;; (append lst val) -> list?
// ;;;   lst : list?
// ;;;     The list to append to.
// ;;;   val : any
// ;;; Appends val to lst and returns the resulting list.
// ;;; @example (append (list 1 2 3) 4) -> (list 1 2 3 4)
// ;;; @tag list ...
// (define append
//   (lambda (lst val) ...))
// TODO: test this
/**
 * Scope-checks a definition's already-parsed docstring against the function it
 * documents, reporting mismatches (wrong parameter names, undefined predicates,
 * missing descriptions) as "Docstring" warnings.
 *
 * N.B., takes the already-parsed `doc` rather than reading it off the Define,
 * since docstring parsing can fail and that failure is handled by the caller;
 * everything reported here is a *semantic* mismatch between a valid docstring
 * and its function, not a real scope error.
 */
function scopeCheckFunctionDoc(
  diagnostics: ScamperDiagnostic[],
  doc: FunctionDoc,
  { name: nameId, value }: A.Define,
  globals: string[],
): void {
  const name = nameId.name
  if (!A.isLam(value)) {
    // can't attach function docs onto non-function definitions
    diagnostics.push(
      mkDiagnostic('Docstring', 'warning', 'Function docstring attached to non-function definition',
        doc.range,
      ),
    )
    return
  }

  const paramNames = value.params.map((p) => p.name)
  const {
    signature: {
      function: {
        head: { name: docName },
        args,
      },
      predicate,
      range: sigRange,
    },
    params: docParamDescriptions,
    // TODO: we don't scope check tags for now
    // tags,
    range: docRange,
  } = doc
  const docParams = [...args.map((v) => v.name)]

  // (append...
  if (name !== docName) {
    diagnostics.push(
      mkDiagnostic('Docstring', 'warning', `Docstring function name "${docName}" does not match defined name "${name}"`,
        sigRange,
      ),
    )
    // this is not a catastrophic error, continue parsing
  }

  // ... lst val)...
  for (const param of paramNames) {
    const nextDocParam = docParams.shift()
    if (nextDocParam === undefined) {
      diagnostics.push(
        mkDiagnostic('Docstring', 'warning', `Expected function parameter "${param}" to be defined in docstring signature`,
          sigRange,
        ),
      )
      continue
    }
    if (param !== nextDocParam) {
      diagnostics.push(
        mkDiagnostic('Docstring', 'warning', `Function signature defines parameter "${param}" in this position but docstring signature instead defines "${nextDocParam}"`,
          sigRange,
        ),
      )
    }
  }
  // don't check for remaining parameters, docstring param description check will get that

  // ... -> list?...
  scopeCheckPred(diagnostics, predicate, globals)

  // ...lst : list?... (param descriptions)
  const paramWasChecked = new Map<string, boolean>(
    [...paramNames].map((p) => [p, false]),
  )
  for (const {
    name: pName,
    predicate: pPred,
    range: pRange,
  } of docParamDescriptions) {
    if (!paramNames.includes(pName)) {
      diagnostics.push(
        mkDiagnostic('Docstring', 'warning', `Docstring describes unknown function parameter "${pName}"`,
          pRange,
        ),
      )
    }
    paramWasChecked.set(pName, true)
    scopeCheckPred(diagnostics, pPred, globals)
  }
  for (const [pName, wasChecked] of paramWasChecked) {
    if (wasChecked) {
      continue
    }
    diagnostics.push(
      mkDiagnostic('Docstring', 'warning', `Description of function parameter "${pName}" missing`,
        docRange,
      ),
    )
  }
  // TODO: validate @example calls the documented function with the correct arity
  // TODO: validate @example results satisfy the signature return predicate
  return
}

/**
 * Resolves an import to the identifiers it exports, reporting a diagnostic (and
 * returning undefined) if the module is an unknown built-in, a missing file, or
 * a file that failed to parse.
 */
async function resolveImport(
  diagnostics: ScamperDiagnostic[],
  s: A.Import,
): Promise<A.Identifier[] | undefined> {
  if (s.kind === 'builtin') {
    const mod = SymbolDB.get(s.module)
    if (mod === undefined) {
      diagnostics.push(
        mkDiagnostic(
          'Scope',
          'warning',
          `No such built-in library: '${s.module}'`,
          s.range,
        ),
      )
    }
    return mod
  }

  // File import. N.B., import '../fs' lazily -- a static import would pull the
  // OPFS implementation into this module's (widely-imported) graph and disturb
  // tests that mock the file system (see symbol-db.ts).
  const { getFS } = await import('../fs')
  let exists: boolean
  try {
    exists = await getFS().fileExists(s.module)
  } catch (e) {
    // The host can refuse a name outright -- notably one that reaches outside
    // the working directory (#340). Report its complaint as a diagnostic rather
    // than letting it abort the whole check.
    diagnostics.push(
      mkDiagnostic(
        'Scope',
        'warning',
        e instanceof Error ? e.message : String(e),
        s.range,
      ),
    )
    return undefined
  }
  if (!exists) {
    diagnostics.push(
      mkDiagnostic('Scope', 'warning', `File '${s.module}' does not exist`, s.range),
    )
    return undefined
  }
  // Imported files' symbols were loaded into the DB before scope checking (see
  // SymbolDB.loadTransitiveImports). A missing DB entry for a file that exists
  // means it failed to parse.
  const mod = SymbolDB.get(s.module)
  if (mod === undefined) {
    diagnostics.push(
      mkDiagnostic('Scope', 'warning', `Could not load module '${s.module}'`, s.range),
    )
  }
  return mod
}

/**
 * First pass over the top level: records every binding a statement introduces
 * (a define's name, or an import's exported names) into `globals`, so that all
 * top-level definitions are mutually visible regardless of their order in the
 * program. This matches Racket module semantics -- every module-level
 * definition and import shares one mutually-recursive scope covering the whole
 * body -- so top-level mutual recursion and forward references resolve.
 *
 * It also resolves imports (reporting a missing / unparseable / unknown module)
 * and flags name collisions. A collision between two *user-introduced* names --
 * define/define, define/import, or import/import -- is reported symmetrically,
 * regardless of order (Racket: "an identifier can be either imported or defined
 * ... but not both"). `sources` maps each user-introduced name to what
 * introduced it (`null` for a define, else the module name), so re-importing
 * the same module is idempotent and a library import that merely re-binds a
 * standard-library name is not spuriously flagged.
 */
async function collectTopLevelBindings(
  diagnostics: ScamperDiagnostic[],
  globals: string[],
  qualified: QualifiedModules,
  sources: Map<string, string | null>,
  s: A.Stmt,
): Promise<void> {
  switch (s.tag) {
    case 'import': {
      const ids = await resolveImport(diagnostics, s)
      if (ids === undefined) {
        return
      }
      if (s.alias !== undefined) {
        // Qualified import: the module's names are reachable only as
        // `alias.member`, never injected unqualified into `globals`. Register
        // the alias -> exports mapping (in its own namespace, so it never
        // collides with a define/value name). Re-importing the same module
        // under the same alias is idempotent; a different module under an
        // in-use alias is a collision.
        const prev = qualified.get(s.alias)
        if (prev !== undefined && prev.module !== s.module) {
          diagnostics.push(
            mkDiagnostic(
              'Scope',
              'warning',
              `Qualified name '${s.alias}' is already bound to module '${prev.module}'`,
              s.range,
            ),
          )
        } else if (prev === undefined) {
          qualified.set(s.alias, {
            module: s.module,
            exports: new Set(ids.map((id) => id.name)),
          })
        }
        return
      }
      for (const { name } of ids) {
        const prev = sources.get(name)
        if (prev !== undefined && prev !== s.module) {
          // Already introduced by a define (null) or a different module.
          diagnostics.push(
            mkDiagnostic(
              'Scope',
              'warning',
              `Global variable '${name}' is already defined`,
              s.range,
            ),
          )
        } else if (prev === undefined) {
          if (!globals.includes(name)) {
            globals.push(name)
          }
          sources.set(name, s.module)
        }
        // prev === s.module: the same module re-imported; idempotent, skip.
      }
      return
    }

    case 'define': {
      const name = s.name.name
      if (globals.includes(name)) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `Global variable '${name}' is already defined`,
            s.range,
          ),
        )
      } else {
        globals.push(name)
      }
      // Mark as user-introduced so a later import of the same name collides.
      sources.set(name, null)
      return
    }

    case 'export':
    case 'display':
    case 'stmtexp':
      // Introduces no top-level binding. (An export's names are validated in
      // the second pass, once every top-level binding has been collected.)
      return

    default:
      throw new ICE(
        'collectTopLevelBindings',
        `Non-core statement encountered ${s.tag}`,
      )
  }
}

/**
 * Second pass over the top level: scope-checks each statement's bodies (and a
 * define's docstring) against the fully-populated `globals`.
 */
function scopeCheckStmtBodies(
  diagnostics: ScamperDiagnostic[],
  globals: string[],
  qualified: QualifiedModules,
  sources: Map<string, string | null>,
  s: A.Stmt,
): void {
  switch (s.tag) {
    case 'import':
      return

    case 'export': {
      // A module can only export names it defines itself (`sources` records a
      // define as null; an import records its module name). Re-exporting an
      // imported or standard-library name is a no-op at runtime, and exporting
      // an unbound name is a typo.
      for (const name of s.names) {
        if (sources.get(name.name) === null) {
          continue
        }
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            globals.includes(name.name)
              ? `Cannot export '${name.name}': it is not defined in this module`
              : `Exporting undefined variable '${name.name}'`,
            name.range,
          ),
        )
      }
      return
    }

    case 'define': {
      scopeCheckExp(diagnostics, globals, qualified, [], s.value)
      if (s.docComments) {
        // A malformed docstring is collected as a "Docstring" warning, the
        // same treatment as a semantic mismatch -- not a real scope error.
        const { doc, diagnostics: docDiagnostics } =
          parseFunctionDocFromComments(s.docComments)
        diagnostics.push(...docDiagnostics)
        if (doc) {
          scopeCheckFunctionDoc(diagnostics, doc, s, globals)
        }
      }
      return
    }

    case 'display':
      scopeCheckExp(diagnostics, globals, qualified, [], s.value)
      return

    case 'stmtexp':
      scopeCheckExp(diagnostics, globals, qualified, [], s.expr)
      return

    default:
      throw new ICE(
        'scopeCheckStmtBodies',
        `Non-core statement encountered ${s.tag}`,
      )
  }
}

/**
 * Scope-checks an (expanded) program, collecting diagnostics. Loads every
 * transitively-imported file's symbols first, seeds the runtime and prelude
 * globals, then checks it in two passes so top-level definitions are mutually
 * recursive: collect all top-level bindings, then check every statement's body.
 */
export async function scopeCheckProgram(
  diagnostics: ScamperDiagnostic[],
  prog: A.Prog,
) {
  // Ensure every imported file's symbols are in the DB before we resolve names.
  // Failures found below the top level are reported here (a direct import's
  // failure is reported when it is resolved in collectTopLevelBindings).
  for (const f of await SymbolDB.loadTransitiveImports(prog)) {
    diagnostics.push(
      mkDiagnostic(
        'Scope',
        'warning',
        `Could not load module '${f.filename}' (imported by '${f.importer}')`,
        f.range,
      ),
    )
  }
  const globals: string[] = []
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  for (const id of SymbolDB.get('runtime')!) {
    globals.push(id.name)
  }
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  for (const id of SymbolDB.get('prelude')!) {
    globals.push(id.name)
  }
  // Two passes so that top-level definitions are mutually recursive: collect
  // every top-level binding first (also resolving imports, registering qualified
  // module aliases, and flagging name collisions), then check each statement's
  // bodies against the full set.
  const sources = new Map<string, string | null>()
  const qualified: QualifiedModules = new Map()
  for (const s of prog) {
    await collectTopLevelBindings(diagnostics, globals, qualified, sources, s)
  }
  for (const s of prog) {
    scopeCheckStmtBodies(diagnostics, globals, qualified, sources, s)
  }
}

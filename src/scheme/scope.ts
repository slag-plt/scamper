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

    case 'pctor': {
      p.args.forEach((p) => {
        scopeCheckPat(diagnostics, locals, p)
      })
      return
    }
  }
}

/**
 * Scope-checks an expression, reporting references to names bound in neither
 * `locals` (lexical scope) nor `globals` (top level), plus duplicate binders.
 */
function scopeCheckExp(
  diagnostics: ScamperDiagnostic[],
  globals: string[],
  locals: string[],
  e: A.Exp,
) {
  switch (e.tag) {
    case 'id': {
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
      scopeCheckExp(diagnostics, globals, locals, e.head)
      e.args.forEach((e) => {
        scopeCheckExp(diagnostics, globals, locals, e)
      })
      return
    }

    case 'lam': {
      // N.B., do we want to warn in the case of shadowed variables?
      const allParams = (
        e.restParam ? [...e.params, e.restParam] : e.params
      ).map((p) => p.name)
      checkDuplicateVars(diagnostics, allParams, e.range)
      scopeCheckExp(diagnostics, globals, [...locals, ...allParams], e.body)
      return
    }
    case 'let': {
      const vars = e.bindings.map((b) => b.id.name)
      checkDuplicateVars(diagnostics, vars, e.range)
      e.bindings.forEach((b) => {
        scopeCheckExp(diagnostics, globals, locals, b.value)
      })
      scopeCheckExp(diagnostics, globals, [...locals, ...vars], e.body)
      return
    }
    case 'begin': {
      e.exps.forEach((e) => {
        scopeCheckExp(diagnostics, globals, locals, e)
      })
      return
    }

    case 'if': {
      scopeCheckExp(diagnostics, globals, locals, e.guard)
      scopeCheckExp(diagnostics, globals, locals, e.ifB)
      scopeCheckExp(diagnostics, globals, locals, e.elseB)
      return
    }
    case 'match': {
      scopeCheckExp(diagnostics, globals, locals, e.scrutinee)
      e.branches.forEach((b) => {
        const bindingVars = new Set<string>()
        scopeCheckPat(diagnostics, bindingVars, b.pat)
        scopeCheckExp(diagnostics, globals, [...locals, ...bindingVars], b.body)
      })
      return
    }
    case 'quote': {
      // N.B., no need to scope check a "frozen" AST
      return
    }
    case 'jsvar': {
      // N.B., no variable references to check -- the argument is a literal
      // string naming a JS binding, resolved at runtime.
      return
    }
    case 'error': {
      scopeCheckExp(diagnostics, globals, locals, e.exp)
      return
    }
    case 'apply': {
      scopeCheckExp(diagnostics, globals, locals, e.fn)
      scopeCheckExp(diagnostics, globals, locals, e.args)
      return
    }
    case 'report': {
      scopeCheckExp(diagnostics, globals, locals, e.exp)
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
 * Scope-checks a single top-level statement, extending `globals` with any
 * names it introduces (a define/struct binder, or an import's bindings).
 */
async function scopeCheckStmt(
  diagnostics: ScamperDiagnostic[],
  globals: string[],
  s: A.Stmt,
) {
  switch (s.tag) {
    case 'import': {
      if (s.kind === 'builtin') {
        const mod = SymbolDB.get(s.module)
        if (mod !== undefined) {
          for (const id of mod) {
            globals.push(id.name)
          }
        } else {
          diagnostics.push(
            mkDiagnostic(
              'Scope',
              'warning',
              `No such built-in library: '${s.module}'`,
              s.range,
            ),
          )
        }
        return
      }

      // File import. N.B., import '../fs' lazily -- a static import would pull
      // the OPFS implementation into this module's (widely-imported) graph and
      // disturb tests that mock the file system (see symbol-db.ts).
      const { getFS } = await import('../fs')
      if (!(await getFS().fileExists(s.module))) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `File '${s.module}' does not exist`,
            s.range,
          ),
        )
        return
      }
      // Imported files' symbols were loaded into the DB before scope checking
      // (see SymbolDB.loadTransitiveImports). A missing DB entry for a file
      // that exists means it failed to parse.
      const mod = SymbolDB.get(s.module)
      if (mod === undefined) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `Could not load module '${s.module}'`,
            s.range,
          ),
        )
      } else {
        mod.forEach((id) => globals.push(id.name))
      }
      return
    }

    case 'define': {
      if (globals.includes(s.name.name)) {
        diagnostics.push(
          mkDiagnostic(
            'Scope',
            'warning',
            `Global variable '${s.name.name}' is already defined`,
            s.range,
          ),
        )
      } else {
        globals.push(s.name.name)
      }
      scopeCheckExp(diagnostics, globals, [], s.value)
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

    case 'display': {
      scopeCheckExp(diagnostics, globals, [], s.value)
      return
    }

    case 'stmtexp': {
      scopeCheckExp(diagnostics, globals, [], s.expr)
      return
    }

    default:
      throw new ICE('scopeCheckStmt', `Non-core statement encountered ${s.tag}`)
  }
}

/**
 * Scope-checks an (expanded) program, collecting diagnostics. Loads every
 * transitively-imported file's symbols first, seeds the runtime and prelude
 * globals, then checks each statement in order.
 */
export async function scopeCheckProgram(
  diagnostics: ScamperDiagnostic[],
  prog: A.Prog,
) {
  // Ensure every imported file's symbols are in the DB before we resolve names.
  // Failures found below the top level are reported here (a direct import's
  // failure is reported by its own statement in scopeCheckStmt).
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
  for (const s of prog) {
    await scopeCheckStmt(diagnostics, globals, s)
  }
}

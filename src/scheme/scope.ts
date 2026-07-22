import { getFS } from "../fs"
import * as L from "../lpm"
import { ICE, ScamperError } from "../lpm"
import * as A from "./ast.js"
import {
  ComplexPred,
  FunctionDoc,
  Pred,
  parseFunctionDocFromComments,
} from "./docstring/docstring"
import { mkScamperErrorWithRange } from "./util"

function checkDuplicateVars(
  errors: ScamperError[],
  vars: string[],
  range: L.Range,
) {
  const seen = new Set<string>()
  for (const v of vars) {
    if (seen.has(v)) {
      errors.push(
        new ScamperError(
          "Parser",
          `Duplicate variable '${v}' encountered in binding list`,
          undefined,
          range,
        ),
      )
    }
    seen.add(v)
  }
}

function scopeCheckPat(errors: ScamperError[], locals: Set<string>, p: A.Pat) {
  switch (p.tag) {
    case "pvar": {
      if (locals.has(p.name)) {
        errors.push(
          new ScamperError(
            "Parser",
            `Duplicate binding variable '${p.name}' encountered in pattern`,
            undefined,
            p.range,
          ),
        )
      } else {
        locals.add(p.name)
      }
      return
    }

    case "pwild":
      return
    case "plit":
      return

    case "pctor": {
      p.args.forEach((p) => {
        scopeCheckPat(errors, locals, p)
      })
      return
    }
  }
}

function scopeCheckExp(
  errors: ScamperError[],
  globals: string[],
  locals: string[],
  e: A.Exp,
) {
  switch (e.tag) {
    case "var": {
      if (!locals.includes(e.name) && !globals.includes(e.name)) {
        errors.push(
          new ScamperError(
            "Parser",
            `Undefined variable '${e.name}'`,
            undefined,
            e.range,
          ),
        )
      }
      return
    }

    case "lit":
      return

    case "app": {
      scopeCheckExp(errors, globals, locals, e.head)
      e.args.forEach((e) => {
        scopeCheckExp(errors, globals, locals, e)
      })
      return
    }

    case "lam": {
      // N.B., do we want to warn in the case of shadowed variables?
      const allParams = e.restParam ? [...e.params, e.restParam] : e.params
      checkDuplicateVars(errors, allParams, e.range)
      scopeCheckExp(errors, globals, [...locals, ...allParams], e.body)
      return
    }
    case "let": {
      const vars = e.bindings.map((b) => b.name)
      checkDuplicateVars(errors, vars, e.range)
      e.bindings.forEach((b) => {
        scopeCheckExp(errors, globals, locals, b.value)
      })
      scopeCheckExp(errors, globals, [...locals, ...vars], e.body)
      return
    }
    case "begin": {
      e.exps.forEach((e) => {
        scopeCheckExp(errors, globals, locals, e)
      })
      return
    }

    case "if": {
      scopeCheckExp(errors, globals, locals, e.guard)
      scopeCheckExp(errors, globals, locals, e.ifB)
      scopeCheckExp(errors, globals, locals, e.elseB)
      return
    }
    case "match": {
      scopeCheckExp(errors, globals, locals, e.scrutinee)
      e.branches.forEach((b) => {
        const bindingVars = new Set<string>()
        scopeCheckPat(errors, bindingVars, b.pat)
        scopeCheckExp(errors, globals, [...locals, ...bindingVars], b.body)
      })
      return
    }
    case "quote": {
      // N.B., no need to scope check a "frozen" AST
      return
    }
    case "jsvar": {
      // N.B., no variable references to check -- the argument is a literal
      // string naming a JS binding, resolved at runtime.
      return
    }
    case "error": {
      scopeCheckExp(errors, globals, locals, e.exp)
      return
    }
    case "apply": {
      scopeCheckExp(errors, globals, locals, e.fn)
      scopeCheckExp(errors, globals, locals, e.args)
      return
    }
    case "report": {
      scopeCheckExp(errors, globals, locals, e.exp)
      return
    }
    default:
      throw new ICE("scopeCheckExp", `Non-core expression encountered ${e.tag}`)
  }
}

// TODO: test this
function scopeCheckPred(
  errors: ScamperError[],
  predicate: Pred,
  globals: string[],
) {
  if (A.isVar(predicate)) {
    if (!globals.includes(predicate.name)) {
      errors.push(
        mkScamperErrorWithRange(
          "Parser",
          `Undefined predicate "${predicate.name}"`,
          predicate.range,
        ),
      )
    }
  } else {
    scopeCheckComplexPred(errors, predicate, globals)
  }
}

// TODO: test this
function scopeCheckComplexPred(
  errors: ScamperError[],
  { head: { name }, args, range }: ComplexPred,
  globals: string[],
) {
  if (!globals.includes(name)) {
    errors.push(
      mkScamperErrorWithRange("Parser", `Undefined predicate "${name}"`, range),
    )
  }
  for (const arg of args) {
    scopeCheckPred(errors, arg, globals)
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
//
// N.B., takes the already-parsed `doc` (rather than reading it off the
// Define directly) since parsing a docstring can fail, and that failure is
// handled by the caller -- everything this function itself reports is a
// *semantic* mismatch between an already-valid docstring and the function
// it's attached to (wrong param names, missing description, ...), which is
// just as much a documentation-quality issue as a malformed docstring, not
// a real scope error. So errors are collected into a local array and
// re-tagged with phase "Docstring" before being merged into the caller's
// `outerErrors`, regardless of which branch below (or which nested
// scopeCheckPred call) produced them.
function scopeCheckFunctionDoc(
  outerErrors: ScamperError[],
  doc: FunctionDoc,
  { name, value }: A.Define,
  globals: string[],
): void {
  const errors: ScamperError[] = []
  try {
    scopeCheckFunctionDocInner(errors, doc, name, value, globals)
  } finally {
    outerErrors.push(
      ...errors.map(
        (e) => new ScamperError("Docstring", e.message, e.modName, e.range, e.source),
      ),
    )
  }
}

function scopeCheckFunctionDocInner(
  errors: ScamperError[],
  doc: FunctionDoc,
  name: string,
  value: A.Exp,
  globals: string[],
): void {
  if (!A.isLam(value)) {
    // can't attach function docs onto non-function definitions
    errors.push(
      mkScamperErrorWithRange(
        "Parser",
        "Function docstring attached to non-function definition",
        doc.range,
      ),
    )
    return
  }

  const { params } = value
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
    errors.push(
      mkScamperErrorWithRange(
        "Parser",
        `Docstring function name "${docName}" does not match defined name "${name}"`,
        sigRange,
      ),
    )
    // this is not a catastrophic error, continue parsing
  }

  // ... lst val)...
  for (const param of params) {
    const nextDocParam = docParams.shift()
    if (nextDocParam === undefined) {
      errors.push(
        mkScamperErrorWithRange(
          "Parser",
          `Expected function parameter "${param}" to be defined in docstring signature`,
          sigRange,
        ),
      )
      continue
    }
    if (param !== nextDocParam) {
      errors.push(
        mkScamperErrorWithRange(
          "Parser",
          `Function signature defines parameter "${param}" in this position but docstring signature instead defines "${nextDocParam}"`,
          sigRange,
        ),
      )
    }
  }
  // don't check for remaining parameters, docstring param description check will get that

  // ... -> list?...
  scopeCheckPred(errors, predicate, globals)

  // ...lst : list?... (param descriptions)
  const paramWasChecked = new Map<string, boolean>(
    [...params].map((p) => [p, false]),
  )
  for (const {
    name: pName,
    predicate: pPred,
    range: pRange,
  } of docParamDescriptions) {
    if (!params.includes(pName)) {
      errors.push(
        mkScamperErrorWithRange(
          "Parser",
          `Docstring describes unknown function parameter "${pName}"`,
          pRange,
        ),
      )
    }
    paramWasChecked.set(pName, true)
    scopeCheckPred(errors, pPred, globals)
  }
  for (const [pName, wasChecked] of paramWasChecked) {
    if (wasChecked) {
      continue
    }
    errors.push(
      mkScamperErrorWithRange(
        "Parser",
        `Description of function parameter "${pName}" missing`,
        docRange,
      ),
    )
  }
  // TODO: validate @example calls the documented function with the correct arity
  // TODO: validate @example results satisfy the signature return predicate
  return
}

async function scopeCheckStmt(
  errors: ScamperError[],
  builtinLibs: Map<string, L.Module>,
  globals: string[],
  s: A.Stmt,
) {
  switch (s.tag) {
    case "import": {
      if (builtinLibs.has(s.module)) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        for (const [name, _] of builtinLibs.get(s.module)!.bindings) {
          globals.push(name)
        }
      } else if (await getFS().fileExists(s.module)) {
        // TODO: should gather top-level bindings from the imported module, but
        // for now, let's just assume everything is good to check the rest of the
        // pipeline...
      } else {
        errors.push(
          new ScamperError(
            "Parser",
            `Library '${s.module}' is not defined`,
            undefined,
            s.range,
          ),
        )
      }
      return
    }

    case "define": {
      if (globals.includes(s.name)) {
        errors.push(
          new ScamperError(
            "Parser",
            `Global variable '${s.name}' is already defined`,
            undefined,
            s.range,
          ),
        )
      } else {
        globals.push(s.name)
      }
      scopeCheckExp(errors, globals, [], s.value)
      if (s.docComments) {
        // parsing (not just scope-checking) can fail here -- a malformed
        // docstring is reported the same way as a semantic mismatch (phase
        // "Docstring"), not as a real scope error.
        try {
          const doc = parseFunctionDocFromComments(s.docComments)
          if (doc) {
            scopeCheckFunctionDoc(errors, doc, s, globals)
          }
        } catch (e) {
          if (!(e instanceof ScamperError)) {
            throw e
          }
          errors.push(e)
        }
      }
      return
    }

    case "display": {
      scopeCheckExp(errors, globals, [], s.value)
      return
    }

    case "stmtexp": {
      scopeCheckExp(errors, globals, [], s.expr)
      return
    }

    default:
      throw new ICE("scopeCheckStmt", `Non-core statement encountered ${s.tag}`)
  }
}

export async function scopeCheckProgram(
  builtinLibs: Map<string, L.Module>,
  errors: ScamperError[],
  prog: A.Prog,
) {
  const globals: string[] = []
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  for (const name of builtinLibs.get('runtime')!.bindings.keys()) {
     globals.push(name)
   }
   // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
   for (const name of builtinLibs.get('prelude')!.bindings.keys()) {
     globals.push(name)
   }
  for (const s of prog) {
    await scopeCheckStmt(errors, builtinLibs, globals, s)
  }
}

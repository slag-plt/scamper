import { ICE, ScamperError } from '../lpm'
import * as A from './ast.js'
import * as L from '../lpm'

function checkDuplicateVars (errors: ScamperError[], vars: string[], range: L.Range) {
  const seen = new Set<string>()
  for (const v of vars) {
    if (seen.has(v)) {
      errors.push(new ScamperError('Parser', `Duplicate variable '${v}' encountered in binding list`, undefined, range))
    }
    seen.add(v)
  }
}

function scopeCheckPat (errors: ScamperError[], locals: Set<string>, p: A.Pat) {
  switch (p.tag) {
    case 'pvar': {
      if (locals.has(p.name)) {
        errors.push(new ScamperError('Parser', `Duplicate binding variable '${p.name}' encountered in pattern`, undefined, p.range))
      } else {
        locals.add(p.name)
      }
      return
    }

    case 'pwild': return
    case 'plit': return

    case 'pctor': {
      p.args.forEach((p) => scopeCheckPat(errors, locals, p))
      return
    }
  }
}

function scopeCheckExp (errors: ScamperError[], globals: string[], locals: string[], e: A.Exp) {
  switch (e.tag) {
    case 'var': {
      if (!locals.includes(e.name) && !globals.includes(e.name)) {
        errors.push(new ScamperError('Parser', `Undefined variable '${e.name}'`, undefined, e.range))
      }
      return
    }

    case 'lit': return

    case 'app': {
      scopeCheckExp(errors, globals, locals, e.head)
      e.args.forEach((e) => scopeCheckExp(errors, globals, locals, e))
      return
    }

    case 'lam': {
      // N.B., do we want to warn in the case of shadowed variables?
      checkDuplicateVars(errors, e.params, e.range)
      scopeCheckExp(errors, globals, [...locals, ...e.params], e.body)
      return
    }
    case 'let': {
      const vars = e.bindings.map(b => b.name)
      checkDuplicateVars(errors, vars, e.range)
      e.bindings.forEach(b => scopeCheckExp(errors, globals, locals, b.value))
      scopeCheckExp(errors, globals, [...locals, ...vars], e.body)
      return
    }
    case 'begin': {
      e.exps.forEach((e) => scopeCheckExp(errors, globals, locals, e))
      return
    }

    case 'if': {
      scopeCheckExp(errors, globals, locals, e.guard)
      scopeCheckExp(errors, globals, locals, e.ifB)
      scopeCheckExp(errors, globals, locals, e.elseB)
      return
    }
    case 'match': {
      scopeCheckExp(errors, globals, locals, e.scrutinee)
      e.branches.forEach((b) => {
        const bindingVars: Set<string> = new Set()
        scopeCheckPat(errors, bindingVars, b.pat)
        scopeCheckExp(errors, globals, [...locals, ...bindingVars], b.body)
      })
      return
    }
    case 'quote': {
      // N.B., no need to scope check a "frozen" AST
      return
    }
    default:
      throw new ICE('scopeCheckExp', `Non-core expression encountered ${e.tag}`)
  }
}

function scopeCheckStmt (errors: ScamperError[], builtinLibs: Map<string, L.Library>, globals: string[], s: A.Stmt) {
  switch (s.tag) {
    case 'import': {
      if (!builtinLibs.has(s.module)) {
        errors.push(new ScamperError('Parser', `Library '${s.module}' is not defined`, undefined, s.range))
      }
      for (const [name, _] of builtinLibs.get(s.module)!.lib) {
        globals.push(name)
      }
      return
    }
      
    case 'define': {
      if (globals.includes(s.name)) {
        errors.push(new ScamperError('Parser', `Global variable '${s.name}' is already defined`, undefined, s.range))
      } else {
        globals.push(s.name)
      }
      return
    }

    case 'display': {
      scopeCheckExp(errors, globals, [], s.value)
      return
    }

    case 'stmtexp': {
      scopeCheckExp(errors, globals, [], s.expr)
      return
    }

    default:
      throw new ICE('scopeCheckStmt', `Non-core statement encountered ${s.tag}`)
  }
}

export function scopeCheckProgram (builtinLibs: Map<string, L.Library>, errors: ScamperError[], prog: A.Prog) {
  const globals: string[] = []
  for (const [name, _] of builtinLibs.get('prelude')!.lib) {
    globals.push(name)
  }
  for (const s of prog) {
    scopeCheckStmt(errors, builtinLibs, globals, s)
  }
}
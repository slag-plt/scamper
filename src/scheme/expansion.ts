import * as A from './ast.js'

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function collectSectionHoles (bvars: string[], e: A.Exp): A.Exp {
  switch (e.tag) {
    case 'var': {
      if (e.name === '_') {
        const newName = genHoleSym()
        bvars.push(newName)
        return A.mkVar(newName, e.range)
      } else {
        return e
      }
    }
    case 'lit': return e
    case 'app': return A.mkApp(collectSectionHoles(bvars, e.head), e.args.map((a) => collectSectionHoles(bvars, a)), e.range)
    case 'lam': return A.mkLam(e.params, collectSectionHoles(bvars, e.body), e.range)
    case 'let': return A.mkLet(e.bindings.map((b) => ({name: b.name, value: collectSectionHoles(bvars, b.value)})), collectSectionHoles(bvars, e.body), e.range)
    case 'begin': return A.mkBegin(e.exps.map((a) => collectSectionHoles(bvars, a)), e.range)
    case 'if': return A.mkIf(collectSectionHoles(bvars, e.guard), collectSectionHoles(bvars, e.ifB), collectSectionHoles(bvars, e.elseB), e.range)
    case 'match': return A.mkMatch(collectSectionHoles(bvars, e.scrutinee), e.branches.map((b) => ({pat: b.pat, body: collectSectionHoles(bvars, b.body)})), e.range)
    case 'quote': return e
    case 'let*': return A.mkLetS(e.bindings.map((b) => ({name: b.name, value: collectSectionHoles(bvars, b.value)})), collectSectionHoles(bvars, e.body), e.range)
    case 'and': return A.mkAnd(e.exps.map((a) => collectSectionHoles(bvars, a)), e.range)
    case 'or': return A.mkOr(e.exps.map((a) => collectSectionHoles(bvars, a)), e.range)
    case 'cond': return A.mkCond(e.branches.map((b) => ({test: collectSectionHoles(bvars, b.test), body: collectSectionHoles(bvars, b.body)})), e.range)
    case 'section': {
      // N.B., we do not collect holes in embedded sections
      return A.mkSection(e.exps, e.range)
    }
  } 
}

export function expandExpr (e: A.Exp): A.Exp {
  switch (e.tag) {
    // Core forms
    case 'var': return e
    case 'lit': return e
    case 'app': return A.mkApp(expandExpr(e.head), e.args.map(expandExpr), e.range)
    case 'lam': return A.mkLam(e.params, expandExpr(e.body), e.range)
    case 'let': return A.mkLet(e.bindings.map((b) => ({name: b.name, value: expandExpr(b.value)})), expandExpr(e.body), e.range)
    case 'begin': return A.mkBegin(e.exps.map(expandExpr), e.range)
    case 'if': return A.mkIf(expandExpr(e.guard), expandExpr(e.ifB), expandExpr(e.elseB), e.range)
    case 'match': return A.mkMatch(expandExpr(e.scrutinee), e.branches.map((b) => ({pat: b.pat, body: expandExpr(b.body)})), e.range)
    case 'quote': return e

    // Derived forms

    case 'let*': {
      // (let* [x1 e1] ... [xk ek] e)
      // -->
      // (let [x1 e1]
      //   ...
      //     (let [xk ek] e))
      const bindings = e.bindings.map((b) => ({name: b.name, value: expandExpr(b.value)}))
      const body = expandExpr(e.body)
      let ret = body
      for (let i = bindings.length - 1; i >= 0; i--) {
        ret = A.mkLet([bindings[i]], ret, e.range)
      }
      return ret
    }
    case 'and': {
      // (and e1 ... ek)
      // -->
      // (if e1
      //   ...
      //     (if ek
      //       #t
      //       #f)
      //   ...
      //   #f)
      const exps = e.exps.map(expandExpr)
      let ret: A.Exp = A.mkLit(true)
      for (let i = exps.length - 1; i >= 0; i--) {
        ret = A.mkIf(exps[i], ret, A.mkLit(false), e.range)
      }
      return ret
    }
    case 'or': {
      // (or e1 ... ek)
      // -->
      // (if e1
      //   #t
      //   ...
      //     (if ek
      //       #t
      //       #f))
      const exps = e.exps.map(expandExpr)
      let ret: A.Exp = A.mkLit(false)
      for (let i = exps.length - 1; i >= 0; i--) {
        ret = A.mkIf(exps[i], A.mkLit(true), ret, e.range)
      }
      return ret
    }
    case 'cond': {
      // (cond [e11 e12] ... [ek1 ek2])
      // -->
      // (if e11 e12
      //   ...
      //     (if ek1 ek2 (error "No matching clause in cond"))
      const branches = e.branches.map((c) => ({test: expandExpr(c.test), body: expandExpr(c.body)}))
      let ret: A.Exp = A.mkApp(A.mkVar('error'), [A.mkLit('No matching clause in cond')], e.range)
      for (let i = branches.length - 1; i >= 0; i--) {
        ret = A.mkIf(branches[i].test, branches[i].body, ret, e.range)
      }
      return ret
    }
    case 'section': {
      // (section e1 ... ek)
      // -->
      // (lambda (x1 ... xm) (e1' ... ek'))
      //   where occurrences of _ are replaced with fresh x1 ... xm
      const bvars: string[] = []
      const exps = e.exps.map((arg) => collectSectionHoles(bvars, arg))
      return A.mkLam(bvars, A.mkApp(exps[0], exps.slice(1)), e.range)
    }
  }
}

export function expandStmt (s: A.Stmt): A.Stmt[] {
  switch (s.tag) {
    case 'import': return [s]
    case 'define': return [A.mkDefine(s.name, expandExpr(s.value), s.range)]
    case 'display': return [A.mkDisp(expandExpr(s.value), s.range)]
    case 'struct': {
      const ctor = A.mkDefine(
        s.name,
        A.mkApp(A.mkVar('##mkCtorFn##'), s.fields.map((f) => A.mkVar(f))),
        s.range
      )
      const pred = A.mkDefine(
        `${s.name}?`,
        A.mkApp(A.mkVar('##mkPredFn##'), [A.mkVar(s.name)], s.range)
      )
      const accessors = s.fields.map((f) => A.mkDefine(
        `${s.name}-${f}`,
        A.mkApp(A.mkVar('##mkGetFn##'), [A.mkVar(s.name), A.mkVar(f)]),
        s.range
      ))
      return [ctor, pred, ...accessors]
    }
    case 'stmtexp': return [A.mkStmtExp(s.expr)]
  }
}

export function expandProgram (prog: A.Prog): A.Prog {
  return prog.flatMap(expandStmt)
}
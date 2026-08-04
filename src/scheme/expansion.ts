import * as A from './ast.js'

export function expandExpr(e: A.Exp): A.Exp {
  switch (e.tag) {
    // Core forms
    case 'id':
      return e
    case 'lit':
      return e
    case 'app':
      return A.mkApp(expandExpr(e.head), e.args.map(expandExpr), e.range)
    case 'lam':
      return A.mkLam(e.params, expandExpr(e.body), e.range, e.restParam)
    case 'let':
      return A.mkLet(
        e.bindings.map((b) => ({ pat: b.pat, value: expandExpr(b.value) })),
        expandExpr(e.body),
        e.range,
      )
    case 'if':
      return A.mkIf(
        expandExpr(e.guard),
        expandExpr(e.ifB),
        expandExpr(e.elseB),
        e.range,
      )
    case 'match':
      return A.mkMatch(
        expandExpr(e.scrutinee),
        e.branches.map((b) => ({ pat: b.pat, body: expandExpr(b.body) })),
        e.range,
      )
    case 'quote':
      return e
    // Derived forms

    case 'begin': {
      // (begin e1 ... ek)
      // -->
      // (let ([_ e1])
      //   ...
      //     (let ([_ e(k-1)]) ek))
      // Each non-final expression binds to a fresh wildcard, so it runs for
      // effect and its value is discarded; ek is the result.
      const exps = e.exps.map(expandExpr)
      let ret = exps[exps.length - 1]
      for (let i = exps.length - 2; i >= 0; i--) {
        ret = A.mkLet(
          [{ pat: A.mkPWild(e.range), value: exps[i] }],
          ret,
          e.range,
          'begin',
        )
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
      let ret: A.Exp = A.mkLit(true, e.range, 'and')
      for (let i = exps.length - 1; i >= 0; i--) {
        ret = A.mkIf(exps[i], ret, A.mkLit(false, e.range, 'and'), e.range, 'and')
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
      let ret: A.Exp = A.mkLit(false, e.range, 'or')
      for (let i = exps.length - 1; i >= 0; i--) {
        ret = A.mkIf(exps[i], A.mkLit(true, e.range, 'or'), ret, e.range, 'or')
      }
      return ret
    }
    case 'cond': {
      // (cond [e11 e12] ... [ek1 ek2])
      // -->
      // (if e11 e12
      //   ...
      //     (if ek1 ek2 (error "No matching clause in cond"))
      const branches = e.branches.map((c) => ({
        test: expandExpr(c.test),
        body: expandExpr(c.body),
      }))
      let ret: A.Exp = A.mkApp(
        A.mkId('error', e.range),
        [A.mkLit('No matching clause in cond', e.range)],
        e.range,
        'cond',
      )
      for (let i = branches.length - 1; i >= 0; i--) {
        ret = A.mkIf(branches[i].test, branches[i].body, ret, e.range, 'cond')
      }
      return ret
    }
  }
}

export function expandStmt(s: A.Stmt): A.Stmt[] {
  switch (s.tag) {
    case 'import':
      return [s]
    case 'define':
      return [A.mkDefine(s.name, expandExpr(s.value), s.range, s.docComments)]
    case 'display':
      return [A.mkDisp(expandExpr(s.value), s.range)]
    case 'struct': {
      // (struct S (f1 ... fk))
      // -->
      // (define S (##mkCtorFn## S f1 ... fk))
      // (define S? (##mkPredFn## S))
      // ...
      // (define S-fk (##mkGetFn## S fk))
      const ctor = A.mkDefine(
        s.name,
        A.mkApp(
          A.mkId('##mkCtorFn##'),
          [A.mkLit(s.name.name), A.mkLit(s.fields.map((f) => f.name))],
          s.range,
        ),
        s.range,
      )
      const pred = A.mkDefine(
        A.mkId(`${s.name.name}?`, s.range),
        A.mkApp(A.mkId('##mkPredFn##'), [A.mkLit(s.name.name)], s.range),
      )
      const accessors = s.fields.map((f) =>
        A.mkDefine(
          A.mkId(`${s.name.name}-${f.name}`, s.range),
          A.mkApp(A.mkId('##mkGetFn##'), [A.mkLit(s.name.name), A.mkLit(f.name)]),
          s.range,
        ),
      )
      return [ctor, pred, ...accessors]
    }
    case 'stmtexp':
      return [A.mkStmtExp(expandExpr(s.expr))]
  }
}

export function expandProgram(prog: A.Prog): A.Prog {
  return prog.flatMap(expandStmt)
}

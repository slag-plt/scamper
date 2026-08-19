import * as A from './ast.js'
import { ICE } from '../lpm/index.js'

/**
 * Walks a (core, already-expanded) expression to canonicalize the anonymous
 * function shorthand `%` to `%1` and record which parameters it references, so
 * the enclosing `#(...)` can build its lambda. `acc.maxNum` tracks the largest
 * `%k` index seen (the arity) and `acc.hasRest` whether `%&` appears. The parser
 * guarantees `%` identifiers appear only inside a `#(...)` and that `#(...)`
 * forms never nest, so this may descend through every sub-expression freely.
 */
function collectAndNormalizePercent(
  e: A.Exp,
  acc: { maxNum: number; hasRest: boolean },
): A.Exp {
  const rec = (x: A.Exp) => collectAndNormalizePercent(x, acc)
  switch (e.tag) {
    case 'lit':
      return e
    case 'id': {
      if (e.name === '%&') {
        acc.hasRest = true
        return e
      }
      if (e.name === '%') {
        // `%` is shorthand for `%1`; canonicalize it so both spellings bind the
        // same parameter.
        acc.maxNum = Math.max(acc.maxNum, 1)
        return A.mkId('%1', e.range)
      }
      if (/^%[1-9][0-9]*$/.test(e.name)) {
        acc.maxNum = Math.max(acc.maxNum, parseInt(e.name.slice(1), 10))
      }
      return e
    }
    case 'app':
      return A.mkApp(rec(e.head), e.args.map(rec), e.range, e.provenance)
    case 'lam':
      return A.mkLam(e.params, rec(e.body), e.range, e.restParam, e.provenance)
    case 'let':
      return A.mkLet(
        e.bindings.map((b) => ({ pat: b.pat, value: rec(b.value) })),
        rec(e.body),
        e.range,
        e.provenance,
      )
    case 'if':
      return A.mkIf(rec(e.guard), rec(e.ifB), rec(e.elseB), e.range, e.provenance)
    case 'match':
      return A.mkMatch(
        rec(e.scrutinee),
        e.branches.map((b) => ({ pat: b.pat, body: rec(b.body) })),
        e.range,
      )
    default:
      // and/or/begin/cond/anonfn/vec/obj are removed by expandExpr before we
      // get here.
      throw new ICE('collectAndNormalizePercent', `Unexpected form: ${e.tag}`)
  }
}

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
      //     (if ek1 ek2 (##error## "No matching clause in cond"))
      // ##error## is a runtime primitive (src/js/runtime), *not* the prelude's
      // `error`: a fall-through must raise whether or not the user happens to
      // have bound the name `error` (#336).
      const branches = e.branches.map((c) => ({
        test: expandExpr(c.test),
        body: expandExpr(c.body),
      }))
      let ret: A.Exp = A.mkApp(
        A.mkId('##error##', e.range),
        [A.mkLit('No matching clause in cond', e.range)],
        e.range,
        'cond',
      )
      for (let i = branches.length - 1; i >= 0; i--) {
        ret = A.mkIf(branches[i].test, branches[i].body, ret, e.range, 'cond')
      }
      return ret
    }
    case 'anonfn': {
      // #(body)
      // -->
      // (lambda (%1 ... %m [& %&]) body')
      //   where each `%` in body is canonicalized to `%1`, the arity m is the
      //   largest `%k` index referenced, and `%&` (if present) is the rest
      //   parameter. The resulting lambda is tagged `anon-fn` so sugaring
      //   recovers the `#(...)`. The body is expanded first so the collection
      //   walk only meets core forms.
      const acc = { maxNum: 0, hasRest: false }
      const body = collectAndNormalizePercent(expandExpr(e.body), acc)
      const params: A.Identifier[] = []
      for (let i = 1; i <= acc.maxNum; i++) {
        params.push(A.mkId(`%${String(i)}`, e.range))
      }
      const restParam = acc.hasRest ? A.mkId('%&', e.range) : undefined
      return A.mkLam(params, body, e.range, restParam, 'anon-fn')
    }
    case 'vec':
      // [e1 ... ek]
      // -->
      // (##mkVec## e1 ... ek)
      // ##mkVec## is a runtime primitive (src/js/runtime), *not* the prelude's
      // `vector`: a literal's meaning must not depend on whether the user
      // happens to have bound the name `vector`. Tagged `vector-lit` so
      // sugaring recovers the bracket form exactly.
      return A.mkApp(
        A.mkId('##mkVec##', e.range),
        e.exps.map(expandExpr),
        e.range,
        'vector-lit',
      )
    case 'obj':
      // {k1 v1 ... kn vn}
      // -->
      // (##mkObj## k1 v1 ... kn vn)
      // ##mkObj## likewise pairs its arguments up into a Javascript object; it
      // raises if a key is not a string. Tagged `obj-lit` so sugaring recovers
      // the brace form.
      return A.mkApp(
        A.mkId('##mkObj##', e.range),
        e.pairs.flatMap(({ key, value }) => [expandExpr(key), expandExpr(value)]),
        e.range,
        'obj-lit',
      )
  }
}

export function expandStmt(s: A.Stmt): A.Stmt[] {
  switch (s.tag) {
    case 'import':
      return [s]
    case 'define':
      return [A.mkDefine(s.name, expandExpr(s.value), s.range, s.docComments)]
    case 'export':
      return [s]
    case 'defexport':
      // (define-export x e)
      // -->
      // (define x e)
      // (export x)
      // Both are tagged 'define-export' so sugaring recovers the define-export
      // exactly (a hand-written define + export pair is left untouched).
      return [
        A.mkDefine(
          s.name,
          expandExpr(s.value),
          s.range,
          s.docComments,
          'define-export',
        ),
        A.mkExport([s.name], s.range, 'define-export'),
      ]
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
      // The range matters: every other case here carries it through, and
      // without it a bare top-level expression -- which is most of what a
      // student writes -- ends up with no source location at all.
      return [A.mkStmtExp(expandExpr(s.expr), s.range)]
  }
}

export function expandProgram(prog: A.Prog): A.Prog {
  return prog.flatMap(expandStmt)
}

import * as LPM from '../lpm'
import { Fiber } from '../lpm/fiber'
import { Frame } from '../lpm/frame'
import * as A from './ast.js'

/**
 * Lifts an LPM pattern back into a scheme-AST pattern (inverse of codegen's lowerPat).
 * @returns the equivalent scheme-AST pattern
 */
function raisePat(pat: LPM.Pat): A.Pat {
  switch (pat.tag) {
    case 'pwild':
      return A.mkPWild(pat.range)
    case 'plit':
      return A.mkPLit(pat.value, pat.range)
    case 'pvar':
      return A.mkId(pat.name, pat.range)
    case 'pctor':
      return A.mkPCtor(A.mkId(pat.name, pat.range), pat.args.map(raisePat), pat.range)
    case 'pvec':
      return A.mkPVec(pat.args.map(raisePat), pat.range)
  }
}

// The name codegen gives every lambda (see codegen.ts); a `define` replaces it
// when it binds one. A closure still carrying it has no name a student wrote,
// so anything showing the name instead of the lambda is showing an internal
// (#569).
const ANONYMOUS = '##anonymous##'

/** @returns true iff `v` is a closure with no Scamper-facing name. */
export function isAnonymousClosure(v: LPM.Value): v is LPM.Closure {
  return LPM.isClosure(v) && (v.name === undefined || v.name === ANONYMOUS)
}

/**
 * Rebuilds an evaluated closure as the lambda it was written as, by raising its
 * body against the scopes it captured.
 * @param env supplies the top-level names the body references; its locals are
 *            replaced by the closure's own.
 */
export function closureToLam(
  v: LPM.Closure,
  env: LPM.Env = LPM.Env.empty,
): A.Exp {
  const excluded = v.restParam ? [...v.params, v.restParam] : v.params
  return A.mkLam(
    v.params.map((p) => A.mkId(p)),
    raiseFrame(
      [],
      env.withLocalScopes(v.locals).withoutLocals(...excluded),
      v.code.toReversed(),
    ),
    undefined,
    v.restParam ? A.mkId(v.restParam) : undefined,
    v.provenance,
  )
}

/**
 * @param env the frame the values sit in, for raising a closure's body.
 * @return a stack of expressions created from the given value stack.
 */
export function valuesToExps(
  values: LPM.Value[],
  env: LPM.Env = LPM.Env.empty,
): A.Exp[] {
  return values.map((v) => {
    if (isAnonymousClosure(v)) {
      return closureToLam(v, env)
    } else if (LPM.isFunction(v) && v.name) {
      return A.mkId(v.name)
    } else {
      return A.mkLit(v)
    }
  })
}

export function raiseFrame(
  values: A.Exp[],
  env: LPM.Env,
  ops: LPM.Ops[],
): A.Exp {
  for (let i = ops.length - 1; i >= 0; i--) {
    const op = ops[i]
    switch (op.tag) {
      case 'lit': {
        values.push(A.mkLit(op.value, op.range, op.provenance))
        break
      }

      case 'var': {
        const r = env.lookup(op.name)
        if (r.found && r.slot !== LPM.HOLE && !LPM.isFunction(r.slot)) {
          // A bound non-function value: substitute it (shows the value in
          // traces, e.g. a let binder that has already been filled).
          values.push(A.mkLit(r.slot))
        } else {
          // Unbound, a still-unassigned hole, or a function: show the name.
          values.push(A.mkId(op.name))
        }
        break
      }

      case 'cls': {
        // A lambda that has not run yet shows as the lambda it is, unless it
        // has a name to show instead. Codegen's is the ANONYMOUS placeholder,
        // never a name a student wrote, and showing it hid the whole step
        // (#569). The provenance rides along so sugaring recovers a `#(...)`
        // written as one.
        if (op.name !== undefined && op.name !== ANONYMOUS) {
          values.push(A.mkId(op.name))
          break
        }
        const excluded = op.restParam ? [...op.params, op.restParam] : op.params
        values.push(
          A.mkLam(
            op.params.map((p) => A.mkId(p)),
            raiseFrame([], env.withoutLocals(...excluded), op.body.toReversed()),
            op.range,
            op.restParam ? A.mkId(op.restParam) : undefined,
            op.provenance,
          ),
        )
        break
      }

      case 'ap': {
        const vs = values.splice(-(op.numArgs + 1))
        const head = vs[0]
        const args = op.numArgs === 0 ? [] : vs.slice(1)
        values.push(A.mkApp(head, args, op.range, op.provenance))
        break
      }

      case 'match': {
        const scrutinee = LPM.popRequired(values, 'the raise stack')
        const matches = op.branches.map(([pat, body]) => {
          const bodyExp = raiseFrame(
            [],
            env.withoutLocals(...LPM.patVars(pat)),
            body.toReversed(),
          )
          return { pat: raisePat(pat), body: bodyExp }
        })
        values.push(A.mkMatch(scrutinee, matches))
        break
      }

      case 'let': {
        // Reconstruct the let so a trace shows per-binding progress: bindings
        // already assigned are omitted (their values substitute into what
        // remains, via the env); the binding in flight shows its current value
        // (reconstructed on the stack); pending bindings show their original
        // value expressions. Still-unassigned binders are excluded so they
        // render as names rather than substituted values or holes.
        if (op.idx === 0) {
          const excl = env.withoutLocals(
            ...op.bindings.flatMap((b) => LPM.patVars(b.pat)),
          )
          const bindings = op.bindings.map((b) => ({
            pat: raisePat(b.pat),
            value: raiseFrame([], excl, b.value.toReversed()),
          }))
          values.push(
            A.mkLet(
              bindings,
              raiseFrame([], excl, op.body.toReversed()),
              op.range,
              op.provenance,
            ),
          )
        } else {
          const currentValue = LPM.popRequired(values, 'the raise stack')
          const remaining = op.bindings.slice(op.idx - 1)
          const excl = env.withoutLocals(
            ...remaining.flatMap((b) => LPM.patVars(b.pat)),
          )
          const bindings = remaining.map((b, i) => ({
            pat: raisePat(b.pat),
            value:
              i === 0 ? currentValue : raiseFrame([], excl, b.value.toReversed()),
          }))
          values.push(
            A.mkLet(
              bindings,
              raiseFrame([], excl, op.body.toReversed()),
              op.range,
              op.provenance,
            ),
          )
        }
        break
      }

      case 'if': {
        const guard = LPM.popRequired(values, 'the raise stack')
        const thenExp = raiseFrame([], env, op.thenB.toReversed())
        const elseExp = raiseFrame([], env, op.elseB.toReversed())
        values.push(A.mkIf(guard, thenExp, elseExp, op.range, op.provenance))
        break
      }

      case 'hole': {
        // Only ever reached for a hole that has not run yet -- running one
        // raises -- so it reconstructs as the `??` still standing in the
        // source.
        values.push(A.mkHole(op.range))
        break
      }

      case 'pop-scope': {
        // Runtime scope bookkeeping only; nothing to reconstruct.
        break
      }

      case 'ap-spread': {
        const [fn, args] = values.splice(-2)
        values.push(A.mkApp(A.mkId('apply'), [fn, args]))
        break
      }

      case 'push-handler': {
        // N.B., no-op: the handler value stays on the reconstruction stack (it
        // was left there at runtime too) and is consumed by pop-handler below.
        break
      }

      case 'pop-handler': {
        // with-handler is now an ordinary procedure, reconstructed at its call
        // site, so these bracketing ops are transparent to reconstruction: drop
        // the (peeked) handler value, leaving the guarded result.
        const result = LPM.popRequired(values, 'the raise stack')
        values.pop()
        values.push(result)
        break
      }

    }
  }
  return LPM.popRequired(values, 'the raise stack')
}

export function raiseFrames(frames: Frame[]): A.Exp {
  if (frames.length === 0) {
    throw new LPM.ICE('raiseFrames', 'no frames to raise')
  }
  const lastFrame = frames[frames.length - 1]
  let ret = raiseFrame(
    valuesToExps(lastFrame.values, lastFrame.env),
    lastFrame.env,
    lastFrame.ops,
  )
  for (let i = frames.length - 2; i >= 0; i--) {
    const values = valuesToExps(frames[i].values, frames[i].env)
    values.push(ret)
    ret = raiseFrame(values, frames[i].env, frames[i].ops)
  }
  return ret
}

export function raiseFiber(fiber: Fiber): A.Exp {
  return raiseFrames(fiber.frames)
}

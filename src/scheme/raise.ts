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

/** @return a stack of expressions created from the given value stack. */
export function valuesToExps(values: LPM.Value[]): A.Exp[] {
  return values.map((v) => {
    if ((LPM.isFunction(v) || LPM.isClosure(v)) && v.name) {
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
        const excluded = op.restParam ? [...op.params, op.restParam] : op.params
        const body = raiseFrame(
          [],
          env.withoutLocals(...excluded),
          op.body.toReversed(),
        )
        if (op.provenance === 'anon-fn') {
          // Reconstruct the tagged lambda so sugaring recovers the `#(...)`.
          values.push(
            A.mkLam(
              op.params.map((p) => A.mkId(p)),
              body,
              op.range,
              op.restParam ? A.mkId(op.restParam) : undefined,
              'anon-fn',
            ),
          )
        } else if (op.name) {
          values.push(A.mkId(op.name))
        } else {
          values.push(
            A.mkLam(
              op.params.map((p) => A.mkId(p)),
              body,
              undefined,
              op.restParam ? A.mkId(op.restParam) : undefined,
            ),
          )
        }
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
    valuesToExps(lastFrame.values),
    lastFrame.env,
    lastFrame.ops,
  )
  for (let i = frames.length - 2; i >= 0; i--) {
    const values = valuesToExps(frames[i].values)
    values.push(ret)
    ret = raiseFrame(values, frames[i].env, frames[i].ops)
  }
  return ret
}

export function raiseFiber(fiber: Fiber): A.Exp {
  return raiseFrames(fiber.frames)
}

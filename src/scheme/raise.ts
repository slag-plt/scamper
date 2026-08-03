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
        values.push(A.mkLit(op.value))
        break
      }

      case 'var': {
        if (env.has(op.name)) {
          const v = env.get(op.name)!
          if (LPM.isFunction(v)) {
            values.push(A.mkId(op.name))
          } else {
            values.push(A.mkLit(env.get(op.name)))
          }
        } else {
          values.push(A.mkId(op.name))
        }
        break
      }

      case 'ctor': {
        const arity = op.fields.length
        const args = arity === 0 ? [] : values.splice(-arity)
        values.push(A.mkApp(A.mkId(op.name), args))
        break
      }

      case 'cls': {
        const excluded = op.restParam ? [...op.params, op.restParam] : op.params
        const body = raiseFrame(
          [],
          env.withoutLocals(...excluded),
          op.body.toReversed(),
        )
        if (op.name) {
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
        values.push(A.mkApp(head, args))
        break
      }

      case 'match': {
        const scrutinee = values.pop()!
        const matches = op.branches.map(([pat, body]) => {
          const bodyExp = raiseFrame([], env, body.toReversed())
          return { pat: raisePat(pat), body: bodyExp }
        })
        values.push(A.mkMatch(scrutinee, matches))
        break
      }

      case 'error': {
        const arg = values.pop()!
        values.push(A.mkError(arg))
        break
      }

      case 'ap-spread': {
        const [fn, args] = values.splice(-2)
        values.push(A.mkApp(A.mkId('apply'), [fn, args]))
        break
      }

      case 'check-fn': {
        // N.B., no-op: check-fn only validates the guarded function at runtime; it
        // leaves the stack ([.., handler, fn]) untouched for push-handler below.
        break
      }

      case 'push-handler': {
        // N.B., no-op: the handler value stays on the reconstruction stack (it
        // was left there at runtime too) and is consumed by pop-handler below.
        break
      }

      case 'pop-handler': {
        // Stack is [.., handler, guarded]; guarded is normally the reconstructed
        // application of `f` to its args (from the preceding `ap`). Recover it
        // into a with-handler form.
        const guarded = values.pop()!
        const handler = values.pop()!
        if (guarded.tag === 'app') {
          values.push(A.mkWithHandler(handler, guarded.head, guarded.args))
        } else {
          values.push(A.mkWithHandler(handler, guarded, []))
        }
        break
      }

      case 'popv': {
        values.pop()!
        break
      }
    }
  }
  return values.pop()!
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

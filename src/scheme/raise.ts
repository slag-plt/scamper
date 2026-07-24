import * as LPM from '../lpm'
import { Fiber } from '../lpm/fiber'
import { Frame } from '../lpm/frame'
import * as A from './ast.js'

/**
 * Lifts an LPM pattern back into a scheme-AST pattern (the inverse of
 * codegen's lowerPat). LPM patterns keep bare-string names, so each binder is
 * wrapped back into an A.Identifier carrying the pattern's range.
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

      case 'raise': {
        values.push(A.mkApp(A.mkId('raise'), [A.mkLit(op.msg)]))
        break
      }

      case 'error': {
        const arg = values.pop()!
        values.push(A.mkError(arg))
        break
      }

      case 'apply': {
        const [fn, args] = values.splice(-2)
        values.push(A.mkApply(fn, args))
        break
      }

      case 'pops': {
        // N.B., pops the local environment, but we don't track that here!
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

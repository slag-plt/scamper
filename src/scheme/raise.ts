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

/** @return the variable names bound by an LPM pattern (recursively). */
function lpmPatVars(pat: LPM.Pat): string[] {
  switch (pat.tag) {
    case 'pvar':
      return [pat.name]
    case 'pctor':
      return pat.args.flatMap(lpmPatVars)
    case 'pwild':
    case 'plit':
      return []
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
          const bodyExp = raiseFrame(
            [],
            env.withoutLocals(...lpmPatVars(pat)),
            body.toReversed(),
          )
          return { pat: raisePat(pat), body: bodyExp }
        })
        values.push(A.mkMatch(scrutinee, matches))
        break
      }

      case 'let': {
        // The k binding values were reconstructed inline before this op.
        const vals =
          op.patterns.length === 0 ? [] : values.splice(-op.patterns.length)
        const bindings = op.patterns.map((pat, i) => ({
          pat: raisePat(pat),
          value: vals[i],
        }))
        // Exclude the binders so their occurrences in the body render as names,
        // not substituted values (they shadow any same-named outer binding).
        const body = raiseFrame(
          [],
          env.withoutLocals(...op.patterns.flatMap(lpmPatVars)),
          op.body.toReversed(),
        )
        values.push(A.mkLet(bindings, body))
        break
      }

      case 'if': {
        const guard = values.pop()!
        const thenExp = raiseFrame([], env, op.thenB.toReversed())
        const elseExp = raiseFrame([], env, op.elseB.toReversed())
        values.push(A.mkIf(guard, thenExp, elseExp))
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
        const result = values.pop()!
        values.pop()
        values.push(result)
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

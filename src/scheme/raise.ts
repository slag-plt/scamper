import * as LPM from '../lpm'
import * as A from './ast.js'

/** @return a stack of expressions created from the given value stack. */
export function valuesToExps (values: LPM.Value[]): A.Exp[] {
  return values.map((v) => {
    if ((LPM.isFunction(v) || LPM.isClosure(v)) && v.name) {
      return A.mkVar(v.name)
    } else {
      return A.mkLit(v)
    }
  })
}

export function raiseFrame (values: A.Exp[], ops: LPM.Ops[]): A.Exp { 
  for (let i = ops.length - 1; i >= 0; i--) {
    const op = ops[i]
    switch (op.tag) {
      case 'lit': {
        values.push(A.mkLit(op.value))
        break
      }

      case 'var': {
        values.push(A.mkVar(op.name))
        break
      }

      case 'ctor': {
        const arity = op.fields.length
        const args = arity === 0 ? [] : values.splice(-arity)
        values.push(A.mkApp(A.mkVar(op.name), args))
        break
      }

      case 'cls': {
        const body = raiseFrame([], op.body)
        // TODO: maybe lambdas need to carry names for raising purposes?
        values.push(A.mkLam(op.params, body))
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
          const bodyExp = raiseFrame([], body)
          return { pat, body: bodyExp }
        })
        values.push(A.mkMatch(scrutinee, matches))
        break
      }

      case 'raise': {
        values.push(A.mkApp(A.mkVar('raise'), [A.mkLit(op.msg)]))
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

export function raiseFrames (frames: LPM.Frame[]): A.Exp {
  if (frames.length === 0) {
    throw new LPM.ICE('raiseFrames', 'no frames to raise')
  }
  const lastFrame = frames[frames.length - 1]
  let ret = raiseFrame(valuesToExps(lastFrame.values), lastFrame.ops)
  for (let i = frames.length - 2; i >= 0; i--) {
    const values = valuesToExps(frames[i].values)
    values.push(ret)
    ret = raiseFrame(values, frames[i].ops)
  }
  return ret
}
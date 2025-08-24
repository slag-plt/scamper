import * as L from './lang.js'
import * as U from './util.js'
import { ICE, ScamperError } from './error.js'
import { OutputChannel, ErrorChannel } from './output.js'

/** The Little Pattern Machine */
export class Machine {
  builtinLibs: Map<string, [string, L.Value][]>
  maxCallStackDepth: number
  out: OutputChannel
  err: ErrorChannel
  mainThread: L.Thread

  constructor (builtinLibs: Map<string, [string, L.Value][]>, env: L.Env, exp: L.Exp, out: OutputChannel, err: ErrorChannel, maxCallStackDepth = 10000) {
    this.builtinLibs = builtinLibs
    this.out = out
    this.err = err
    this.maxCallStackDepth = maxCallStackDepth
    this.mainThread = new L.Thread('##main##', env, exp)
  }

  isFinished (): boolean {
    return this.mainThread.isFinished() 
  }

  static tryMatch (v: L.Value, p: L.Pat): [string, L.Value][] | undefined {
    switch (p.tag) {
      case 'pwild': {
        return []
      }

      case 'plit': {
        if (U.equals(v, p.value)) {
          return []
        } else {
          return undefined
        }
      }

      case 'pvar': {
        return [[p.name, v]]
      }

      case 'pctor': {
        if (U.isStructKind(v, p.name)) {
          const flds = U.getFieldsOfStruct(v)
          if (flds.length !== p.args.length) {
            return undefined
          }
          const bindings: [string, L.Value][] = []
          for (let i = 0; i < flds.length; i++) {
            const pat = p.args[i]
            const val = v[flds[i]]
            const match = Machine.tryMatch(val, pat)
            if (!match) { return undefined }
            bindings.push(...match)
          }
          return bindings
        } else {
          return undefined
        }
      }
    }
  }

  stepThread (thread: L.Thread): void {
    const current = thread.getCurrentFrame()
    if (current.isFinished()) {
      if (current.values.length !== 1) {
        throw new ICE('Machine.stepThread', 'Thread did not finish with exactly one value on the stack')
      }
      const ret = current.values.pop()
      thread.pop()
      if (thread.isFinished()) {
        thread.result = ret
      } else {
        thread.getCurrentFrame().values.push(ret)
      }
      return
    }
    const instr = current.popInstr()
    switch (instr.tag) {
      case 'lit': {
        current.values.push(instr.value)
        break
      }
      
      case 'var': {
        if (!current.env.has(instr.name)) {
          throw new ScamperError('Runtime', `Variable not found: ${instr.name}`)
        } else {
          current.values.push(current.env.get(instr.name)!)
        }
        break
      }
      
      case 'ctor':
        current.values.push(U.mkStruct(
          instr.name, instr.fields, current.values.splice(-instr.fields.length)))
        break
      
      case 'cls': {
        current.values.push(U.mkClosure(
          instr.params,
          instr.body,
          current.env,
          (...args: L.Value[]): L.Value => {
            return this.evaluateThread(new L.Thread(
              instr.name ?? '##anonymous##',
              current.env.extend(...instr.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              instr.body
            ))
          }
        ))
        break
      }
      
      case 'ap': {
        if (current.values.length < instr.numArgs + 1) {
          throw new ICE('Machine.stepThread', `Not enough values for application: ${instr.numArgs + 1}`) 
        }
        const values = current.values.splice(-(instr.numArgs + 1))
        const fn = values[0]
        const args = values.splice(-instr.numArgs)
        if (typeof fn === 'function') {
          current.values.push(fn(...args))
        } else if (U.isClosure(fn)) {
          if (thread.frames.length >= this.maxCallStackDepth) {
            throw new ScamperError('Runtime', `Maximum call stack depth ${this.maxCallStackDepth} exceeded`)
          } else if (current.isFinished()) {
            // N.B., if this thread is finished, then tail-call optimize by
            // overwriting the current frame instead of pushing a new one.
            current.name = fn.name ?? '##anonymous##'
            current.env = new L.Env(current.env.parent)
            fn.params.forEach((x, i) => {
              current.env.set(x, args[i])
            })
            current.pushExp(fn.code)
          } else {
            thread.push(
              fn.name ?? '##anonymous##',
              fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              fn.code
            )
          }
        } else {
          throw new ScamperError('Runtime', `Not a function or closure: ${JSON.stringify(fn)}`)
        }
        break
      }
      
      case 'match': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Match requires at least one value')
        }
        const scrutinee = current.values.pop()!
        const bindings = Machine.tryMatch(scrutinee, instr.pattern)
        if (bindings) {
          current.env = current.env.extend(...bindings)
          current.pushExp(instr.ifB)
        } else {
          current.pushExp(instr.elseB)
        }
        break
      }

      case 'disp': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Display requires at least one value')
        }
        this.out.send(current.values.pop()!)
        break
      }
      
      case 'raise': {
        throw new ScamperError('Runtime', instr.msg)
      }
      
      case 'pop': {
        current.env = current.env.pop()
        break
      }
    }
  }

  evaluateThread (thread: L.Thread): L.Value {
    while (!thread.isFinished()) {
      this.stepThread(thread)
    }
    return thread.result
  }

  step (): void { this.stepThread(this.mainThread) }
  evaluate (): L.Value { return this.evaluateThread(this.mainThread) }
}
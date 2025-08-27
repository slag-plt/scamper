import * as L from './lang.js'
import * as U from './util.js'
import { ICE, ScamperError } from './error.js'
import { OutputChannel, ErrorChannel } from './output.js'

/** The Little Pattern Machine */
export class Machine {
  builtinLibs: Map<string, L.Library>
  maxCallStackDepth: number
  stepMatch: boolean
  out: OutputChannel
  err: ErrorChannel
  mainThread: L.Thread

  constructor (builtinLibs: Map<string, L.Library>, env: L.Env, blk: L.Blk,
               out: OutputChannel, err: ErrorChannel, maxCallStackDepth = 10000, stepMatch = false) {
    this.builtinLibs = builtinLibs
    this.out = out
    this.err = err
    this.maxCallStackDepth = maxCallStackDepth
    this.stepMatch = stepMatch
    this.mainThread = new L.Thread('##main##', env, blk)
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

  reportAndUnwind (thread: L.Thread, err: ScamperError) {
    this.err.report(err)
    thread.unwindToNextStatement()
  }

  stepThread (thread: L.Thread): void {
    const current = thread.getCurrentFrame()
    if (current.isFinished()) {
      if (current.values.length !== 1) {
        throw new ICE('Machine.stepThread', `Thread must finish with exactly one value on the stack, finished with ${current.values.length} instead`)
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
          this.reportAndUnwind(thread, new ScamperError('Runtime', `Variable not found: ${instr.name}`))
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
        const args = instr.numArgs === 0 ? [] : values.splice(-instr.numArgs)
        if (typeof fn === 'function') {
          let result = undefined 
          try {
            result = fn(...args)
          } catch (e) {
            if (e instanceof ScamperError) {
              // N.B., annotate the error from the runtime with additional info
              e.source = fn.name ?? '##anonymous##'
              e.range = instr.range
              this.reportAndUnwind(thread, e)
            } else {
            this.reportAndUnwind(thread,
              new ScamperError('Runtime', `Error applying function: ${e}`, undefined, instr.range, fn.name ?? '##anonymous##'))
            }
            return
          }
          current.values.push(result)
        } else if (U.isClosure(fn)) {
          if (thread.frames.length >= this.maxCallStackDepth) {
            this.reportAndUnwind(thread, new ScamperError('Runtime', `Maximum call stack depth ${this.maxCallStackDepth} exceeded`))
            return
          } else if (current.isFinished()) {
            // N.B., if this thread is finished, then tail-call optimize by
            // overwriting the current frame instead of pushing a new one.
            current.name = fn.name ?? '##anonymous##'
            current.env = new L.Env(current.env.parent)
            fn.params.forEach((x, i) => {
              current.env.set(x, args[i])
            })
            current.pushBlk(fn.code)
          } else {
            thread.push(
              fn.name ?? '##anonymous##',
              fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              fn.code
            )
          }
        } else {
          this.reportAndUnwind(thread, new ScamperError('Runtime', `Not a function or closure: ${JSON.stringify(fn)}`))
        }
        break
      }
      
      case 'match': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Match requires at least one value')
        }
        const scrutinee = current.values.pop()!
        if (instr.branches.length === 0) {
          this.reportAndUnwind(thread, new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
          return
        }
        if (this.stepMatch) {
          const [pat, blk] = instr.branches[0]
          instr.branches = instr.branches.slice(1)
          const bindings = Machine.tryMatch(scrutinee, pat)
          if (bindings) {
            current.env = current.env.extend(...bindings)
            current.pushBlk(blk)
          } else {
            current.pushBlk([instr])
          }
        } else {
          for (const [pat, blk] of instr.branches) {
            const bindings = Machine.tryMatch(scrutinee, pat)
            if (bindings) {
              current.env = current.env.extend(...bindings)
              current.pushBlk(blk)
              return
            }
          }
          this.reportAndUnwind(thread, new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
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

      case 'define': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Define requires at least one value')
        }
        current.env.set(instr.name, current.values.pop()!)
        break
      }

      case 'import': {
        if (this.builtinLibs.has(instr.name)) {
          const lib = this.builtinLibs.get(instr.name)!
          if (lib.initializer) { lib.initializer() }
          for (const [name, value] of lib.lib) {
            current.env.set(name, value)
          }
        }
        break
      }
      
      case 'raise': {
        this.reportAndUnwind(thread, new ScamperError('Runtime', instr.msg))
        break
      }
      
      case 'pops': {
        current.env = current.env.pop()
        break
      }

      case 'popv': {
        current.values.pop()
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
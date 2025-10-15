import * as L from './lang.js'
import * as U from './util.js'
import { ICE, ScamperError } from './error.js'
import { OutputChannel, ErrorChannel } from './output.js'
import { mkTraceOutput } from './trace.js'

export type Options = {
  maxCallStackDepth: number
  stepMatch: boolean
  isTracing: boolean
  raisingTarget: string
}

export const defaultOptions: Options = {
  maxCallStackDepth: 10000,
  stepMatch: false,
  isTracing: false,
  raisingTarget: 'scheme'
}

/** The type of functions responsible for raising LPM threads back to a surface language. */
type Raiser = (thread: L.Thread) => L.Value

/** The Little Pattern Machine */
export class Machine {
  builtinLibs: Map<string, L.Library>
  raisingProviders: Map<string, Raiser>
  options: Options
  out: OutputChannel
  err: ErrorChannel
  mainThread: L.Thread

  constructor (builtinLibs: Map<string, L.Library>, raisingProviders: Map<string, Raiser>, env: L.Env, prog: L.Prog,
               out: OutputChannel, err: ErrorChannel, options: Options = defaultOptions) {
    this.builtinLibs = builtinLibs
    this.raisingProviders = raisingProviders
    this.out = out
    this.err = err
    this.options = options
    this.mainThread = new L.Thread('##main##', env, prog)
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

  stepThread (thread: L.Thread): boolean {
    if (thread.isFinished()) { return false }
    const stmt = thread.getCurrentStmt()
    switch (stmt.tag) {
      case 'disp': {
        if (thread.frames.length > 0) {
          return this.stepFrame(thread)
        } else {
          const result = thread.results[thread.curStmt]
          this.out.send(result)
          thread.advanceStmt()
          return false
        }
      }

      case 'import': {
        if (this.builtinLibs.has(stmt.name)) {
          const lib = this.builtinLibs.get(stmt.name)!
          if (lib.initializer) { lib.initializer() }
          for (const [name, value] of lib.lib) {
            thread.env.set(name, value)
          }
        }
        thread.advanceStmt()
        return false
      }

      case 'define': {
        if (thread.frames.length > 0) {
          return this.stepFrame(thread)
        } else {
          const result = thread.results[thread.curStmt]
          thread.env.set(stmt.name, result)
          thread.advanceStmt()
          return false
        }
      }

      case 'stmtexp': {
        if (thread.frames.length > 0) {
          return this.stepFrame(thread)
        } else {
          thread.advanceStmt()
          return false
        }
      }
    }
  }

  outputTrace (thread: L.Thread) {
    if (this.options.isTracing) {
      const raiser = this.raisingProviders.get(this.options.raisingTarget)!
      const source = raiser(thread)
      this.out.send(mkTraceOutput(source))
    }
  }

  stepFrame (thread: L.Thread): boolean {
    const current = thread.getCurrentFrame()
    if (current.isFinished()) {
      if (current.values.length !== 1) {
        throw new ICE('Machine.stepFrame', `Frame must finish with exactly one value on the stack, finished with ${current.values.length} instead`)
      }
      const ret = current.values.pop()
      thread.pop()
      if (thread.frames.length === 0) {
        thread.results[thread.curStmt] = ret
      } else {
        thread.getCurrentFrame().values.push(ret)
      }
      return false
    }
    const instr = current.popInstr()
    switch (instr.tag) {
      case 'lit': {
        current.values.push(instr.value)
        return false
      }
      
      case 'var': {
        if (!current.env.has(instr.name)) {
          this.reportAndUnwind(thread, new ScamperError('Runtime', `Variable not found: ${instr.name}`))
        } else {
          current.values.push(current.env.get(instr.name)!)
        }
        return false
      }
      
      case 'ctor': {
        current.values.push(U.mkStruct(
          instr.name, instr.fields, current.values.splice(-instr.fields.length)))
        return false
      }
      
      case 'cls': {
        current.values.push(U.mkClosure(
          instr.params,
          instr.body,
          current.env,
          (...args: L.Value[]): L.Value => {
            return this.evaluateThread(new L.Thread(
              instr.name ?? '##anonymous##',
              current.env.extend(...instr.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              [U.mkStmtExp(instr.body, instr.range)]
            ))
          }
        ))
        return false
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
          }
          current.values.push(result)
          return false
        } else if (U.isClosure(fn)) {
          if (thread.frames.length >= this.options.maxCallStackDepth) {
            this.reportAndUnwind(thread, new ScamperError('Runtime', `Maximum call stack depth ${this.options.maxCallStackDepth} exceeded`))
            return false
          } else if (current.isFinished()) {
            // N.B., if this thread is finished, then tail-call optimize by
            // overwriting the current frame instead of pushing a new one.
            current.name = fn.name ?? '##anonymous##'
            current.env = fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][])
            current.pushBlk(fn.code)
            return false
          } else {
            thread.push(
              fn.name ?? '##anonymous##',
              fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              fn.code
            )
            return false
          }
        } else {
          this.reportAndUnwind(thread, new ScamperError('Runtime', `Not a function or closure: ${JSON.stringify(fn)}`))
          return false
        }
      }
      
      case 'match': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Match requires at least one value')
        }
        const scrutinee = current.values.pop()!
        if (instr.branches.length === 0) {
          this.reportAndUnwind(thread, new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
          return false
        }
        // N.B., if we step a match, then we only peel one branch off at a time
        if (this.options.stepMatch) {
          const [pat, blk] = instr.branches[0]
          instr.branches = instr.branches.slice(1)
          const bindings = Machine.tryMatch(scrutinee, pat)
          if (bindings) {
            current.env = current.env.extend(...bindings)
            current.pushBlk(blk)
          } else {
            current.pushBlk([instr])
          }
          return false
        } else {
          for (const [pat, blk] of instr.branches) {
            const bindings = Machine.tryMatch(scrutinee, pat)
            if (bindings) {
              current.env = current.env.extend(...bindings)
              current.pushBlk(blk)
              return false
            }
          }
          this.reportAndUnwind(thread, new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
          return false
        }
      }

      case 'raise': {
        this.reportAndUnwind(thread, new ScamperError('Runtime', instr.msg))
        return false
      }
      
      case 'pops': {
        current.env = current.env.pop()
        return true
      }

      case 'popv': {
        current.values.pop()
        return true
      }
    }
  }

  evaluateThread (thread: L.Thread): L.Value {
    while (!thread.isFinished()) {
      this.stepThread(thread)
    }
    // N.B., return the results of the last statement of the thread as the final result
    return thread.results[thread.curStmt - 1]
  }

  step (): void { 
    let shouldContinue = true
    while (shouldContinue)  {
      shouldContinue = this.stepThread(this.mainThread)
    }
  }

  evaluate (): L.Value { return this.evaluateThread(this.mainThread) }
}
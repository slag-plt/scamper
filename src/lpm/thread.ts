import { ICE, ScamperError } from './error.js'
import * as L from './lang.js'
import { OutputChannel, ErrorChannel } from './output/index.js'
import { Raiser } from './raiser.js'
import { mkTraceOutput } from './trace.js'
import * as U from './util.js'

/** The type of runtime options. */
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

export function cloneOptions(opt: Options): Options {
  const { maxCallStackDepth, stepMatch, isTracing, raisingTarget } = opt
  return { maxCallStackDepth, stepMatch, isTracing, raisingTarget }
}

/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export class Frame {
  name: string
  env: L.Env
  values: L.Value[]
  ops: L.Ops[]

  constructor(name: string, env: L.Env, blk: L.Blk) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = blk.toReversed()
  }

  isFinished(): boolean {
    return this.ops.length === 0
  }

  pushBlk(blk: L.Blk) {
    this.ops.push(...blk.toReversed())
  }

  popInstr(): L.Ops {
    return this.ops.pop()!
  }
}

/** A single thread of execution in LPM. */
export class Thread {
  name: string
  options: Options
  builtinLibs: Map<string, L.Library>
  out: OutputChannel
  err: ErrorChannel
  raisingProviders: Map<string, Raiser<any>>
  prog: L.Prog
  curStmt: number
  env: L.Env
  frames: L.Frame[]
  results: L.Value[]
  lastStep: L.Value | undefined

  constructor(name: string, env: L.Env, prog: L.Prog, options: Options, builtinLibs: Map<string, L.Library>, out: OutputChannel, err: ErrorChannel, raisingProviders: Map<string, Raiser<any>>) {
    this.name = name
    this.prog = prog
    this.options = options
    this.builtinLibs = builtinLibs
    this.out = out
    this.err = err
    this.raisingProviders = raisingProviders
    this.curStmt = 0
    this.env = env
    this.frames = []
    this.results = []
    this.lastStep = undefined
    this.setupNextStmt()
  }

  setupNextStmt(): void {
    const stmt = this.getCurrentStmt()
    switch (stmt.tag) {
      case 'disp': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
      case 'import': {
        // N.B., no frame setup required
        break
      }
      case 'define': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
      case 'stmtexp': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
    }
  }

  advanceStmt(): void {
    this.frames = []
    this.curStmt++
    if (!this.isFinished()) { this.setupNextStmt() }
  }

  getCurrentStmt(): L.Stmt {
    return this.prog[this.curStmt]
  }

  isFinished(): boolean {
    return this.curStmt >= this.prog.length
  }

  getCurrentFrame(): L.Frame {
    return this.frames[this.frames.length - 1]
  }

  push(name: string, env: L.Env, blk: L.Blk): void {
    this.frames.push(new L.Frame(name, env, blk))
  }

  pop(): void {
    this.frames.pop()
  }

  unwindToNextStatement(): void {
    this.frames = []
    this.results.push(undefined)
    this.advanceStmt()
  }

  reportAndUnwind(err: ScamperError) {
    this.err.report(err)
    this.unwindToNextStatement()
  }

  ///// Evaluation /////////////////////////////////////////////////////////////

  evaluate (): L.Value {
    while (!this.isFinished()) {
      this.stepThread()
    }
    // N.B., return the results of the last statement of the thread as the final result
    return this.results[this.curStmt - 1]
  }

  stepWithTrace (): void { 
    this.stepThread()
    const provider = this.raisingProviders.get(this.options.raisingTarget)!
    let raisedStep = this.frames.length  > 0 ? provider.raise(this) : undefined
    if (!this.isFinished() && raisedStep !== undefined && provider.equals(this.lastStep, raisedStep)) {
      this.stepThread()
    }
    if (this.options.isTracing && raisedStep !== undefined) {
      this.out.send(mkTraceOutput(raisedStep))
    }
    this.lastStep = raisedStep
  }

  evaluateWithTrace (): void {
    while (!this.isFinished()) {
      this.stepWithTrace()
    }
  }

  evaluateSubthread(name: string, env: L.Env, prog: L.Prog): L.Value {
    // N.B., disable tracing for subthread evaluation!
    const newOpts = cloneOptions(this.options)
    newOpts.isTracing = false
    newOpts.stepMatch = false
    const subthread = new Thread(name, env, prog, newOpts, this.builtinLibs, this.out, this.err, this.raisingProviders)
    return subthread.evaluate()
  }

  ///// Stepping ///////////////////////////////////////////////////////////////

  static tryMatch(v: L.Value, p: L.Pat): [string, L.Value][] | undefined {
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
            const match = Thread.tryMatch(val, pat)
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

  stepThread(): boolean {
    if (this.isFinished()) { return false }
    const stmt = this.getCurrentStmt()
    switch (stmt.tag) {
      case 'disp': {
        if (this.frames.length > 0) {
          return this.stepFrame()
        } else {
          const result = this.results[this.curStmt]
          this.out.send(result)
          this.advanceStmt()
          return false
        }
      }

      case 'import': {
        if (this.builtinLibs.has(stmt.name)) {
          const lib = this.builtinLibs.get(stmt.name)!
          if (lib.initializer) { lib.initializer() }
          for (const [name, value] of lib.lib) {
            this.env.set(name, value)
          }
        }
        this.advanceStmt()
        return false
      }

      case 'define': {
        if (this.frames.length > 0) {
          return this.stepFrame()
        } else {
          const result = this.results[this.curStmt]
          this.env.set(stmt.name, result)
          this.advanceStmt()
          return false
        }
      }

      case 'stmtexp': {
        if (this.frames.length > 0) {
          return this.stepFrame()
        } else {
          this.advanceStmt()
          return false
        }
      }
    }
  }

  stepFrame(): boolean {
    const current = this.getCurrentFrame()
    if (current.isFinished()) {
      if (current.values.length !== 1) {
        throw new ICE('Machine.stepFrame', `Frame must finish with exactly one value on the stack, finished with ${current.values.length} instead`)
      }
      const ret = current.values.pop()
      this.pop()
      if (this.frames.length === 0) {
        this.results[this.curStmt] = ret
      } else {
        this.getCurrentFrame().values.push(ret)
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
          this.reportAndUnwind(new ScamperError('Runtime', `Variable not found: ${instr.name}`))
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
            return this.evaluateSubthread(
              instr.name ?? '##anonymous##',
              current.env.extend(...instr.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              [U.mkStmtExp(instr.body, instr.range)]
            )
          }))
        return false
      }

      case 'ap': {
        if (this.frames.length >= this.options.maxCallStackDepth) {
          this.reportAndUnwind(new ScamperError('Runtime', `Maximum call stack depth ${this.options.maxCallStackDepth} exceeded`))
          return false
        }
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
              this.reportAndUnwind(e)
            } else {
              this.reportAndUnwind(
                new ScamperError('Runtime', `Error applying function: ${e}`, undefined, instr.range, fn.name ?? '##anonymous##'))
            }
          }
          current.values.push(result)
          return false
        } else if (U.isClosure(fn)) {
          if (this.frames.length >= this.options.maxCallStackDepth) {
            this.reportAndUnwind(new ScamperError('Runtime', `Maximum call stack depth ${this.options.maxCallStackDepth} exceeded`))
            return false
          } else if (fn.params.length !== args.length) {
            this.reportAndUnwind(new ScamperError('Runtime', `Arity mismatch in function call: expected ${fn.params.length} arguments but got ${args.length}`))
            return false
          } else if (current.isFinished()) {
            // N.B., if this thread is finished, then tail-call optimize by
            // overwriting the current frame instead of pushing a new one.
            current.name = fn.name ?? '##anonymous##'
            current.env = fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][])
            current.pushBlk(fn.code)
            return false
          } else {
            this.push(
              fn.name ?? '##anonymous##',
              fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
              fn.code
            )
            return false
          }
        } else {
          this.reportAndUnwind(new ScamperError('Runtime', `Not a function or closure: ${JSON.stringify(fn)}`))
          return false
        }
      }

      case 'match': {
        if (current.values.length === 0) {
          throw new ICE('Machine.stepThread', 'Match requires at least one value')
        }
        const scrutinee = current.values.pop()!
        if (instr.branches.length === 0) {
          this.reportAndUnwind(new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
          return false
        }
        // N.B., if we step a match, then we only peel one branch off at a time
        if (this.options.stepMatch) {
          const [pat, blk] = instr.branches[0]
          instr.branches = instr.branches.slice(1)
          const bindings = Thread.tryMatch(scrutinee, pat)
          if (bindings) {
            current.env = current.env.extend(...bindings)
            current.pushBlk(blk)
          } else {
            current.pushBlk([instr])
          }
          return false
        } else {
          for (const [pat, blk] of instr.branches) {
            const bindings = Thread.tryMatch(scrutinee, pat)
            if (bindings) {
              current.env = current.env.extend(...bindings)
              current.pushBlk(blk)
              return false
            }
          }
          this.reportAndUnwind(new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
          return false
        }
      }

      case 'raise': {
        this.reportAndUnwind(new ScamperError('Runtime', instr.msg))
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
}
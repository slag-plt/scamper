import {ICE, ScamperError} from './error.js'
import * as L from './lang.js'
import { OutputChannel, ErrorChannel } from './output/index.js'
import { Raiser } from './raiser.js'
import { mkTraceStart, mkTraceOutput } from './trace.js'
import * as U from './util.js'
import {SimpleErrorChannel} from "./output/simple-error";

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

export function cloneOptions (opt: Options): Options {
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

  constructor (name: string, env: L.Env, blk: L.Blk) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = blk.toReversed()
  }

  isFinished(): boolean {
    return this.ops.length === 0
  }

  pushBlk (blk: L.Blk) {
    this.ops.push(...blk.toReversed())
  }

  popInstr (): L.Ops {
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
  frames: Frame[]
  results: L.Value[]
  isProcessingExpr: boolean

  constructor(name: string, env: L.Env, prog: L.Prog, options: Options,
              builtinLibs: Map<string, L.Library>, out: OutputChannel,
              err: ErrorChannel, raisingProviders: Map<string, Raiser<any>>) {
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
    this.isProcessingExpr = false
  }

  /** Advances this thread to the next statement. */
  advanceStmt(): void {
    this.frames = []
    this.isProcessingExpr = false
    this.curStmt++
    if (!this.isFinished()) {
      if (this.options.isTracing) { this.out.popLevel() }
    }
  }

  /** @return the current statement being executed by this thread. */
  getCurrentStmt(): L.Stmt {
    return this.prog[this.curStmt]
  }

  /** @return `true` iff this thread has finished executing. */
  isFinished(): boolean {
    return this.curStmt >= this.prog.length
  }

  getCurrentFrame(): Frame {
    return this.frames[this.frames.length - 1]
  }

  push(name: string, env: L.Env, blk: L.Blk): void {
    this.frames.push(new Frame(name, env, blk))
  }

  pop(): void {
    this.frames.pop()
  }

  reportAndUnwind(err: ScamperError) {
    this.err.report(err)
    this.frames = []
    this.isProcessingExpr = false
    this.results.push(undefined)
    this.advanceStmt()
  }

  ///// Evaluation /////////////////////////////////////////////////////////////

  step (): void {
    this.stepThread()
    if (this.options.isTracing && this.isProcessingExpr) {
      const provider = this.raisingProviders.get(this.options.raisingTarget)!
      if (this.frames.length > 0) {
        this.out.send(mkTraceOutput(provider.raise(this)!))
      } else {
        this.out.send(mkTraceOutput(this.results[this.curStmt]))
      }
    }
  }

  evaluate (): L.Value {
    while (!this.isFinished()) {
      this.step()
    }
    // N.B., return the results of the last statement of the thread as the final result
    return this.results[this.curStmt - 1]
  }

  evaluateSubthread(name: string, env: L.Env, prog: L.Prog): L.Value {
    // N.B., disable tracing for subthread evaluation!
    const newOpts = cloneOptions(this.options)
    newOpts.isTracing = false
    newOpts.stepMatch = false
    const errChannel = new SimpleErrorChannel()
    const subthread = new Thread(name, env, prog, newOpts, this.builtinLibs, this.out, errChannel, this.raisingProviders)
    const result = subthread.evaluate()
    const errs = errChannel.errors;
    if (errs.length > 0) {
      throw errChannel.getSubthreadErrors()
    }
    return result
  }

  ///// Stepping ///////////////////////////////////////////////////////////////

  /**
   * Checks if the thread is currently processing an expression. If not, it
   * begins processing the given expression.
   * @param preamble the preamble to use when tracing
   * @param expr the expression to proces next
   */
  private checkIfProcessingExpr (preamble: string, expr: L.Blk): void {
    if (!this.isProcessingExpr) {
      this.isProcessingExpr = true
      this.push(`##stmt_${this.curStmt}##`, this.env, expr)
      if (this.options.isTracing) {
        this.out.pushLevel('trace-block')
        this.out.send(mkTraceStart(
          preamble,
          this.raisingProviders.get(this.options.raisingTarget)!.raise(this)))
      }
    }
  }

  /**
   * Pattern matches value `v` against pattern `p`, producing a list of bindings
   * if successful, or `undefined` if the match fails.
   * @param v the scrutinee value
   * @param p the pattern value
   * @returns a list of bindings if successful or `undefined` if the match fails
   */
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

  /**
   * Steps this thread forward once. Assumes that the thread has already been
   * advanced to the next statement, if necessary.
   */
  private stepThread (): void {
    if (this.isFinished()) { return }
    const stmt = this.getCurrentStmt()
    switch (stmt.tag) {
      // disp case
      case 'disp': {
        this.checkIfProcessingExpr('Displaying', stmt.expr)
        if (this.frames.length > 0) {
          this.stepFrame()
        } else {
          const result = this.results[this.curStmt]
          this.out.send(result)
          this.advanceStmt()
        }
        return
      }

      // import case
      case 'import': {
        if (this.builtinLibs.has(stmt.name)) {
          const lib = this.builtinLibs.get(stmt.name)!
          if (lib.initializer) { lib.initializer() }
          for (const [name, value] of lib.lib) {
            this.env.set(name, value)
          }
        }
        if (this.options.isTracing) {
          this.out.pushLevel('trace-block')
          this.out.send(`Imported library: ${stmt.name}`)
          // we don't pop here because advanceStmt pops for us
        }
        this.advanceStmt()
        return
      }

      // define case
      case 'define': {
        this.checkIfProcessingExpr(`Defining ${stmt.name} as`, stmt.expr)
        if (this.frames.length > 0) {
          this.stepFrame()
        } else {
          const result = this.results[this.curStmt]
          this.env.set(stmt.name, result)
          this.advanceStmt()
        }
        return
      }

      // stmtexp case
      case 'stmtexp': {
        this.checkIfProcessingExpr(`Evaluating`, stmt.expr)
        if (this.frames.length > 0) {
          this.stepFrame()
        } else {
          this.advanceStmt()
        }
        return
      }
    }
  }

  /**
   * Checks whether the current frame is finished, and if so, pops it, setting
   * the return value appropriately.
   * @returns `true` iff the frame returned
   */
  private checkFrameReturn(): boolean {
    // N.B., LPM has no explicit return instruction; a frame is finished when
    // it runs out of instructions
    const current = this.getCurrentFrame()
    if (!current || !current.isFinished()) {
      return false
    } else {
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
      return true
    }
  }

  private stepFrame(): void {
    const current = this.getCurrentFrame()
    // N.B., continue stepping until a "major" step occurs where the program
    // state changes significantly. Probably should make this an option (where
    // skipping is the default.)
    let cont = true
    while (cont && !this.getCurrentFrame().isFinished()) {
      const instr = current.popInstr()
      switch (instr.tag) {
        // lit case (minor step)
        case 'lit': {
          current.values.push(instr.value)
          break
        }

        // var case (minor step)
        case 'var': {
          if (!current.env.has(instr.name)) {
            this.reportAndUnwind(new ScamperError('Runtime', `Variable not found: ${instr.name}`))
          } else {
            current.values.push(current.env.get(instr.name)!)
          }
          break
        }

        // ctor case (minor step)
        case 'ctor': {
          current.values.push(U.mkStruct(
            instr.name, instr.fields, current.values.splice(-instr.fields.length)))
          break
        }

        // cls case (minor step)
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
          break 
        }

        // ap case (major step)
        case 'ap': {
          cont = false
          if (this.frames.length >= this.options.maxCallStackDepth) {
            this.reportAndUnwind(new ScamperError('Runtime', `Maximum call stack depth ${this.options.maxCallStackDepth} exceeded`))
            break
          }
          if (current.values.length < instr.numArgs + 1) {
            // NOTE: should this be a runtime error instead of an ICE?
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
          } else if (U.isClosure(fn)) {
            if (this.frames.length >= this.options.maxCallStackDepth) {
              this.reportAndUnwind(new ScamperError('Runtime', `Maximum call stack depth ${this.options.maxCallStackDepth} exceeded`))
            } else if (fn.params.length !== args.length) {
              this.reportAndUnwind(new ScamperError('Runtime', `Arity mismatch in function call: expected ${fn.params.length} arguments but got ${args.length}`))
            } else if (current.isFinished()) {
              // N.B., if this thread is finished, then tail-call optimize by
              // overwriting the current frame instead of pushing a new one.
              current.name = fn.name ?? '##anonymous##'
              current.env = fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][])
              current.pushBlk(fn.code)
            } else {
              this.push(
                fn.name ?? '##anonymous##',
                fn.env.extend(...fn.params.map((p, i) => [p, args[i]]) as [string, L.Value][]),
                fn.code
              )
            }
          } else {
            this.reportAndUnwind(new ScamperError('Runtime', `Not a function or closure: ${JSON.stringify(fn)}`))
          }
          break 
        }

        // match case (major step)
        case 'match': {
          cont = false
          if (current.values.length === 0) {
            throw new ICE('Machine.stepThread', 'Match requires at least one value')
          }
          const scrutinee = current.values.pop()!
          if (instr.branches.length === 0) {
            this.reportAndUnwind(new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
            break
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
          } else {
            let successful = false
            for (const [pat, blk] of instr.branches) {
              const bindings = Thread.tryMatch(scrutinee, pat)
              if (bindings) {
                current.env = current.env.extend(...bindings)
                current.pushBlk(blk)
                successful = true
                break
              }
            }
            if (!successful) {
              this.reportAndUnwind(new ScamperError('Runtime', 'Inexhaustive pattern match failure'))
            }
          }
          break 
        }

        case 'raise': {
          cont = false
          this.reportAndUnwind(new ScamperError('Runtime', instr.msg))
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
      cont = !this.checkFrameReturn() && cont
    }
  }
}
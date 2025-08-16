import Ops from './ops.js'
import * as R from './runtime.js'

/** Globals are a mapping from identifiers to values. */
export type Globals = Map<R.Id, R.Value>

/**
 * An execution frame captures the ongoing execution of a function call:
 * + `ops`: the code currently being executed.
 * + `pc`: the program counter that tracks the current instructions.
 * + `values`: the value stack that holds intermediate results.
 * + `env`: the local environment.
 */
export type Frame = {
  code: R.Code,
  pc: number,
  values: R.Value[],
  env: R.Env,
}

/** Runtime options for the LPM */
export type Options = {
  maxArgs: number,
  maxCallStackDepth: number,
}

/**
 * An implementor of the Output interface is capable of receiving and
 * "rendering" LPM values that are displayed (via the `disp` op).
 */
export interface Output {
  send (v: R.Value): void
}

/**
 * Tries to match a pattern against a value. A pattern is a value that
 * contains pattern variables (`PVar`s).
 * @param p the pattern
 * @param v the value
 * @param range the range of the pattern match in source code, if available
 * @returns an array of bindings, mapping local variable indices to bound
 *          values or `undefined` if pattern matching is not successful.
 */
export function tryMatch (p: R.Value, v: R.Value, range?: R.Range): [number, R.Value][] | undefined {
  if (R.isPVar(p)) {
    const pvar = p as R.PVar
    // N.B., wildcards are denoted by pattern variables with negative indices.
    if (pvar.idx < 0) {
      return []
    } else {
      return [[pvar.idx, v]]
    }
  } else if (p === null && v === null) {
    return []
  } else if (typeof p === 'boolean' && typeof v === 'boolean' && p === v) {
    return []
  } else if (typeof p === 'number' && typeof v === 'number' && p === v) {
    return []
  } else if (typeof p === 'string' && typeof v === 'string' && p === v) {
    return []
  } else if (R.isChar(p) && R.isChar(v)) {
    return (p as R.Char).value === (v as R.Char).value ? [] : undefined
  } else if (p === null && v === null) {
    return []
  } else if (R.isPair(p) && (R.isPair(v) || R.isStruct(v))) {
    const elems = R.listToVector(p as R.List)
    // N.B., performed a null check above, so p has at least one element
    const head = elems[0]
    const args = elems.slice(1)
    if (!R.isSym(head)) {
      throw new R.ScamperError('Runtime', 'A symbol is expected at the head of a constructor pattern', undefined, range)
    }
    const ctor = (head as R.Sym).value
    if ((ctor === 'pair' || ctor === 'cons') && args.length === 2 && R.isPair(v)) {
      const env1 = tryMatch(args[0], (v as R.Pair).fst)
      const env2 = tryMatch(args[1], (v as R.Pair).snd)
      return env1 && env2 ? env1.concat(env2) : undefined
  } else if (R.isStructKind(v, ctor)) {
      const st = v as R.Struct
      const fields = R.getFieldsOfStruct(st)
      if (fields.length === args.length) {
        let env: [number, R.Value][] = []
        for (let i = 0; i < fields.length; i++) {
          const env2 = tryMatch(args[i], st[fields[i]])
          if (!env2) {
            return undefined
          }
          env = env.concat(env2)
        }
        return env
      } else {
        return undefined
      }
    }
  } else {
    return undefined
  }
}

/**
 * The Little Pattern Machine (LPM), a stack-based virtual machine for executing
 * small functional programs.
 */
export class Machine {
  program: R.Program                    // global: all compiled code
  globals: Globals                    // thread-local: diff. threads can have different views of globals
  builtinLibs: Map<string, R.Library>   // global: all available libraries
  currentFrame?: Frame                // thread-local
  dump: Frame[]                       // thread-local
  output: Output                      // global
  options: Options                    // global

  constructor (program: R.Program, globals: Globals, builtinLibs: Map<string, R.Library>, entry: R.Id, output: Output, options: Options) {
    this.program = program
    this.globals = globals
    this.builtinLibs = builtinLibs
    const code = program.code.get(entry)!
    this.currentFrame = {
      code,
      pc: 0,
      values: [],
      env: new Array(code.numLocals)
    }
    this.options = options
    this.output = output
    this.dump = []
  }

  /** Advance the program counter of the current frame forward by one instruction. */
  advancePc () {
    if (this.currentFrame === undefined) {
      throw new R.ICE('advancePc', 'ICE: No current frame to advance program counter')
    }
    this.currentFrame.pc += 2;
  }

  /**
   * Dumps the current frame onto the call stack and switches to a new
   * stack frame indicated by the given values.
   */
  dumpAndSwitch (values: R.Value[], env: R.Env, ops: R.Code, range?: R.Range) {
    if (this.currentFrame === undefined) {
      throw new R.ICE('advancePc', 'ICE: No current frame to advance program counter')
    }
    this.dump.push(this.currentFrame)
    if (this.dump.length > this.options.maxCallStackDepth) {
      throw new R.ScamperError('Runtime', "Maximum call stack size exceeded", undefined, range)
    } else {
      this.currentFrame = { values, pc: 0, code: ops, env }
    }
  }

  /** @returns if this machine has finished execution */
  isFinished () {
    return this.currentFrame === undefined
  }

  /** Executes the machine from start to finish. */
  execute () { 
    while (!this.isFinished()) {
      this.step()
    }
  }

  /** Executes a single step of the machine. */
  step () {
    const frame = this.currentFrame
    if (frame === undefined) {
      throw new R.ICE('step', 'ICE: No current frame to execute')
    }
    const code = frame.code
    const pc = frame.pc
    const instr = code.ops[pc]
    const arg = code.ops[pc + 1]
    const values = frame.values
    switch (instr) {
      case Ops.noop: {
        this.advancePc()
        return
      }

      case Ops.ifnb: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected an argument for ifnb')
        } else if (arg < 0 || arg >= code.ops.length) {
          throw new R.ICE('step', `Invalid jump target ${arg} for ifnb operation`)
        } else if (values.length == 0) {
          throw new R.ScamperError('Runtime', 'ifnb operation requires at least one value on the stack')
        } else {
          const isJump = values.pop()!
          if (isJump) {
            this.advancePc()
          } else {
            frame.pc = arg
          }
          return
        }
      }

      case Ops.ifnm: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected an argument for ifnm')
        } else if (arg < 0 || arg >= code.ops.length) {
          throw new R.ICE('step', `Invalid jump target ${arg} for ifnm operation`)
        } else if (values.length === 0) {
          throw new R.ScamperError('Runtime', 'ifnm operation requires at least one value on the stack')
        } else if (this.program.objects[arg] === undefined) {
          throw new R.ScamperError('Runtime', `Object with index ${arg} not found in program`)
        } else if (!R.isPair(this.program.objects[arg])) {
          throw new R.ScamperError('Runtime', `Object at index ${arg} is not a pair`)
        } else {
          const obj: R.Pair = this.program.objects[arg] as R.Pair
          const pattern = obj.fst
          const failIdx = obj.snd as number
          const scrutinee = values.pop()!
          const bindings = tryMatch(pattern, scrutinee, R.Range.none)
          if (bindings === undefined) {
            frame.pc = failIdx
          } else {
            bindings.forEach(([idx, v]) => {
              frame.env[idx] = v
            })
            this.advancePc()
          }
          return
        }
      }

      case Ops.ap: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected an argument for ap')
        } else if (arg < 0 || arg >= this.options.maxArgs) {
          throw new R.ICE('step', `Invalid argument count ${arg} for ap (0 <= count <= ${this.options.maxArgs})`)
        } else if (values.length < arg + 1) {
          throw new R.ICE('step', `ap expects ${arg + 1} values but found ${values.length} on the stack.`)
        } else {
          // N.B., the stack should look like this for a valid application:
          // [ ... , f, v1, v2, ..., vk ]
          const fargs = arg === 0 ? [] : values.splice(-arg)
          const head = values.pop()
          if (R.isClosure(head)) {
            const closure = head as R.Closure
            if (closure.params.length !== fargs.length) {
              throw new R.ScamperError('Runtime', `Function expected ${closure.params.length} arguments, passed ${fargs.length} instead.`, undefined, R.Range.none)
            }
            if (!this.program.code.has(closure.code)) {
              throw new R.ScamperError('Runtime', `Function label ${closure.code} not found in program`)
            }
            const fcode = this.program.code.get(closure.code)!
            // N.B., we capture by value, so we make a copy of the closure's
            // environment and push the function arguments onto it. As an
            // invariant of our local variable indexing scheme, function
            // parameters appear in-order after the enclosing environment.
            const env = new Array<R.Value>(fcode.numLocals)
            let i = 0
            closure.env.forEach((v, _) => { env[i++] = v })
            fargs.forEach((v, _) => { env[i++] = v })
            this.advancePc()
            // N.B., if the next instruction is a ret, then tail call optimize:
            // overwrite this stack frame with the new function call directly.
            // TOOD: refactor the tail-call optimization into dumpAndSwitch?
            if (code.ops[frame.pc] === Ops.ret) {
                frame.values = []
                frame.env = env
                frame.code = fcode
                frame.pc = 0
            } else {
                this.dumpAndSwitch([], env, fcode /* TODO: range? */)
            }
            return
          } else if (R.isJsFunction(head)) {
            values.push((head as Function)(...fargs))
            this.advancePc()
            return
          } else {
            throw new R.ScamperError('Runtime', `Function application expected a function, received a ${R.typeOf(head)}`)
          }
        }
      }

      case Ops.ret: {
        if (values.length !== 1) {
          throw new R.ICE('step', 'Value stack must have exactly one value when returning')
        } else {
          const ret = values.pop()!
          // N.B, If the dump is empty, we clear the current frame to signal
          // the end of program execution
          if (this.dump.length === 0) {
            this.currentFrame = undefined
          } else {
            // N.B., we expext that the PC of the new frame has already been
            // advanced during the initial dump/function call.
            this.currentFrame = this.dump.pop()!
            this.currentFrame.values.push(ret)
          }
          return
        }
      }

      case Ops.disp: {
        if (values.length === 0) {
          throw new R.ScamperError('Runtime', 'disp operation requires at least one value on the stack')
        }
        this.output.send(values.pop()!)
        this.advancePc()
        return
      }

      case Ops.int: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected argument for int operation')
        } else {
          values.push(arg)
          this.advancePc()
        }
        return
      }

      case Ops.bool: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected argument for bool operation')
        } else if (arg === 0) {
          values.push(false)
        } else if (arg === 1) {
          values.push(true)
        } else {
          throw new R.ScamperError('Runtime', 'bool operation requires a 0 (false) or 1 (true) argument')
        }
        this.advancePc()
        return
      }

      case Ops.str: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected argument for str operation')
        } else {
          const str = this.program.identifiers[arg]
          if (str === undefined) {
            throw new R.ICE('step', `Identifier at index ${arg} not found`)
          }
          values.push(str)
          this.advancePc()
        }
        return
      }

      case Ops.obj: {
        if (arg === undefined) {
          throw new R.ICE('step', 'Expected argument for obj operation')
        } else {
          const obj = this.program.objects[arg]
          if (obj === undefined) {
            throw new R.ICE('step', `Object at index ${arg} not found`)
          }
          values.push(obj)
          this.advancePc()
        } 
        return
      }

      case Ops.lload: {
        if (arg === undefined) {
          throw new R.ScamperError('Runtime', 'Expected argument for lload operation')
        } else if (arg < 0 && arg >= frame.env.length) {
          throw new R.ICE('step', `Invalid local variable index ${arg} for lload operation`)
        } else {
          values.push(frame.env[arg])
          this.advancePc()
        }
        return
      }

      case Ops.lstore: {
        if (arg === undefined) {
          throw new R.ScamperError('Runtime', 'Expected argument for lstore operation')
        } else if (arg < 0 && arg >= frame.env.length) {
          throw new R.ICE('step', `Invalid local variable index ${arg} for lstore operation`)
        } else if (values.length === 0) {
          throw new R.ScamperError('Runtime', 'lstore operation requires at least one value on the stack')
        } else {
          frame.env[arg] = values.pop()!
          this.advancePc()
          return
        }
      }

      case Ops.gload: {
        if (arg === undefined) {
          throw new R.ScamperError('Runtime', 'Expected argument for gload operation')
        } else if (arg < 0 || arg >= this.program.identifiers.length) {
          throw new R.ICE('step', `Invalid global variable index ${arg} for gload operation`)
        } else {
          const id = this.program.identifiers[arg]
          if (!this.globals.has(id)) {
            throw new R.ScamperError('Runtime', `Global variable '${id}' not found`)
          }
          values.push(this.globals.get(id)!)
          this.advancePc()
          return
        }
      }

      case Ops.gstore: {
        if (arg === undefined) {
          throw new R.ScamperError('Runtime', 'Expected argument for gstore operation')
        } else if (arg < 0 || arg >= this.program.identifiers.length) {
          throw new R.ICE('step', `Invalid global variable index ${arg} for gstore operation`)
        } else if (values.length === 0) {
          throw new R.ScamperError('Runtime', 'gstore operation requires at least one value on the stack')
        } else {
          this.globals.set(this.program.identifiers[arg], values.pop()!)
          this.advancePc()
          return
        }
      }

      case Ops.add: {
        if (values.length < 2) {
          throw new R.ICE('step', 'add operation requires the value stack has at least two values.')
        }
        // N.B., last argument is pushed last on the stack, so pop it first
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(+)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(+)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 + v2)
          this.advancePc()
          return
        }
      }

      case Ops.sub: {
        if (values.length < 2) {
          throw new R.ICE('step', 'subtraction operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(-)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(-)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 - v2)
          this.advancePc()
          return
        }
      }

      case Ops.mul: {
        if (values.length < 2) {
          throw new R.ICE('step', 'multiplication operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(*)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(*)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 * v2)
          this.advancePc()
          return
        }
      }

      case Ops.div: {
        if (values.length < 2) {
          throw new R.ICE('step', 'division operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(/)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(/)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 / v2)
          this.advancePc()
          return
        }
      }

      case Ops.lt: {
        if (values.length < 2) {
          throw new R.ICE('step', 'comparison operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(<)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(<)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 < v2)
          this.advancePc()
          return
        }
      }

      case Ops.lte: {
        if (values.length < 2) {
          throw new R.ICE('step', 'comparison operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(<=)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(<=)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 <= v2)
          this.advancePc()
          return
        }
      }

      case Ops.gt: {
        if (values.length < 2) {
          throw new R.ICE('step', 'comparison operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(>)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(>)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 > v2)
          this.advancePc()
          return
        }
      }

      case Ops.gte: {
        if (values.length < 2) {
          throw new R.ICE('step', 'comparison operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        if (typeof v1 !== 'number') {
          throw new R.ScamperError('Runtime', '(>=)', `Expected a number, received a ${R.typeOf(v1)}`)
        } else if (typeof v2 !== 'number') {
          throw new R.ScamperError('Runtime', '(>=)', `Expected a number, received a ${R.typeOf(v2)}`)
        } else {
          values.push(v1 >= v2)
          this.advancePc()
          return
        }
      }

      case Ops.eq: {
        if (values.length < 2) {
          throw new R.ICE('step', 'equality operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        values.push(v1 === v2)
        this.advancePc()
        return
      }

      case Ops.neq: {
        if (values.length < 2) {
          throw new R.ICE('step', 'inequality operation requires the value stack has at least two values.')
        }
        const v2 = values.pop()!
        const v1 = values.pop()!
        values.push(v1 !== v2)
        this.advancePc()
        return
      }

      case Ops.loadlib: {
        if (arg === undefined) {
          throw new R.ScamperError('Runtime', 'Expected argument for loadlib operation')
        } else if (arg < 0 || arg >= this.program.identifiers.length) {
          throw new R.ICE('step', `Invalid library index ${arg} for loadlib operation`)
        } else {
          const libName = this.program.identifiers[arg]
          if (this.builtinLibs.has(libName)) {
            const lib = this.builtinLibs.get(libName)!
            if (lib.initializer !== undefined) {
              lib.initializer()
            }
            for (const [name, value] of lib.lib) {
              this.globals.set(name, value)
            }
          } else {
            throw new R.ScamperError('Runtime', `Library '${libName}' not found`, undefined, R.Range.none)
          }
        }
      }
    }
  }
}
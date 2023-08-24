import { Op, Value, Env } from './value.js'
import { ICE, Id, ScamperError } from './lang.js'
import * as S from './lang.js'
import * as V from './value.js'
import { render } from './lib/image.js'

class Control {
  private idx: number
  private ops: Op[]

  constructor (ops: Op[]) {
    this.idx = 0
    this.ops = [...ops]   // N.B., make a copy because we mutate ops!
  }

  isEmpty (): boolean { return this.idx >= this.ops.length }
  next (): Op { return this.ops[this.idx++] }
  insertOps (ops: Op[]): void { this.ops.splice(this.idx, 0, ...ops) }

  toString (): string {
    return `[idx=${this.idx}, ops=${this.ops.map(V.opToString).join(',')}]`
  }
}

class ExecutionState {
  stack: Value[]
  env: Env
  control: Control
  dump: [Value[], Env, Control][]

  constructor (env: Env, ops: Op[]) {
    this.stack = []
    this.env = env
    this.control = new Control(ops)
    this.dump = []
  }

  isRunning(): boolean { return !this.control.isEmpty() }
}

export function expToOps (e: S.Exp): Op[]{
  switch (e.tag) {
    case 'var':
      return [V.mkVar(e.name, e.range)]
    case 'num':
      return [V.mkValue(e.value)]
    case 'bool':
      return [V.mkValue(e.value)]
    case 'str':
      return [V.mkValue(e.value)]
    case 'lam':
      return [V.mkCls(e.args, expToOps(e.body))]
    case 'let':
      const valOps = e.bindings.map((b) => b.body).flatMap(expToOps)
      return valOps.concat([V.mkLet(e.bindings.map((b) => b.name))]).concat(expToOps(e.body))
    case 'app':
      return [e.head].concat(e.args).flatMap(expToOps).concat([V.mkAp(e.args.length, e.range)])
    case 'if':
      return expToOps(e.guard).concat([V.mkIf(expToOps(e.ifb), expToOps(e.elseb), e.range)])
  }
}

export function step (state: ExecutionState): void {
  const op = state.control.next()
  const stack = state.stack
  switch (op.tag) {
    case 'val':
      stack.push(op.value)
      break
    case 'cls':
      stack.push(V.mkClosure(op.params.length, op.params, op.ops, state.env))
      break
    case 'var':
      if (state.env.has(op.name)) {
        stack.push(state.env.get(op.name)!)
      } else {
        throw new ScamperError('Runtime', `Referenced unbound identifier "${op.name}".`, undefined, op.range)
      }
      break
    case 'ap':
      if (stack.length >= op.arity + 1) {
        const head = stack[stack.length - op.arity - 1]
        const args = stack.slice(-op.arity)
        for (let i = 0; i < op.arity + 1; i++) { stack.pop() }
        if (V.isClosure(head)) {
          const closure = head as V.Closure
          if (closure.params.length !== args.length) {
            throw new ScamperError('Runtime', `Function expected ${closure.params.length} arguments, passed ${args.length} instead.`, undefined, op.range)
          } else {
            state.dump.push([state.stack, state.env, state.control])
            state.stack = []
            state.env = closure.env.extend(closure.params.map((p, i) => [p, args[i]]))
            state.control = new Control(closure.ops)
          }
        } else if (V.isJsFunction(head)) {
          const jsFunc = head as V.JsFunction
          if (!jsFunc.isVariadic && (jsFunc.arity !== args.length)) {
            throw new ScamperError('Runtime', `Function expected ${jsFunc.arity} arguments, passed ${args.length} instead.`, undefined, op.range)
          } else if (jsFunc.isVariadic && (args.length < jsFunc.arity)) {
            throw new ScamperError('Runtime', `Function expected at least arguments ${jsFunc.arity}, passed ${args.length}.`, undefined, op.range) 
          } else {
            try {
              const result = (head as V.JsFunction).fn(...args) as Value
              stack.push(result)
            } catch (e) {
              renderException(state.env, e)
            }
          }
        }
      } else {
        throw new ICE('sem.step', `Not enough arguments on stack. Need ${op.arity + 1}, have ${stack.length}`)
      }
      break
    case 'if':
      if (stack.length >= 1) {
        const guard = stack.pop()!
        if (guard === true) {
          state.control.insertOps(op.ifb)
        } else if (guard === false) {
          state.control.insertOps(op.elseb)
        } else {
          throw new ScamperError('Runtime', `Boolean expected in conditional, received ${V.valueToString(guard)} instead`, undefined, op.range)
        }
      } else {
        throw new ICE('sem.step', `Guard missing from stack for conditional`)
      }
      break
    case 'let':
      if (stack.length >= op.names.length) {
        const values = stack.slice(-op.names.length)
        for (let i = 0; i < op.names.length; i++) { stack.pop() }
        state.env = state.env.extend(op.names.map((n, i) => [n, values[i]]))
        // TODO: need to pop the env at some point, right?
      }
      break
  }
  if (state.control.isEmpty() && state.dump.length > 0) {
    const ret = state.stack.pop()!
    const [stack, env, control] = state.dump.pop()!
    state.stack = stack
    state.env = env
    state.control = control
    state.stack.push(ret)
  }
}

function renderException (env: Env, err: any) {
  execute(new ExecutionState(env, [V.mkVar('render', S.noRange), V.mkValue(err as any), V.mkAp(1, S.noRange)]))
}

function execute (state: ExecutionState): Value {
  while (state.isRunning()) {
    step(state)
  }
  if (state.stack.length !== 1) {
    throw new ICE('sem.execute', `Stack size is not 1 after execution: ${state.stack}`)
  }
  return state.stack.pop()!
}

export function runProgram (builtinLibs: Map<Id, [Id, Value][]>, env: Env, prog: S.Prog) {
  prog.forEach((stmt) => {
    switch (stmt.tag) {
      case 'binding': {
        try {
          const state = new ExecutionState(env, expToOps(stmt.body))
          const result = execute(state)
          env.set(stmt.name, result)
        } catch (e) {
          renderException(env, e)
        }
        break
      }
      case 'import': {
        if (builtinLibs.has(stmt.modName)) {
          env = env.extend(builtinLibs.get(stmt.modName)!)
        } else {
          throw new ScamperError('Runtime', `Module ${stmt.modName} not found`, undefined, stmt.range)
        }
        break
      }
      case 'stmtexp': {
        try {
          const state = new ExecutionState(env, expToOps(stmt.body))
          execute(state)
        } catch (e) {
          renderException(env, e)
        }
      }
      break
    }
  })
}

export function runClosure (closure: V.Closure, ...args: Value[]): Value {
  const state = new ExecutionState(closure.env, closure.ops)
  state.stack = args
  return execute(state)
}

export function callFunction (fn: V.Closure | V.JsFunction | Function, ...args: any): any {
  if (V.isClosure(fn)) {
    return runClosure(fn as V.Closure, ...args)
  } else if (V.isJsFunction(fn)) {
    return (fn as V.JsFunction).fn(...args)
  } else {
    return (fn as Function)(...args)
  }
}
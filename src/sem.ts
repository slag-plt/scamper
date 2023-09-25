import { ICE, Id, noRange, ScamperError } from './lang.js'
import { Pat, Exp, Stmt, Prog, Op, Value, Env } from './lang.js'
import { expToHTML, mkCodeElement, renderToOutput } from './display.js'
import * as C from './contract.js'

class Control {
  idx: number
  ops: Op.T[]

  constructor (ops: Op.T[]) {
    this.idx = 0
    this.ops = ops
  }

  isEmpty (): boolean { return this.idx >= this.ops.length }
  next (): Op.T { return this.ops[this.idx++] }

  toString (): string {
    return `[idx=${this.idx}, ops=${this.ops.map(Op.toString).join(',')}]`
  }

  jumpTo(label: Op.Label): void {
    let cur = this.ops[this.idx]
    while (!this.isEmpty() && (cur.tag !== 'lbl' || cur.name !== label)) {
      cur = this.ops[++this.idx]
    }
    if (this.isEmpty()) {
      throw new ICE('Control.jumpTo', `Label ${label} not found`)
    }
  }
}

class ExecutionState {
  stack: Value.T[]
  env: Env
  control: Control
  dump: [Value.T[], Env, Control][]

  constructor (env: Env, ops: Op.T[]) {
    // N.B., if the state consists of a single value, then immediately
    // turn the value Op to a genuine value to avoid an unnecessary
    // step of computation
    if (ops.length === 1 && ops[0].tag === 'val') {
      this.stack = [ops[0].value]
      this.env = env
      this.control = new Control([])
      this.dump = []
    } else {
      this.stack = []
      this.env = env
      this.control = new Control(ops)
      this.dump = []
    }
  }

  isFinished(): boolean { return this.control.isEmpty() && this.dump.length === 0 }

  dumpAndSwitch (stack: Value.T[], env: Env, ops: Op.T[]): void {
    this.dump.push([this.stack, this.env, this.control])
    this.stack = stack
    this.env = env
    this.control = new Control(ops)
  }

  isDumpEmpty() { return this.dump.length === 0 }

  popDump () {
    const [stack, env, control] = this.dump.pop()!
    this.stack = stack
    this.env = env
    this.control = control
  }

  jumpPast (label: Op.Label): void {
    this.control.jumpTo(label)
    this.control.idx += 1
  }
}

function valueToExp (env: Env, v: Value.T): Exp.T {
  if (Value.isNumber(v)) {
    return Exp.mkVal(v, noRange)
  } else if (Value.isBoolean(v)) {
    return Exp.mkVal(v, noRange)
  } else if (Value.isString(v)) {
    return Exp.mkVal(v, noRange)
  } else if (Value.isNull(v)) {
    return Exp.mkVar('null', noRange)
  } else if (Value.isVoid(v)) {
    return Exp.mkVar('void', noRange)
  } else if (Value.isArray(v)) {
    return Exp.mkApp (Exp.mkVar('vector', noRange), (v as Value.T[]).map((v) => (valueToExp(env, v))), '(', noRange)
  } else if (Value.isClosure(v)) {
    const cls = v as Value.Closure
    if (cls.name === undefined) {
      return Exp.mkLam(cls.params, dumpToExp([[], cls.env, new Control(cls.ops)]), '(', noRange)
    } else {
      return Exp.mkVar(cls.name, noRange)
    }
  } else if (Value.isJsFunction(v)) {
    return Exp.mkVar((v as Function).name, noRange)
  } else if (Value.isChar(v)) {
    return Exp.mkVal(v, noRange)
  } else if (Value.isList(v)) {
    const l = v as Value.List
    if (l === null) {
      return Exp.mkVar('null', noRange)
    } else {
      const elems = Value.listToArray(l)
      return Exp.mkApp(Exp.mkVar('list', noRange), elems.map((v) => valueToExp(env, v)), '(', noRange)
    }
  } else if (Value.isPair(v)) {
    const p = v as Value.Pair
    return Exp.mkApp(Exp.mkVar('pair', noRange), [p.fst, p.snd].map((v) => valueToExp(env, v)), '(', noRange)
  } else if (Value.isStruct(v)) {
    const s = v as Value.Struct
    const fields = Value.getFieldsOfStruct(s)
    return Exp.mkApp(Exp.mkVar(s[Value.structKind], noRange), fields.map((f) => valueToExp(env, s[f])), '(', noRange)
  } else {
    // NOTE: we're slowly mushing together values and expressions... perhaps
    // we should collapse the hierarchy once and for all to avoid this mess?
    return Exp.mkVal(v, noRange)
  }
}

function findCondBranches (start: number, label: string, ops: Op.T[]): { branches: { guard: Op.T[], body: Op.T[] }[], endIdx: number } {
  let i = start
  const branches: { guard: Op.T[], body: Op.T[] }[] = []
  let guard: Op.T[] = []
  let op = ops[i]
  while (op.tag !== 'lbl' || op.name !== label) {
    if (op.tag === 'cond' && op.end === label) {
      branches.push({ guard, body: op. body})
      guard = []
    } else {
      guard.push(op)
    }
    op = ops[++i]
  }
  return { branches, endIdx: i }
}

function findArgs (start: number, label: string, ops: Op.T[]): { segments: Op.T[][], endIdx: number } {
  let i = start
  let segments: Op.T[][] = []
  let seg: Op.T[] = []
  let op = ops[i]
  while (op.tag !== 'lbl' || op.name !== label) {
    if ((op.tag === 'and' || op.tag === 'or') && op.jmpTo === label) {
      segments.push(seg)
      seg = []
    } else {
      seg.push(op)
    }
    op = ops[++i]
  }
  return { segments, endIdx: i }
}

function dumpToExp ([stack, env, control]: [Value.T[], Env, Control], hole?: Exp.T): Exp.T {
  let expStack = stack.map((v) => valueToExp(env, v))
  if (hole !== undefined) { expStack.push(hole) }
  for (let i = control.idx; i < control.ops.length; i++) {
    const op = control.ops[i]
    if (op.tag === 'var') {
      if (env.has(op.name)) {
        const val = env.get(op.name)!
        if (Value.isClosure(val) || Value.isJsFunction(val)) {
          expStack.push(Exp.mkVar(op.name, op.range))
        } else {
          expStack.push(valueToExp(env, env.get(op.name)!))
        }
      } else {
        expStack.push(Exp.mkVar(op.name, op.range))
      }
    } else if (op.tag === 'val') {
      expStack.push(valueToExp(env, op.value))
    } else if (op.tag === 'cls') {
      expStack.push(Exp.mkLam(op.params, dumpToExp([[], env, new Control(op.ops)]), '(', noRange))
    } else if (op.tag === 'ap') {
      const args = op.arity === 0 ? [] : expStack.slice(-op.arity)
      for (let i = 0; i < op.arity; i++) { expStack.pop() }
      expStack.push(Exp.mkApp(expStack.pop()!, args, '(', noRange))
    } else if (op.tag === 'if') {
      const elseb = dumpToExp([[], env, new Control(op.elseb)])
      const ifb = dumpToExp([[], env, new Control(op.ifb)])
      const guard = expStack.pop()!
      expStack.push(Exp.mkIf(guard, ifb, elseb, '(', noRange))
    } else if (op.tag === 'let') {
      const names = op.names
      const bindings = names.reverse().map((n: string) =>
        ({ name: n, body: expStack.pop()! })).reverse()
      // N.B., names bound by the let shadow outer bindings, so remove them
      // from the environment so that we don't replace them by accident!
      const body = dumpToExp([[], env.quotient(...names), new Control(op.body)])
      expStack.push(Exp.mkLet(bindings, body, '(', noRange))
    } else if (op.tag === 'seq') {
      if (op.numSubexps === 0) {
        expStack.push(Exp.mkBegin([], '(', noRange))
      } else {
        const exps = expStack.slice(-op.numSubexps)
        for (let i = 0; i < op.numSubexps; i++) { expStack.pop() }
        expStack.push(Exp.mkBegin(exps, '(', noRange))
      }
    } else if (op.tag === 'match') {
      throw new ICE('sem.dumpToExp', 'Unimplemented match case')
    } else if (op.tag === 'and') {
      const { segments, endIdx } = findArgs(i + 1, op.jmpTo, control.ops)
      const args = [expStack.pop()!].concat(segments.map((ops) => dumpToExp([[], env, new Control(ops)])))
      expStack.push(Exp.mkAnd(args, '(', noRange))
      i = endIdx
    } else if (op.tag === 'or') {
      const { segments, endIdx } = findArgs(i + 1, op.jmpTo, control.ops)
      const args = [expStack.pop()!].concat(segments.map((ops) => dumpToExp([[], env, new Control(ops)])))
      expStack.push(Exp.mkOr(args, '(', noRange))
      i = endIdx
    } else if (op.tag === 'cond') {
      const first = { guard: expStack.pop()!, body: dumpToExp([[], env, new Control(op.body)]) }
      const { branches, endIdx } = findCondBranches(i + 1, op.end, control.ops)
      expStack.push(Exp.mkCond([first].concat(branches.map((b) => ({
        guard: dumpToExp([[], env, new Control(b.guard)]),
        body: dumpToExp([[], env, new Control(b.body)])
      }))), noRange))
      i = endIdx
    } else if (op.tag === 'exn') {
      expStack.push(Exp.mkApp(Exp.mkVar('error', noRange), [Exp.mkVal(op.msg, noRange)], '(', noRange))
    } else if (op.tag === 'lbl') {
      // N.B., do nothing, skip over labels!
    }
  }
  if (expStack.length !== 1) {
    throw new ICE('sem.dumpToExp', `Stack size is not 1 after execution: ${stack}`)
  } else {
    return expStack.pop()!
  }
}

export function opsToExp (ops: Op.T[]): Exp.T {
  return dumpToExp([[], new Env([]), new Control(ops)])
}

export function stateToExp (state: ExecutionState): Exp.T | undefined {
  const dump: [Value.T[], Env, Control][] = [...state.dump]
  dump.push([state.stack, state.env, state.control])
  let ret: Exp.T | undefined = undefined
  for (let i = dump.length - 1; i >= 0; i--) {
    ret = dumpToExp(dump[i], ret)
  }
  return ret
}

export function expToOps (e: Exp.T): Op.T[] {
  switch (e.kind) {
    case 'var':
      return [Op.mkVar(e.name, e.range)]
    case 'val':
      return [Op.mkValue(e.value)]
    case 'lam':
      return [Op.mkCls(e.args, expToOps(e.body))]
    case 'let':
      const valOps = e.bindings.map((b) => b.body).flatMap(expToOps)
      return valOps.concat([Op.mkLet(e.bindings.map((b) => b.name), expToOps(e.body))])
    case 'app':
      return [e.head].concat(e.args).flatMap(expToOps).concat([Op.mkAp(e.args.length, e.range)])
    case 'if':
      return expToOps(e.guard).concat([Op.mkIf(expToOps(e.ifb), expToOps(e.elseb), e.range)])
    case 'begin':
      return e.exps.flatMap(expToOps).concat([Op.mkSeq(e.exps.length)])
    case 'match':
      return expToOps(e.scrutinee).concat([Op.mkMatch(e.branches.map((b) => ({ pattern: b.pattern, body: expToOps(b.body) })), e.range)])
    case 'and': {
      const label = Op.freshLabel()
      return e.exps.flatMap((e) => expToOps(e).concat([Op.mkAnd(label, e.range)])).concat([Op.mkValue(true), Op.mkLbl(label)])
    }
    case 'or': {
      const label = Op.freshLabel()
      return e.exps.flatMap((e) => expToOps(e).concat([Op.mkOr(label, e.range)])).concat([Op.mkValue(false), Op.mkLbl(label)])
    }
    case 'cond': {
      // TODO: need an error instruction to throw before the label!
      const label = Op.freshLabel()
      return e.branches.flatMap((b) => expToOps(b.guard).concat([Op.mkCond(expToOps(b.body), label, e.range)])).concat([
        Op.mkExn('No branches of "cond" expression matched', undefined, e.range),
        Op.mkLbl(label)
      ])
    }
  }
}

export function tryMatch (p: Pat.T, v: Value.T): [string, Value.T][] | undefined {
  if (p.kind === 'wild') {
    return []
  } else if (p.kind === 'var') {
    return [[p.name, v]]
  } else if (p.kind === 'null' && v === null) {
    return []
  } else if (p.kind === 'bool' && typeof v === 'boolean' && p.value === v) {
    return []
  } else if (p.kind === 'num' && typeof v === 'number' && p.value === v) {
    return []
  } else if (p.kind === 'str' && typeof v === 'string' && p.value === v) {
    return []
  } else if (p.kind === 'char' && Value.isChar(v)) {
    return p.value === (v as Value.Char).value ? [] : undefined
  } else if (p.kind === 'ctor' && (Value.isPair(v) || Value.isStruct(v))) {
    const head = p.ctor
    const args = p.args
    if ((head === 'pair' || head === 'cons') && args.length === 2 && Value.isPair(v)) {
      const env1 = tryMatch(args[0], (v as Value.Pair).fst)
      const env2 = tryMatch(args[1], (v as Value.Pair).snd)
      return env1 && env2 ? env1.concat(env2) : undefined
  } else if (Value.isStructKind(v, head)) {
      const st = v as Value.Struct
      const fields = Value.getFieldsOfStruct(st)
      if (fields.length === args.length) {
        let env: [string, Value.T][] = []
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

function stepPrim (state: ExecutionState): boolean {
  const op = state.control.next()
  const stack = state.stack
  switch (op.tag) {

    case 'val': {
      stack.push(op.value)
      return true
    }

    case 'cls': {
      stack.push(Value.mkClosure(op.params.length, op.params, op.ops, state.env))
      return true
    }
  
    case 'var': {
      if (state.env.has(op.name)) {
        stack.push(state.env.get(op.name)!)
      } else {
        throw new ScamperError('Runtime', `Referenced unbound identifier "${op.name}".`, undefined, op.range)
      }
      return true
    }

    case 'ap': {
      if (stack.length < op.arity + 1) {
        throw new ICE('sem.step', `Not enough arguments on stack. Need ${op.arity + 1}, have ${stack.length}`)
      }
      const head = stack[stack.length - op.arity - 1]
      const args = op.arity === 0 ? [] : stack.slice(-op.arity)
      for (let i = 0; i < op.arity + 1; i++) { stack.pop() }
      if (Value.isClosure(head)) {
        const closure = head as Value.Closure
        if (closure.params.length !== args.length) {
          throw new ScamperError('Runtime', `Function expected ${closure.params.length} arguments, passed ${args.length} instead.`, undefined, op.range)
        } else {
          // TODO: here, we can check if this control is done.
          // If so, then this is a tail call! No need to dump,
          // just overwrite the current state and go forward.
          state.dumpAndSwitch([], closure.env.extend(closure.params.map((p, i) => [p, args[i]])), closure.ops)
        }
        return false
      } else if (Value.isJsFunction(head)) {
        const fn = head as Function
        try {
          const result = fn(...args) as Value.T
          stack.push(result)
        } catch (e) {
          // N.B., annotate any errors from foreign function calls with
          // range information from this application
          if (e instanceof ScamperError) {
            e.source = fn.name
            e.range = op.range
          }
          throw e
        }
        // N.B., continue stepping if we step through one of the primitive
        // constructor-functions
        return fn.name === 'pair' || fn.name === 'list' || fn.name === 'vector'
      } else {
        throw new ScamperError('Runtime', `Non-function value (${Value.typeOf(head)}) in function application`, undefined, op.range)
      }
    }

    case 'if': {
      if (stack.length >= 1) {
        const guard = stack.pop()!
        if (guard === true) {
          state.dumpAndSwitch([], state.env, op.ifb)
        } else if (guard === false) {
          state.dumpAndSwitch([], state.env, op.elseb)
        } else {
          throw new ScamperError('Runtime', `Boolean expected in conditional, received ${Value.toString(guard)} instead`, undefined, op.range)
        }
      } else {
        throw new ICE('sem.step', `Guard missing from stack for conditional`)
      }
      return false
    }

    case 'let': {
      if (stack.length >= op.names.length) {
        const values = stack.slice(-op.names.length)
        for (let i = 0; i < op.names.length; i++) { stack.pop() }
        const newEnv = state.env.extend(op.names.map((n, i) => [n, values[i]]))
        state.dumpAndSwitch([], newEnv, op.body)
      } else {
        throw new ICE('sem.step', `Not enough values on stack for let binding`)
      }
      return false
    }
  
    case 'seq': {
      stack.slice(-op.numSubexps)
      if (stack.length >= op.numSubexps) {
        // N.B., the top of the stack is the last value created which we want to return!
        const ret = stack.pop()
        for (let i = 1; i < op.numSubexps; i++) { stack.pop() }
        stack.push(ret)
      } else {
        throw new ICE('sem.step', `Not enough values on stack for sequence, ${op.numSubexps} expected, ${stack.length} found`)
      }
      return false
    }
  
    case 'match': {
      if (stack.length >= 1) {
        const scrutinee = stack.pop()!
        let foundMatch = false
        for (let i = 0; !foundMatch && i < op.branches.length; i++) {
          const bindings = tryMatch(op.branches[i].pattern, scrutinee)
          if (bindings) {
            state.dumpAndSwitch([], state.env.extend(bindings), op.branches[i].body)
            foundMatch = true
          }
        }
        if (!foundMatch) {
          throw new ScamperError('Runtime', `No pattern matches for ${Value.toString(scrutinee)}`, undefined, op.range)
        }
      } else {
        throw new ICE('sem.step', `Scrutinee missing from stack for match`)
      }
      return false
    }

    case 'and': {
      if (stack.length < 1) {
        throw new ICE('sem.and', 'Missing argument to "and" instruction')
      }
      const val = stack.pop()!
      if (typeof val !== 'boolean') {
        throw new ScamperError('Runtime', `"and" expects a boolean value, received ${Value.typeOf(val)}`, undefined, op.range)
      }
      if (!val) {
        state.stack.push(false)
        state.jumpPast(op.jmpTo)
      }
      // N.B., otherwise, move onto the next instruction!
      return false
    }

    case 'or': {
      if (stack.length < 1) {
        throw new ICE('sem.or', 'Missing argument to "or" instruction')
      }
      const val = stack.pop()!
      if (typeof val !== 'boolean') {
        throw new ScamperError('Runtime', `"or" expects a boolean value, received ${Value.typeOf(val)}`, undefined, op.range)
      }
      if (val) {
        state.stack.push(true)
        state.jumpPast(op.jmpTo)
      }
      // N.B., otherwise, move onto the next instruction!
      return false
    }
    
    case 'cond': {
      if (stack.length < 1) {
        throw new ICE('sem.cond', 'Missing guard to "cond" instruction')
      }
      const guard = stack.pop()!
      if (guard) {
        // N.B., make sure to switch this frame's instr pointer before jumping
        // otherwise we'll forget where to return to!
        state.jumpPast(op.end)
        state.dumpAndSwitch([], state.env, op.body)
      }
      return false
    }

    case 'lbl': {
      // N.B., skip over a label peacefully
      return true
    }

    case 'exn': {
      throw new ScamperError('Runtime', op.msg, op.modName, op.range, op.source)
    }
  }
}

function step (state: ExecutionState): void {
  var cont = false
  do {
    cont = stepPrim(state)
    // N.B., pop the dump until we arrive at a non-finished state
    while (state.control.isEmpty() && !state.isDumpEmpty()) {
      const ret = state.stack.pop()!
      state.popDump()
      state.stack.push(ret)
    }
  } while (!state.isFinished() && cont)
}

function executeStructDecl (name: string, fields: string[], env: Env): Env {
  const predFn = function (v: any) {
    C.checkContract(arguments, C.contract(`${name}?`, [C.any]))
    return Value.isStructKind(v, name)
  }
  Value.nameFn(`${name}?`, predFn)
  const pred: [string, Value.T] = [`${name}?`, predFn]

  const ctorFn = function (...args: any[]) {
    C.checkContract(arguments, C.contract(name, fields.map((f) => C.any)))
    return Value.mkStruct(name, fields, args)
  }
  Value.nameFn(name, ctorFn)
  const ctor: [string, Value.T] = [name, ctorFn]

  const fieldFns: [string, Value.T][] = []
  fields.forEach((f) => {
    const fieldName = `${name}-${f}`
    const fn = function (v: Value.Struct) {
      C.checkContract(arguments, C.contract(fieldName, [C.struct(name)]))
      return v[f]
    }
    Value.nameFn(fieldName, fn)
    fieldFns.push([fieldName, fn])
  })

  return env.extend([pred, ctor, ...fieldFns])
}

// TODO: these functions are used by Javascript libraries to invoke higher-order
// functions that could potentially be Scamper closures. We don't expect them to
// side-effect. Hence, we pass in a "dummy" display function that does nothing.
// However, if we allow side-effecting higher-order functions in the future,
// we'll need to revisit this design decision.

function execute (state: ExecutionState): Value.T {
  while (!state.isFinished()) {
    step(state)
  }
  if (state.stack.length !== 1) {
    throw new ICE('sem.execute', `Stack size is not 1 after execution: ${state.stack}`)
  }
  return state.stack.pop()!
}

function runClosure (closure: Value.Closure, ...args: Value.T[]): Value.T {
  const state = new ExecutionState(closure.env.extend(closure.params.map((x, i) => [x, args[i]])), closure.ops)
  return execute(state)
}

export function callFunction (fn: Value.Closure | Function, ...args: any): any {
  if (Value.isClosure(fn)) {
    return runClosure(fn as Value.Closure, ...args)
  } else {
    return (fn as Function)(...args)
  }
}

function makeTraceDiv(): HTMLElement {
  const div = document.createElement('div')
  div.classList.add('scamper-trace')
  return div
}

function makeTraceHeader (s: Stmt.T): HTMLElement {
  switch (s.kind) {
    case 'binding': {
      const ret = mkCodeElement(`Evaluating binding ${s.name}...`)
      ret.append(mkCodeElement('\n'))
      ret.append(expToHTML(s.body))
      return ret
    }

    case 'display': {
      const ret = mkCodeElement('Evaluating displayed expression...')
      ret.append(mkCodeElement('\n'))
      ret.append(expToHTML(s.body))
      return ret
    }

    case 'import':
      return mkCodeElement(`Importing module ${s.modName}...`)

    case 'exp': {
      const ret = mkCodeElement('Evaluating expression...')
      ret.append(mkCodeElement('\n'))
      ret.append(expToHTML(s.body))
      return ret
    }

    case 'struct':
      return mkCodeElement(`Evaluating struct declaration ${s.id}...`)
  }
}

export class Sem {
  display: HTMLElement
  env: Env
  prog: Prog
  curStmt: number
  state?: ExecutionState
  builtinLibs: Map<Id, [Id, Value.T][]>
  traces?: HTMLElement[]
  defaultDisplay: boolean

  constructor (display: HTMLElement, builtinLibs: Map<Id, [Id, Value.T][]>, isTracing: boolean, defaultDisplay: boolean, env: Env, prog: Prog) {
    this.display = display
    this.builtinLibs = builtinLibs
    if (isTracing) {
      this.traces = new Array(prog.length)
      for (let i = 0; i < prog.length; i++) {
        this.traces[i] = makeTraceDiv()
      }
    } else {
      this.traces = undefined
    }
    this.env = env
    this.prog = prog
    // N.B., start at -1 so that we can advance immediately
    this.curStmt = -1
    this.state = undefined
    this.defaultDisplay = defaultDisplay
    this.advance()
  }

  isFinished (): boolean { return this.curStmt === this.prog.length }
  isTracing (): boolean { return this.traces !== undefined }
  appendToCurrentTrace (v: HTMLElement | string): void {
    if (typeof v === 'string') {
      v = mkCodeElement(v)
    }
    this.traces![this.curStmt]!.appendChild(v)
  }

  advance (): void {
    this.curStmt += 1
    this.state = undefined
    if (!this.isFinished() && this.isTracing()) {
      this.display.appendChild(this.traces![this.curStmt]!)
      this.appendToCurrentTrace(makeTraceHeader(this.prog[this.curStmt]))
      this.appendToCurrentTrace('\n')
    }
  }

  step (): void {
    const stmt = this.prog[this.curStmt]
    switch (stmt.kind) {

      case 'binding': {
        if (this.state === undefined) {
          this.state = new ExecutionState(this.env, expToOps(stmt.body))
        }
        if (!this.state.isFinished()) {
          try {
            step(this.state)
            if (this.isTracing()) {
              this.appendToCurrentTrace('-->')
              this.appendToCurrentTrace(' ')
              this.appendToCurrentTrace(expToHTML(stateToExp(this.state)!))
              this.appendToCurrentTrace('\n')
            }
          } catch (e) {
            renderToOutput(this.display, e)
            this.advance()
          }
        } else {
          if (this.state.stack.length !== 1) {
            throw new ICE('sem.step', `Stack size is not 1 after execution: ${this.state.stack}`)
          }
          // N.B., if we bind a lambda, annotate the closure value with the
          // name for stepping purposes
          const val = this.state.stack.pop()!
          if (Value.isClosure(val)) {
            (val as Value.Closure).name = stmt.name
          }
          if (this.env.has(stmt.name)) {
            throw new ScamperError('Runtime', `Identifier "${stmt.name}" already bound`, undefined, stmt.range)
          } else {
            this.env.set(stmt.name, val)
          }
          if (this.isTracing()) {
            this.appendToCurrentTrace(mkCodeElement(`${stmt.name} bound`))
          }
          this.advance()
        }
        break
      }

      case 'exp': {
        if (this.state === undefined) {
          this.state = new ExecutionState(this.env, expToOps(stmt.body))
        }
        if (!this.state.isFinished()) {
          try {
            step(this.state)
            if (this.isTracing()) {
              this.appendToCurrentTrace('-->')
              this.appendToCurrentTrace(' ')
              this.appendToCurrentTrace(expToHTML(stateToExp(this.state)!))
              this.appendToCurrentTrace('\n')
            }
          } catch (e) {
            renderToOutput(this.display, e)
            this.advance()
          }
        } else {
          if (this.state.stack.length !== 1) {
            throw new ICE('sem.step', `Stack size is not 1 after execution: ${this.state.stack}`)
          }
          if (this.defaultDisplay) {
            renderToOutput(this.display, this.state.stack.pop()!)
          } else {
            this.state.stack.pop()
          }
          this.advance()
        }
        break
      }
      case 'import': {
        if (this.builtinLibs.has(stmt.modName)) {
          this.env = this.env.extend(this.builtinLibs.get(stmt.modName)!)
          if (this.isTracing()) {
            this.appendToCurrentTrace(`Module ${stmt.modName} imported`)
          }
          this.advance()
        } else {
          this.advance()
          throw new ScamperError('Runtime', `Module ${stmt.modName} not found`, undefined, stmt.range)
        }
        break
      }
      case 'display': {
        if (this.state === undefined) {
          this.state = new ExecutionState(this.env, expToOps(stmt.body))
        }
        if (!this.state.isFinished()) {
          try {
            step(this.state)
            if (this.isTracing()) {
              this.appendToCurrentTrace('-->')
              this.appendToCurrentTrace(' ')
              this.appendToCurrentTrace(expToHTML(stateToExp(this.state)!))
              this.appendToCurrentTrace('\n')
            }
          } catch (e) {
            renderToOutput(this.display, e)
            this.advance()
          }
        } else {
          if (this.state.stack.length !== 1) {
            throw new ICE('sem.step', `Stack size is not 1 after execution: ${this.state.stack}`)
          }
          renderToOutput(this.display, this.state.stack.pop())
          this.advance()
        }
        break
      }
      case 'struct': {
        this.env = executeStructDecl(stmt.id, stmt.fields, this.env)
        if (this.isTracing()) {
          this.appendToCurrentTrace(`Struct ${stmt.id} declared`)
        }
        this.advance()
        break
      }
    }
  }

  stepToNextStmt (): void {
    const idx = this.curStmt
    try {
      while (!this.isFinished() && this.curStmt === idx) { this.step() }
    } catch (e) {
      renderToOutput(this.display, e)
    }
  }

  execute (): void {
    try {
      while (!this.isFinished()) { this.step() }
    } catch (e) {
      renderToOutput(this.display, e)
    }
  }
}
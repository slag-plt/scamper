type Value = any

/** Environments are scoped collections of variable bindings. */
class Env {
  bindings: Map<string, Value>
  parent?: Env

  constructor (parent?: Env) {
    this.bindings = new Map()
    this.parent = parent
  }

  get (name: string): Value | undefined {
    return this.bindings.get(name) ?? this.parent?.get(name)
  }

  set (name: string, value: Value): void {
    this.bindings.set(name, value)
  }

  has (name: string): boolean {
    return this.bindings.has(name) || (this.parent?.has(name) ?? false)
  }

  extend (...bindings: [string, Value]): Env {
    const ret = new Env(this)
    for (const [name, value] of bindings) {
      ret.set(name, value)
    }
    return ret
  }

  pop (): Env | undefined {
    return this.parent
  }
}

type Lit   = { tag: 'lit', value: Value }
type Var   = { tag: 'var', name: string }
type Ctor  = { tag: 'ctor', name: string }
type Cls   = { tag: 'cls', params: string[], body: Exp }
type Ap    = { tag: 'ap', numArgs: number }
type Match = { tag: 'match', pattern: Pat, ifB: Exp, elseB: Exp }
type Raise = { tag: 'raise', msg: string }
type Pop   = { tag: 'pop' }
type Ops   = Lit | Var | Ctor | Cls | Ap | Match | Raise | Pop
type Exp   = Ops[]

type PWild = { tag: 'wildcard' }
type PVar  = { tag: 'pvar', name: string }
type PCtor = { tag: 'pc', name: string, args: Pat[] }
type Pat   = PWild | PVar | PCtor

type Frame  = { name: string, env: Env, exp: Exp }
type Thread = { frames: Frame[] }

interface Output {
  send: (v: Value) => void
}

class Machine {
  builtinLibs: Map<string, [string, Value][]>
  maxCallStackDepth: number
  output: Output

  constructor (builtinLibs: Map<string, [string, Value][]>, env: Env, exp: Exp, output: Output, maxCallStackDepth = 10000) {
    this.builtinLibs = builtinLibs
    this.output = output
    this.maxCallStackDepth = maxCallStackDepth
  }
}
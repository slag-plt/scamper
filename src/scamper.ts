import * as Parser from './parser.js'
import * as Sem from './sem.js'
import * as Value from './value.js'

import builtinLibs from './lib/builtin.js'
import Prelude from './lib/prelude.js'

class Scamper {
  env: Value.Env
  display: HTMLElement

  constructor (display: HTMLElement, initialEnv?: Value.Env) {
    this.display = display
    if (initialEnv !== undefined) {
      this.env = initialEnv
    } else {
      this.env = new Value.Env([...Prelude,])
    }
  }

  parseProgram (src: string) {
    return Parser.parseProgram(src)
  }

  runProgram (src: string) {
    const prog = Parser.parseProgram(src)
    const state = new Sem.Sem(this.display, builtinLibs, this.env, prog)
    state.execute()
  }
}

export default Scamper
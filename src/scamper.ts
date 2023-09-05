import * as Lang from './lang.js'
import * as Parser from './parser.js'
import * as Sem from './sem.js'
import * as Value from './value.js'

import builtinLibs from './lib/builtin.js'
import Prelude from './lib/prelude.js'

class Scamper {
  env: Value.Env
  display: HTMLElement
  isTracing: boolean
  prog: Lang.Prog
  sem: Sem.Sem

  constructor (display: HTMLElement, isTracing: boolean, src: string, initialEnv?: Value.Env) {
    this.display = display
    this.isTracing = isTracing
    if (initialEnv !== undefined) {
      this.env = initialEnv
    } else {
      this.env = new Value.Env([...Prelude,])
    }
    this.prog = Parser.parseProgram(src)
    this.sem = new Sem.Sem(this.display, builtinLibs, isTracing, this.env, this.prog)
  }

  runProgram () { this.sem.execute() }
  stepProgram () { this.sem.step() }
  stepStmtProgram () { this.sem.stepToNextStmt() }
}

export default Scamper
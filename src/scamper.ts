import { Env, Prog } from './lang.js'
import * as Parser from './parser.js'
import * as Sem from './sem.js'

import builtinLibs from './lib/builtin.js'
import Prelude from './lib/prelude.js'

export type ScamperOptions = {
  isTracing: boolean
  initialEnv?: Env
  defaultDisplay: boolean
}

export function mkOptions(): ScamperOptions {
  return {
    isTracing: false,
    initialEnv: undefined,
    defaultDisplay: true
  }
}

export class Scamper {
  env: Env
  display: HTMLElement
  isTracing: boolean
  prog: Prog
  sem: Sem.Sem

  constructor (display: HTMLElement, src: string, opts: ScamperOptions) {
    this.display = display
    this.isTracing = opts.isTracing
    if (opts.initialEnv !== undefined) {
      this.env = opts.initialEnv
    } else {
      if (Prelude.initializer !== undefined) {
        Prelude.initializer()
      }
      this.env = new Env([...Prelude.lib,])
    }
    this.prog = Parser.parseProgram(src)
    this.sem = new Sem.Sem(this.display, builtinLibs, opts.isTracing, opts.defaultDisplay, this.env, this.prog)
  }

  runProgram () { this.sem.execute() }
  stepProgram () { this.sem.step() }
  stepStmtProgram () { this.sem.stepToNextStmt() }
}

export default Scamper
import {Env, ParserOutput, Prog} from './lang.js'
import * as Parser from './parser.js'
import * as Sem from './sem.js'

import builtinLibs from './lib/index.js'
import Prelude from './lib/prelude.js'

export type ScamperOptions = {
  isTracing: boolean
  isPrintingCode: boolean
  initialEnv?: Env
  defaultDisplay: boolean
}

export function mkOptions(): ScamperOptions {
  return {
    isTracing: false,
    isPrintingCode: false,
    initialEnv: undefined,
    defaultDisplay: true
  }
}

export class Scamper {
  env: Env
  display: HTMLElement
  isTracing: boolean
  parseroutput: ParserOutput
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
    this.parseroutput = Parser.parseProgram(src)
    this.prog = this.parseroutput.prog;
    this.sem = new Sem.Sem(
      this.display,
      builtinLibs,
      // TODO: probably should just pass opts through...
      opts.isTracing,
      opts.defaultDisplay,
      opts.isPrintingCode,
      this.env,
      this.prog,
      src)
  }

  runProgram () {
    this.sem.execute();
  }

  runnerTree () {
    this.parseroutput.ast.renderTree(this.display, this.parseroutput.ast.nodes);
  }

  stepProgram () { this.sem.step() }
  stepStmtProgram () { this.sem.stepToNextStmt() }
}

export default Scamper
import * as Scheme from './scheme'
import * as LPM from './lpm'
import * as D from './display'
import builtinLibs from './lib'

export class Scamper {

  display: D.HTMLDisplay
  prog: LPM.Prog | undefined
  machine: LPM.Machine | undefined

  constructor (target: HTMLElement, src: string) {
    this.display = new D.HTMLDisplay(target)
    this.prog = Scheme.compile(this.display, src)
    if (this.prog) {
      this.machine = new LPM.Machine(
        builtinLibs,
        Scheme.mkInitialEnv(),
        this.prog,
        this.display,
        this.display,
      )
    }
  }

  runProgram () {
    if (this.machine) {
      this.machine.evaluate()
    }
  }

  runnerTree () {
    // TODO: need to update! 
    // this.parseroutput.ast.renderTree(this.display, this.parseroutput.ast.nodes);
  }

  stepProgram () {
    // TOOD: need to reimplement this!
    // this.sem.step()
  }
  stepStmtProgram () {
    // TODO: need to reimplement this!
    // this.sem.stepToNextStmt()
  }
}

export default Scamper
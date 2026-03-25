import * as Scheme from "./scheme"
import * as LPM from "./lpm"
import HtmlDisplay from "./lpm/output/html"
import builtinLibs from "./lib"

export class Scamper {
  display: HtmlDisplay
  prog: LPM.Prog | undefined
  machine: LPM.Thread | undefined

  constructor(target: HTMLElement, src: string, isTracing = false) {
    this.display = new HtmlDisplay(target)
    this.prog = Scheme.compile(this.display, src)
    if (this.prog) {
      const opts = LPM.defaultOptions
      opts.isTracing = isTracing
      this.machine = new LPM.Thread(
        "##main##",
        Scheme.mkInitialEnv(),
        this.prog,
        opts,
        builtinLibs,
        this.display,
        this.display,
        new Map([["scheme", Scheme.raiser]]),
      )
    }
  }

  send(v: LPM.Value) {
    this.display.send(v)
  }

  report(err: LPM.ScamperError) {
    this.display.report(err)
  }

  runProgram() {
    if (this.machine) {
      this.machine.evaluate()
    }
  }

  async runProgramAsync() {
    if (this.machine) {
      await this.machine.evaluateAsync()
    }
  }

  runnerTree() {
    // TODO: need to update!
    // this.parseroutput.ast.renderTree(this.display, this.parseroutput.ast.nodes);
  }

  stepProgram() {
    if (this.machine) {
      this.machine.step()
    }
  }

  stepStmtProgram() {
    if (this.machine) {
      this.machine.stepExpr()
    }
  }
}

export default Scamper

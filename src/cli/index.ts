import * as Scheme from '../scheme'
import builtinLibs from '../lib'
import * as LPM from '../lpm'
import fs from 'fs'

class ConsoleOutput implements LPM.OutputChannel, LPM.ErrorChannel {
  seenError: boolean = false
  send (v: LPM.Value): void {
    console.log(LPM.toString(v))
  }

  err (e: LPM.ScamperError): void {
    this.seenError = true
    console.error(e.toString())
  }
}

const src = fs.readFileSync(process.stdin.fd, 'utf-8');
const out = new ConsoleOutput()
const program = Scheme.compile(out, src)
if (program === undefined) { process.exit(1) }
const machine = new LPM.Machine(builtinLibs, Scheme.mkInitialEnv(), program, out, out)
machine.evaluate()
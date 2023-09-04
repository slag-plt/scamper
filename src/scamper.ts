import * as Lang from './lang.js'
import * as Parser from './parser.js'
import * as Render from './display.js'
import * as Sem from './sem.js'
import * as Value from './value.js'

import builtinLibs from './lib/builtin.js'
import Prelude from './lib/prelude.js'

class Scamper {
  env: Value.Env
  display: (v: any) => void

  constructor (outputId?: string, initialEnv?: Value.Env) {
    if (outputId !== undefined) {
      this.display = (v: any) => {
        const output = document.getElementById(outputId)
        const div = document.createElement('div')
        div.classList.add('scamper-output')
        div.appendChild(Render.renderToHTML(v))
        output!.appendChild(div)
      }
    } else {
      this.display = (v: any) => {
        console.log(Render.renderToString(v))
      }
    }
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
    // Sem.runProgram(builtinLibs, this.display, this.env, prog)
  }
}

export default Scamper
import * as Parser from './parser.js'
import * as Render from './render.js'
import * as Value from './value.js'
import * as Sem from './sem.js'

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

  runProgram (src: string) {
    const prog = Parser.parseProgram(src)
    Sem.runProgram(builtinLibs, this.display, this.env, prog)
  }
}

export default Scamper
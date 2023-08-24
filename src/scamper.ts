import * as Parser from './parser.js'
import * as Render from './render.js'
import * as Value from './value.js'
import * as Sem from './sem.js'

import builtinLibs from './lib/builtin.js'
import Prelude from './lib/prelude.js'

export function makeInitialEnv (id?: string) {
  return new Value.Env([...Prelude, ...Render.makeRenderLib(id)])
}

export function runProgram (env: Value.Env, src: string) {
  const prog = Parser.parseProgram(src)
  Sem.runProgram(builtinLibs, env, prog)
}

export function runAndReplaceProgramById (id: string) {
  const container = document.getElementById(id)!
  const src = container.innerText
  container.innerText = ''
  runProgram(makeInitialEnv(id), src)
}
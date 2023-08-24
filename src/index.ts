import * as Scamper from './scamper.js'

const prog = `
  (make-string 1)
`

Scamper.runProgram(Scamper.makeInitialEnv(), prog)
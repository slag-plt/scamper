import Scamper from './scamper.js'

const prog = `
  (display (+ 1 (* 3 4) (- 5 6 7) 8 (/ 9 10)))
`

new Scamper().runProgram(prog)
import { scamperTest } from '../harness.js'

scamperTest('tally-all', `
  (import data)
  (tally-all (list "a" "b" "a" "c" "c" "d" "b" "a" "q" "r" "r" "a" "d"))
`,[
  '(list (pair "a" 4) (pair "b" 2) (pair "c" 2) (pair "d" 2) (pair "q" 1) (pair "r" 2))'
])


import {expect, test} from 'vitest'

import builtinLibs from '../src/lib'
import * as Scheme from '../src/scheme'
import * as LPM from '../src/lpm'

function runProgram (src: string): string[] {
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(out, src)
    if (out.log.length !== 0) { return out.log }
    const machine = new LPM.Machine(
      builtinLibs,
      env,
      prog!,
      out,
      out
    )
    machine.evaluate()
    return out.log
}

export function scamperTest (label: string, src: string, expected: string[]) {
  test(label, () => expect(runProgram(src.trim())).toEqual(expected))
}

////////////////////////////////////////////////////////////////////////////////

function mkAccessorTestSource (list: string, fn: string): string {
  return `
    (define test-list ${list})
    (${fn} test-list)
  `
}

scamperTest('car', mkAccessorTestSource('(list "a" "b" "c")', 'car'), ['"a"'])
scamperTest('cdr', mkAccessorTestSource('(list "a" "b" "c")', 'cdr'), ['(list "b" "c")'])

// 4-character accessor tests
scamperTest('caar', mkAccessorTestSource('(list (list "a" "b") (list "c" "d"))', 'caar'), ['"a"'])
scamperTest('cadr', mkAccessorTestSource('(list "a" "b" "c")', 'cadr'), ['"b"'])
scamperTest('cdar', mkAccessorTestSource('(list (list "a" "b") (list "c" "d"))', 'cdar'), ['(list "b")'])
scamperTest('cddr', mkAccessorTestSource('(list "a" "b" "c")', 'cddr'), ['(list "c")'])

// 5-character accessor tests
scamperTest('caaar', mkAccessorTestSource('(list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h")))', 'caaar'), ['"a"'])
scamperTest('cadar', mkAccessorTestSource('(list (list "a" (list "b" "c")) (list "d" (list "e" "f")))', 'cadar'), ['(list "b" "c")'])
scamperTest('cdaar', mkAccessorTestSource('(list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h")))', 'cdaar'), ['(list "b")'])
scamperTest('cddar', mkAccessorTestSource('(list (list "a" "b" "c") (list "d" "e" "f"))', 'cddar'), ['(list "c")'])
scamperTest('caadr', mkAccessorTestSource('(list "a" (list "b" "c") "d")', 'caadr'), ['"b"'])
scamperTest('caddr', mkAccessorTestSource('(list "a" "b" "c" "d")', 'caddr'), ['"c"'])
scamperTest('cdadr', mkAccessorTestSource('(list "a" (list "b" "c" "d") "e")', 'cdadr'), ['(list "c" "d")'])
scamperTest('cdddr', mkAccessorTestSource('(list "a" "b" "c" "d")', 'cdddr'), ['(list "d")'])

// 6-character accessor tests
scamperTest('caaaar', mkAccessorTestSource('(list (list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h"))) (list (list (list "i" "j") (list "k" "l")) (list (list "m" "n") (list "o" "p"))))', 'caaaar'), ['"a"'])
scamperTest('cadaar', mkAccessorTestSource('(list (list (list "a" (list "b" "c")) (list "d" (list "e" "f"))) (list (list "g" (list "h" "i")) (list "j" (list "k" "l"))))', 'cadaar'), ['(list "b" "c")'])
scamperTest('cdaaar', mkAccessorTestSource('(list (list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h"))) (list (list (list "i" "j") (list "k" "l")) (list (list "m" "n") (list "o" "p"))))', 'cdaaar'), ['(list "b")'])
scamperTest('cddaar', mkAccessorTestSource('(list (list (list "a" "b" "c") (list "d" "e" "f")) (list (list "g" "h" "i") (list "j" "k" "l")))', 'cddaar'), ['(list "c")'])
scamperTest('caadar', mkAccessorTestSource('(list (list "a" (list (list "b" "c") (list "d" "e"))) (list "f" (list (list "g" "h") (list "i" "j"))))', 'caadar'), ['(list "b" "c")'])
scamperTest('caddar', mkAccessorTestSource('(list (list "a" "b" (list "c" "d")) (list "e" "f" (list "g" "h")))', 'caddar'), ['(list "c" "d")'])
scamperTest('cdadar', mkAccessorTestSource('(list (list "a" (list (list "b" "c") (list "d" "e"))) (list "f" (list (list "g" "h") (list "i" "j"))))', 'cdadar'), ['(list (list "d" "e"))'])
scamperTest('cdddar', mkAccessorTestSource('(list (list "a" "b" "c" "d") (list "e" "f" "g" "h"))', 'cdddar'), ['(list "d")'])
scamperTest('caaadr', mkAccessorTestSource('(list "a" (list (list "b" "c") (list "d" "e")) "f")', 'caaadr'), ['"b"'])
scamperTest('cadadr', mkAccessorTestSource('(list "a" (list "b" (list "c" "d")) "e")', 'cadadr'), ['(list "c" "d")'])
scamperTest('cdaadr', mkAccessorTestSource('(list "a" (list (list "b" "c") (list "d" "e")) "f")', 'cdaadr'), ['(list "c")'])
scamperTest('cddadr', mkAccessorTestSource('(list "a" (list "b" "c" "d") "e")', 'cddadr'), ['(list "d")'])
scamperTest('caaddr', mkAccessorTestSource('(list "a" "b" (list "c" "d") "e")', 'caaddr'), ['"c"'])
scamperTest('cadddr', mkAccessorTestSource('(list "a" "b" "c" "d" "e")', 'cadddr'), ['"d"'])
scamperTest('cdaddr', mkAccessorTestSource('(list "a" "b" (list "c" "d" "e") "f")', 'cdaddr'), ['(list "d" "e")'])
scamperTest('cddddr', mkAccessorTestSource('(list "a" "b" "c" "d" "e")', 'cddddr'), ['(list "e")'])


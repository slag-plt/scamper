import { expect, test } from 'vitest'
import { runProgram } from '../harness'

////////////////////////////////////////////////////////////////////////////////

function mkAccessorTestSource(list: string, fn: string): string {
  return `
    (define test-list ${list})
    (${fn} test-list)
  `
}

test('car', async () => {
  expect(await runProgram(mkAccessorTestSource('(list "a" "b" "c")', 'car'))).toEqual(
    ['"a"'],
  )
})

test('cdr', async () => {
  expect(await runProgram(mkAccessorTestSource('(list "a" "b" "c")', 'cdr'))).toEqual(
    ['(list "b" "c")'],
  )
})

// 4-character accessor tests
test('caar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list (list "a" "b") (list "c" "d"))', 'caar'),
    ),
  ).toEqual(['"a"'])
})

test('cadr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c")', 'cadr')),
  ).toEqual(['"b"'])
})

test('cdar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list (list "a" "b") (list "c" "d"))', 'cdar'),
    ),
  ).toEqual(['(list "b")'])
})

test('cddr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c")', 'cddr')),
  ).toEqual(['(list "c")'])
})

// 5-character accessor tests
test('caaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h")))',
        'caaar',
      ),
    ),
  ).toEqual(['"a"'])
})

test('cadar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" (list "b" "c")) (list "d" (list "e" "f")))',
        'cadar',
      ),
    ),
  ).toEqual(['(list "b" "c")'])
})

test('cdaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h")))',
        'cdaar',
      ),
    ),
  ).toEqual(['(list "b")'])
})

test('cddar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" "b" "c") (list "d" "e" "f"))',
        'cddar',
      ),
    ),
  ).toEqual(['(list "c")'])
})

test('caadr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" (list "b" "c") "d")', 'caadr')),
  ).toEqual(['"b"'])
})

test('caddr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c" "d")', 'caddr')),
  ).toEqual(['"c"'])
})

test('cdadr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list "a" (list "b" "c" "d") "e")', 'cdadr'),
    ),
  ).toEqual(['(list "c" "d")'])
})

test('cdddr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c" "d")', 'cdddr')),
  ).toEqual(['(list "d")'])
})

// 6-character accessor tests
test('caaaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h"))) (list (list (list "i" "j") (list "k" "l")) (list (list "m" "n") (list "o" "p"))))',
        'caaaar',
      ),
    ),
  ).toEqual(['"a"'])
})

test('cadaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list "a" (list "b" "c")) (list "d" (list "e" "f"))) (list (list "g" (list "h" "i")) (list "j" (list "k" "l"))))',
        'cadaar',
      ),
    ),
  ).toEqual(['(list "b" "c")'])
})

test('cdaaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list (list "a" "b") (list "c" "d")) (list (list "e" "f") (list "g" "h"))) (list (list (list "i" "j") (list "k" "l")) (list (list "m" "n") (list "o" "p"))))',
        'cdaaar',
      ),
    ),
  ).toEqual(['(list "b")'])
})

test('cddaar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list (list "a" "b" "c") (list "d" "e" "f")) (list (list "g" "h" "i") (list "j" "k" "l")))',
        'cddaar',
      ),
    ),
  ).toEqual(['(list "c")'])
})

test('caadar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" (list (list "b" "c") (list "d" "e"))) (list "f" (list (list "g" "h") (list "i" "j"))))',
        'caadar',
      ),
    ),
  ).toEqual(['(list "b" "c")'])
})

test('caddar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" "b" (list "c" "d")) (list "e" "f" (list "g" "h")))',
        'caddar',
      ),
    ),
  ).toEqual(['(list "c" "d")'])
})

test('cdadar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" (list (list "b" "c") (list "d" "e"))) (list "f" (list (list "g" "h") (list "i" "j"))))',
        'cdadar',
      ),
    ),
  ).toEqual(['(list (list "d" "e"))'])
})

test('cdddar', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list (list "a" "b" "c" "d") (list "e" "f" "g" "h"))',
        'cdddar',
      ),
    ),
  ).toEqual(['(list "d")'])
})

test('caaadr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list "a" (list (list "b" "c") (list "d" "e")) "f")',
        'caaadr',
      ),
    ),
  ).toEqual(['"b"'])
})

test('cadadr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list "a" (list "b" (list "c" "d")) "e")',
        'cadadr',
      ),
    ),
  ).toEqual(['(list "c" "d")'])
})

test('cdaadr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource(
        '(list "a" (list (list "b" "c") (list "d" "e")) "f")',
        'cdaadr',
      ),
    ),
  ).toEqual(['(list "c")'])
})

test('cddadr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list "a" (list "b" "c" "d") "e")', 'cddadr'),
    ),
  ).toEqual(['(list "d")'])
})

test('caaddr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list "a" "b" (list "c" "d") "e")', 'caaddr'),
    ),
  ).toEqual(['"c"'])
})

test('cadddr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c" "d" "e")', 'cadddr')),
  ).toEqual(['"d"'])
})

test('cdaddr', async () => {
  expect(
    await runProgram(
      mkAccessorTestSource('(list "a" "b" (list "c" "d" "e") "f")', 'cdaddr'),
    ),
  ).toEqual(['(list "d" "e")'])
})

test('cddddr', async () => {
  expect(
    await runProgram(mkAccessorTestSource('(list "a" "b" "c" "d" "e")', 'cddddr')),
  ).toEqual(['(list "e")'])
})

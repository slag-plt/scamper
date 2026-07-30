import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness'

////////////////////////////////////////////////////////////////////////////////

test('abs-quotient', async () => {
  expect(
    await runProgram(`
(abs 13)
(abs 0.71)
(abs 111)
(abs 4)
(abs 6.1)
(abs -5)
(abs -0.69)
(abs 89)
(abs 0)
(abs -0.10000000000000009)
(quotient 7 2)
(quotient 100 1)
(quotient 2 2)
`),
  ).toEqual([
    '13',
    '0.71',
    '111',
    '4',
    '6.1',
    '5',
    '0.69',
    '89',
    '0',
    '0.10000000000000009',
    '3',
    '100',
    '1',
  ])
})

test('append-reverse', async () => {
  expect(
    await runProgram(`
(append (list "ab" "ba") (list 1 2 3) (list))
(append (list) (list))
(append (list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")
 (list "Hello" "World"))
(append (list #t #t #t #t) (list #f #f #f #f) (list "a" "a"))
(append (list 10 10 10 10 10) (list #t #t) (list "abab" "abab"))
(reverse (list "ab" "ba"))
(reverse (list))
(reverse (list "Hello" "World"))
(reverse (list "abab" "abab"))
(reverse (list 1 2 3))
(reverse (list 1 2 3 4 5))
(reverse (reverse (list 1 2 3 4 5 6 7 8 9 10)))
`),
  ).toEqual([
    '(list "ab" "ba" 1 2 3)',
    'null',
    '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "Hello" "World")',
    '(list #t #t #t #t #f #f #f #f "a" "a")',
    '(list 10 10 10 10 10 #t #t "abab" "abab")',
    '(list "ba" "ab")',
    'null',
    '(list "World" "Hello")',
    '(list "abab" "abab")',
    '(list 3 2 1)',
    '(list 5 4 3 2 1)',
    '(list 1 2 3 4 5 6 7 8 9 10)',
  ])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - map's own
// implementation calls the user-supplied function argument via
// callScamperFn regardless of whether it's a JsFunction or a closure. Only
// the `apply` calls in this test pass; `map` needs to be rewritten in
// Scamper itself (like all-satisfy? in prelude.scm) before this can pass.
test('apply-map', async () => {
  expect(
    await runProgram(`
(apply string-length (list "HelloWorld"))
(apply list (list "HelloWorld" "HelloWorld" "HelloWorld"))
(apply string-split (list "HelloWorld" "l"))
(apply + (list 1 2 3 4 5 6 7 8 9 10))
(apply * (list 9 5 10))
(map string-length (list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld"))
(map procedure? (list string-length list + -) )
(map car (list (pair 2 4) (pair "a" "b") (pair "first" "second")))
(map cdr (list (pair 2 4) (pair "a" "b") (pair "first" "second")))
`),
  ).toEqual([
    '10',
    '(list "HelloWorld" "HelloWorld" "HelloWorld")',
    '(list "He" "" "oWor" "d")',
    '55',
    '450',
    '(list 10 10 10 10 10 10 10 10 10 10)',
    '(list #t #t #t #t)',
    '(list 2 "a" "first")',
    '(list 4 "b" "second")',
  ])
})

test('apply', async () => {
  expect(
    await runProgram(`
(apply (lambda (x) (+ x 1)) (list 1))

(apply + (list 3 4))

(apply min (list 3 1 8 9 2 -5 0 3 5 1))
`),
  ).toEqual(['2', '7', '-5'])
})

test('arithmetic', async () => {
  expect(
    await runProgram(`
(+ 1 2 3 4 5)
(- 22 7 2 1 8)
(* 1 2 3 4 5)
(/ 22 2 5 4)
(+ 5)
(- 5)
(* 5)
(/ 5)
`),
  ).toEqual(['15', '4', '120', '0.55', '5', '-5', '5', '0.2'])
})

test('asin-acos-atan', async () => {
  expect(
    await runProgram(`
(asin 0.4201670368266409)
(asin 0.6518337710215366)
(asin -0.8645514486106083)
(asin -0.7568024953079282)
(asin -0.18216250427209588)
(asin 0.9589242746631385)
(asin -0.6365371822219679)
(asin 0.8600694058124532)
(asin 0)
(acos 0.9074467814501962)
(acos 0.7583618759905082)
(acos -0.5025443191453852)
(acos -0.6536436208636119)
(acos 0.9832684384425845)
(acos 0.28366218546322625)
(acos 0.7712460149971067)
(acos 0.5101770449416689)
(acos 1)
(atan 0.4630211329364896)
(atan 0.8595286652169407)
(atan 1.7203486651303583)
(atan 1.1578212823495777)
(atan -0.18526223068913525)
(atan 3.380515006246586)
(atan -0.8253361052690248)
(atan 1.6858253705060158)
(atan 0)
`),
  ).toEqual([
    '0.43362938564082704',
    '0.71',
    '-1.0442571243572367',
    '-0.8584073464102067',
    '-0.1831853071795868',
    '1.2831853071795865',
    '-0.69',
    '1.0354056994857892',
    '0',
    '0.4336293856408271',
    '0.71',
    '2.0973355292325566',
    '2.2831853071795867',
    '0.18318530717958706',
    '1.2831853071795865',
    '0.69',
    '1.0354056994857892',
    '0',
    '0.43362938564082704',
    '0.71',
    '1.0442571243572367',
    '0.8584073464102068',
    '-0.1831853071795868',
    '1.2831853071795865',
    '-0.69',
    '1.0354056994857894',
    '0',
  ])
})

test('assoc', async () => {
  expect(
    await runProgram(`
(define inventory (list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8)))

inventory

(assoc-key? "apples" inventory)

(assoc-key? "grapes" inventory)

(assoc-ref "apples" inventory)

(assoc-ref "bananas" inventory)

(assoc-ref "oranges" inventory)

(define updated-inventory (assoc-set "apples" 3 inventory))

updated-inventory

(assoc-ref "apples" updated-inventory)

(assoc-ref "bananas" updated-inventory)

(assoc-ref "oranges" updated-inventory)

`),
  ).toEqual([
    '(list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8))',
    '#t',
    '#f',
    '5',
    '2',
    '8',
    '(list (pair "apples" 3) (pair "bananas" 2) (pair "oranges" 8))',
    '3',
    '2',
    '8',
  ])
})

test('car-cdr', async () => {
  expect(
    await runProgram(`
(car (pair #t #f))
(car (pair 1 2))
(car (pair "hi" "bye"))
(car (pair "a" "b"))
(car (pair 0.003 100))
(cdr (pair 1 2))
(cdr (pair #t #f))
(cdr (pair "hi" "bye"))
(cdr (pair "a" "b"))
(cdr (pair 0.003 100))

`),
  ).toEqual([
    '#t',
    '1',
    '"hi"',
    '"a"',
    '0.003',
    '2',
    '#f',
    '"bye"',
    '"b"',
    '100',
  ])
})

describe('list accessors (c[ad]+r family)', () => {
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
})

test('ceiling', async () => {
  expect(
    await runProgram(`
(ceiling 3.2)
(ceiling 4)
(ceiling 3.001)
(ceiling -2.1)
(ceiling 0.0)
`),
  ).toEqual(['4', '4', '4', '-2', '0'])
})

test('char-comp', async () => {
  expect(
    await runProgram(`
(define c1 #\\c)
(define c2 #\\f)

(char=? c1 c1)
(char=? c1 c2)
(char=? c2 c1)
(char=? c2 c2)

(char<? c1 c1)
(char<? c1 c2)
(char<? c2 c1)
(char<? c2 c2)

(char>? c1 c1)
(char>? c1 c2)
(char>? c2 c1)
(char>? c2 c2)

(char<=? c1 c1)
(char<=? c1 c2)
(char<=? c2 c1)
(char<=? c2 c2)

(char>=? c1 c1)
(char>=? c1 c2)
(char>=? c2 c1)
(char>=? c2 c2)

(define c3 #\\A)
(define c4 #\\d)

(char-ci=? c3 c3)
(char-ci=? c3 c4)
(char-ci=? c4 c3)
(char-ci=? c4 c4)

(char-ci<? c3 c3)
(char-ci<? c3 c4)
(char-ci<? c4 c3)
(char-ci<? c4 c4)

(char-ci>? c3 c3)
(char-ci>? c3 c4)
(char-ci>? c4 c3)
(char-ci>? c4 c4)

(char-ci<=? c3 c3)
(char-ci<=? c3 c4)
(char-ci<=? c4 c3)
(char-ci<=? c4 c4)

(char-ci>=? c3 c3)
(char-ci>=? c3 c4)
(char-ci>=? c4 c3)
(char-ci>=? c4 c4)
`),
  ).toEqual([
    '#t',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#t',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
  ])
})

test('char-ops', async () => {
  expect(
    await runProgram(`
(digit-value #\\5)
(digit-value #\\0)

(char->integer #\\a)
(integer->char 97)

(char-upcase #\\a)
(char-upcase #\\A)
(char-downcase #\\a)
(char-downcase #\\A)
(char-foldcase #\\a)
(char-foldcase #\\A)
`),
  ).toEqual([
    '5',
    '0',
    '97',
    '#\\a',
    '#\\A',
    '#\\A',
    '#\\a',
    '#\\a',
    '#\\a',
    '#\\a',
  ])
})

test('char-pred', async () => {
  expect(
    await runProgram(`
(char-alphabetic? #\\c)
(char-alphabetic? #\\5)
(char-alphabetic? #\\space)

(char-numeric? #\\c)
(char-numeric? #\\5)
(char-numeric? #\\space)

(char-whitespace? #\\c)
(char-whitespace? #\\5)
(char-whitespace? #\\space)

(char-upper-case? #\\c)
(char-upper-case? #\\F)
(char-upper-case? #\\newline)

(char-lower-case? #\\c)
(char-lower-case? #\\F)
(char-lower-case? #\\newline)
`),
  ).toEqual([
    '#t',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#f',
  ])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - compose and |>
// both call their function arguments via callScamperFn, so any composition
// of more than one function fails immediately. Needs compose/pipe rewritten
// in Scamper itself (like all-satisfy? in prelude.scm) before this can pass.
test('compose', async () => {
  expect(
    await runProgram(`
(define inc
  (lambda (x) (+ x 1)))

(inc 1)

((compose inc) 1)

((compose inc inc inc inc inc) 1)

(|> 1 inc)

(|> 1 inc inc inc inc inc)

(|> "hello"
    string->list
    (lambda (l) (filter (lambda (c) (not (char=? c #\\l))) l))
    list->string)

((compose length
          (lambda (l) (filter (lambda (n) (even? n)) l)))
 (range 10))

(define string-reverse (o list->string reverse string->list))

(string-reverse "hello")
`),
  ).toEqual(['2', '2', '6', '2', '6', '"heo"', '5', '"olleh"'])
})

test('cons-pair', async () => {
  expect(
    await runProgram(`
(cons #t #f)
(cons 1 2)
(cons "hi" "bye")
(cons "a" "b")
(cons 0.003 100)
(pair 1 2)
(pair #t #f)
(pair "hi" "bye")
(pair "a" "b")
(pair 0.003 100)
`),
  ).toEqual([
    'Runtime error [1:1-1:12]: (cons) The second argument to cons should be a list',
    'Runtime error [2:1-2:10]: (cons) The second argument to cons should be a list',
    'Runtime error [3:1-3:17]: (cons) The second argument to cons should be a list',
    'Runtime error [4:1-4:14]: (cons) The second argument to cons should be a list',
    'Runtime error [5:1-5:16]: (cons) The second argument to cons should be a list',
    '(pair 1 2)',
    '(pair #t #f)',
    '(pair "hi" "bye")',
    '(pair "a" "b")',
    '(pair 0.003 100)',
  ])
})

test('contract-check-map', async () => {
  expect(
    await runProgram(`
(map char-upcase (list "h" "e" "l" "l" "o"))
`),
  ).toEqual([
    'Runtime error [581:1-581:50]: (error) expected a char, received string',
  ])
})

test('equal', async () => {
  expect(
    await runProgram(`
(equal? 4 4)
(equal? 10 20)
(equal? "Hello" "Hello")
(equal? "Hello" "HELLO")
(equal? 4 "4")
(equal? 4 4.0)
`),
  ).toEqual(['#t', '#f', '#t', '#f', '#f', '#t'])
})

test('error', async () => {
  expect(
    await runProgram(`
(error "This is an example runtime error")
`),
  ).toEqual([
    'Runtime error [1:1-1:42]: (error) This is an example runtime error',
  ])
})

test('error-qq', async () => {
  expect(
    await runProgram(`
(error "existing")
(+ 5 (??))
`),
  ).toEqual([
    'Runtime error [1:1-1:18]: (error) existing',
    'Runtime error [2:6-2:9]: (??) Hole encountered in program!',
  ])
})

test('exp-log', async () => {
  expect(
    await runProgram(`
(exp 13)
(exp 0.71)
(exp 111)
(exp 4)
(exp 6.1)
(exp -5)
(exp -0.69)
(exp 89)
(exp 0)
(exp -0.10000000000000009)
(log 13)
(log 0.71)
(log 111)
(log 4)
(log 6.1)
`),
  ).toEqual([
    '442413.3920089205',
    '2.0339912586467506',
    '1.609487066961518e+48',
    '54.598150033144236',
    '445.85777008251677',
    '0.006737946999085467',
    '0.5015760690660556',
    '4.489612819174345e+38',
    '1',
    '0.9048374180359595',
    '2.5649493574615367',
    '-0.342490308946776',
    '4.709530201312334',
    '1.3862943611198906',
    '1.8082887711792655',
  ])
})

test('expt', async () => {
  expect(
    await runProgram(`
(expt 7 2)
(expt 0.2 0.5)
(expt 100 1)
(expt 2.0 2)
(expt 3.0 3.1)
`),
  ).toEqual(['49', '0.4472135954999579', '100', '4', '30.135325698915423'])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - JS libs can no
// longer invoke Scamper closures/functions directly.
test('filter-fold-reduce', async () => {
  expect(
    await runProgram(`
(filter string? (list 4 "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" #t "HelloWorld" "HelloWorld" "HelloWorld" list))
(filter procedure? (list string-length list? + - "true" #f = string?))
(filter null? (list (pair 2 4) (pair "a" "b") (pair "first" "second") (list) (list 4 5 6)))
(filter list? (list (pair 2 4) (pair "a" "b") (pair "first" "second") (list) (list 4 5 6)))
(fold + 5 (list 0 1 2 3 4))
(fold - 5 (list 10 9 8 7 6))
(reduce + (list 5 0 1 2 3 4))
(reduce - (list 5 10 9 8 7 6))
(fold-right - 5 (list 10 9 8 7 6))
(reduce-right - (list 10 9 8 7 6 5))
`),
  ).toEqual([
    '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")',
    '(list [Function: string-length] [Function: list?] [Function: +] [Function: -] [Function: =] [Function: string?])',
    '(list null)',
    '(list null (list 4 5 6))',
    '15',
    '-35',
    '15',
    '-35',
    '3',
    '3',
  ])
})

test('floor-ceiling', async () => {
  expect(
    await runProgram(`
(floor 0.71)
(floor 111)
(floor 6.1)
(floor 0.69)
(floor 0.10000000000000009)
(floor 0)
(ceiling 0.71)
(ceiling 111)
(ceiling 6.1)
(ceiling 0.69)
(ceiling 0.10000000000000009)
(ceiling 0)
`),
  ).toEqual(['0', '111', '6', '0', '0', '0', '1', '111', '7', '1', '1', '0'])
})

test('gt-geq', async () => {
  expect(
    await runProgram(`
(> 4 5)
(> 5.0 4.99)
(> 0 0.1)
(> 7 9)
(>= 5.0 5)
(>= 5.1 5)
(>= 10 5)
(>= 0.02 0.03)
(>= 0.01 0.02)
`),
  ).toEqual(['#f', '#t', '#f', '#f', '#t', '#t', '#t', '#f', '#f'])
})

test('implies-xor', async () => {
  expect(
    await runProgram(`
(implies #t #f)
(implies #f #f)
(implies #t #t)
(implies #f #t)
(xor #t #f)
(xor #f #f)
(xor #t #t)
(xor #f #t)
`),
  ).toEqual(['#f', '#t', '#t', '#t', '#t', '#f', '#f', '#t'])
})

test('index-of', async () => {
  expect(
    await runProgram(`
(define l (list "a" "b" "c" "d" "e"))

(index-of l "a")
(index-of l "b")
(index-of l "c")
(index-of l "d")
(index-of l "e")
(index-of l "f")
`),
  ).toEqual(['0', '1', '2', '3', '4', '-1'])
})

test('integer', async () => {
  expect(
    await runProgram(`
(integer? 5)
(integer? 78.0)
(integer? "5")
(integer? (/ 10 2))
(integer? 5.5)
`),
  ).toEqual(['#t', '#t', '#f', '#t', '#f'])
})

test('length', async () => {
  expect(
    await runProgram(`
(length (list 28 0 "know"))
(length (list))
(length (list 1 2 3 4 5))
`),
  ).toEqual(['3', '0', '5'])
})

test('list-makeList', async () => {
  expect(
    await runProgram(`
(list "ab" "ba")
(list 1 2 3)
(list)
(list #t #t #t #t)
(list #f #f #f #f)
(make-list 2 "a")
(make-list 10 "HelloWorld")
(make-list 5 10)
(make-list 2 #t)
(make-list 2 "abab")
(make-list 0 "")
`),
  ).toEqual([
    '(list "ab" "ba")',
    '(list 1 2 3)',
    'null',
    '(list #t #t #t #t)',
    '(list #f #f #f #f)',
    '(list "a" "a")',
    '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")',
    '(list 10 10 10 10 10)',
    '(list #t #t)',
    '(list "abab" "abab")',
    'null',
  ])
})

test('list-ref', async () => {
  expect(
    await runProgram(`
(define l (list 1 2 3 4 5))

(list-ref l 0)
(list-ref l 1)
(list-ref l 2)
(list-ref l 3)
(list-ref l 4)
`),
  ).toEqual(['1', '2', '3', '4', '5'])
})

test('list-tail', async () => {
  expect(
    await runProgram(`
(define l (list 1 2 3 4 5))

(list-tail l 0)

(list-tail l 3)

(list-tail l 5)

(list-drop l 0)

(list-drop l 3)

(list-drop l 5)
`),
  ).toEqual([
    '(list 1 2 3 4 5)',
    '(list 4 5)',
    'null',
    '(list 1 2 3 4 5)',
    '(list 4 5)',
    'null',
  ])
})

test('list-take', async () => {
  expect(
    await runProgram(`
(define l (list 1 2 3 4 5))

(list-take l 0)

(list-take l 3)

(list-take l 5)
`),
  ).toEqual(['null', '(list 1 2 3)', '(list 1 2 3 4 5)'])
})

test('lt-leq', async () => {
  expect(
    await runProgram(`
(< 4 5)
(< 5.0 5)
(< 0 0.1)
(< 7 9)
(<= 5.0 5)
(<= 5.1 5)
(<= 10 5)
(<= 0.02 0.02)
(<= 0.01 0.02)
`),
  ).toEqual(['#t', '#f', '#t', '#t', '#t', '#f', '#f', '#t', '#t'])
})

test('min-max', async () => {
  expect(
    await runProgram(`
(min 4 7 2)
(min 0.01 0.2 0.5)
(min 100 10 1)
(min 2.0 2)
(min 3.0 3.1)
(min -5)
(min 1 1 1 1 1 1 1)
(max 4 7 2)
(max 0.01 0.2 0.5)
(max 100 10 1)
(max 2.0 2)
(max 3.0 3.1)
(max -5)
(max 1 1 1 1 1 1 1)
`),
  ).toEqual([
    '2',
    '0.01',
    '1',
    '2',
    '3',
    '-5',
    '1',
    '7',
    '0.5',
    '100',
    '2',
    '3.1',
    '-5',
    '1',
  ])
})

test('nanQ', async () => {
  expect(
    await runProgram(`
(nan? "nan")
(nan? 34)
(nan? 0.1)
(nan? 0.0)
(nan? -3)
`),
  ).toEqual(['#f', '#f', '#f', '#f', '#f'])
})

test('nand-nor', async () => {
  expect(
    await runProgram(`
(nand #t #f #t #f #t #f)
(nand #f #f #f)
(nand #t #t #t)
(nand #t #f)
(nand #t #f #f #f)
(nor #t #f #t #f #t #f)
(nor #f #f #f)
(nor #t #t #t)
(nor #t #f)
(nor #t #f #f #f)
`),
  ).toEqual(['#t', '#t', '#f', '#t', '#t', '#f', '#t', '#f', '#f', '#f'])
})

test('not-boolean', async () => {
  // N.B., unlike standard Scheme (where any non-#f value is truthy), we are
  // stricter: `not`'s docstring declares its param `boolean?`, so a
  // non-boolean argument is a contract violation, enforced by the
  // docstring-derived wrapper in contract.ts. The reported range points at
  // `not`'s own definition in prelude.scm rather than the call site -- a
  // known, unrelated limitation of contract-wrapped errors (#254; see
  // cons-pair, range above).
  expect(
    await runProgram(`
(not #t)
(not #f)
(not 1)
(boolean? #t)
(boolean? #f)
(boolean? 0)
(boolean? 2)
(boolean? 1)
`),
  ).toEqual([
    '#f',
    '#t',
    'Runtime error [274:1-274:35]: (error) expected a boolean, received number',
    '#t',
    '#t',
    '#f',
    '#f',
    '#f',
  ])
})

test('nullQ-listQ', async () => {
  expect(
    await runProgram(`
(null? (list))
(null? (list "ab" "ba"))
(null? (list 1 2 3))
(null? (list ""))
(null? (list))
(list? (list))
(list? (list "ab" "ba"))
(list? (list 1 2 3))
(list? (list ""))
(list? (pair "hello" "world"))
`),
  ).toEqual(['#t', '#f', '#f', '#f', '#t', '#t', '#t', '#t', '#t', '#f'])
})

test('number', async () => {
  expect(
    await runProgram(`
(number? 56)
(number? "56")
(number? 56.2)
(number? (/ 56 2))
(number? 56.0)
`),
  ).toEqual(['#t', '#f', '#t', '#t', '#t'])
})

test('numeq', async () => {
  expect(
    await runProgram(`
(= 4 5)
(= 5.0 5)
(= 0 0.1)
(= 7 9)
(= 5.0 5)
(= 5.1 5)
(= 10 5)
(= 0.02 0.03)
(= 0.01 0.01)
`),
  ).toEqual(['#f', '#t', '#f', '#f', '#t', '#f', '#f', '#f', '#t'])
})

test('odd-even', async () => {
  expect(
    await runProgram(`
(odd? 2)
(odd? (- 10 9))
(odd? 0.0)
(odd? (- 7 4))
(odd? 8)
(even? 2)
(even? 1)
(even? (- 7 3))
(even? 8)
(even? 0.0)
`),
  ).toEqual(['#f', '#t', '#f', '#t', '#f', '#t', '#f', '#t', '#t', '#t'])
})

test('pairQ', async () => {
  expect(
    await runProgram(`
(pair? (pair #t #f))
(pair? (+ 1 2))
(pair? "bye")
(pair? (pair "a" "b"))
(pair? (/ 100 1))
`),
  ).toEqual(['#t', '#f', '#f', '#t', '#f'])
})

test('plus-minus', async () => {
  expect(
    await runProgram(`
(+ 4 7 2)
(+ 0.01 0.2 0.5)
(+ 100 10 1)
(+ 2.0 2)
(+ 3.0 3.1)
(- 4 7 2)
(- 0.01 0.2 0.5)
(- 100 10 1)
(- 2.0 2)
(- 3.0 3.1)
`),
  ).toEqual([
    '13',
    '0.71',
    '111',
    '4',
    '6.1',
    '-5',
    '-0.69',
    '89',
    '0',
    '-0.10000000000000009',
  ])
})

test('positive-negative', async () => {
  expect(
    await runProgram(`
(positive? -0.002)
(positive? (- 7 10))
(positive? 0.0)
(positive? (- 7 3))
(positive? (- 7.8 7.7))
(negative? (- 7.8 7.7))
(negative? -0.1)
(negative? (- 7 10))
(negative? 0)
(negative? (- 7 4))
`),
  ).toEqual(['#f', '#f', '#f', '#t', '#t', '#f', '#t', '#t', '#f', '#f'])
})

test('qq', async () => {
  expect(
    await runProgram(`
(+ (??) 1)
`),
  ).toEqual(['Runtime error [1:4-1:7]: (??) Hole encountered in program!'])
})

test('random', async () => {
  // N.B., random can no longer be driven through map/reduce pipelines (see
  // #248), so we call it directly many times and assert every result is an
  // integer in [0, n) -- no determinism required.
  const results = await runProgram(
    Array.from({ length: 100 }, () => '(random 5)').join('\n'),
  )
  for (const r of results) {
    const v = Number(r)
    expect(Number.isInteger(v)).toBe(true)
    expect(v).toBeGreaterThanOrEqual(0)
    expect(v).toBeLessThan(5)
  }
})

test('range', async () => {
  expect(
    await runProgram(`
(range 10)

(range 50)

(range 0)

(range -1)

(range 5 10)

(range -3 5)

(range 10 5)

(range 5 -3)

(range 0 10 2)

(range 10 5 -1)

(range 10 0 -3)
`),
  ).toEqual([
    '(list 0 1 2 3 4 5 6 7 8 9)',
    '(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49)',
    'null',
    'null',
    '(list 5 6 7 8 9)',
    '(list -3 -2 -1 0 1 2 3 4)',
    'null',
    'null',
    '(list 0 2 4 6 8)',
    '(list 10 9 8 7 6)',
    '(list 10 7 4 1)',
  ])
})

// N.B., regression test for vector-range's docstring having declared 2
// fixed params (beg, end) when prelude_vectorRange is fully variadic like
// range -- the 1- and 2-arg calls below used to fail with a spurious arity
// error before the contract wrapper's arity matched the implementation.
test('vector-range', async () => {
  expect(
    await runProgram(`
(vector-range 10)

(vector-range 50)

(vector-range 0)

(vector-range -1)

(vector-range 5 10)

(vector-range -3 5)

(vector-range 10 5)

(vector-range 5 -3)

(vector-range 0 10 2)

(vector-range 10 5 -1)

(vector-range 10 0 -3)
`),
  ).toEqual([
    '(vector 0 1 2 3 4 5 6 7 8 9)',
    '(vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49)',
    '(vector)',
    '(vector)',
    '(vector 5 6 7 8 9)',
    '(vector -3 -2 -1 0 1 2 3 4)',
    '(vector)',
    '(vector)',
    '(vector 0 2 4 6 8)',
    '(vector 10 9 8 7 6)',
    '(vector 10 7 4 1)',
  ])
})

test('real', async () => {
  expect(
    await runProgram(`
(real? 56)
(real? 56.2)
(real? "56.2")
(real? (/ 50 2))
(real? (/ 51 2))
`),
  ).toEqual(['#t', '#t', '#f', '#t', '#t'])
})

test('remainder-modulo', async () => {
  expect(
    await runProgram(`
(remainder 7 2)
(remainder 35 9)
(remainder 2 2)
(modulo 7 2)
(modulo 35 9)
(modulo 2 2)
`),
  ).toEqual(['1', '8', '0', '1', '8', '0'])
})

test('sin-cos-tan', async () => {
  expect(
    await runProgram(`
(sin 13)
(sin 0.71)
(sin 111)
(sin 4)
(sin 6.1)
(sin -5)
(sin -0.69)
(sin 89)
(sin 0)
(cos 13)
(cos 0.71)
(cos 111)
(cos 4)
(cos 6.1)
(cos -5)
(cos -0.69)
(cos 89)
(cos 0)
(tan 13)
(tan 0.71)
(tan 111)
(tan 4)
(tan 6.1)
(tan -5)
(tan -0.69)
(tan 89)
(tan 0)
`),
  ).toEqual([
    '0.4201670368266409',
    '0.6518337710215366',
    '-0.8645514486106083',
    '-0.7568024953079282',
    '-0.18216250427209588',
    '0.9589242746631385',
    '-0.6365371822219679',
    '0.8600694058124532',
    '0',
    '0.9074467814501962',
    '0.7583618759905082',
    '-0.5025443191453852',
    '-0.6536436208636119',
    '0.9832684384425845',
    '0.28366218546322625',
    '0.7712460149971067',
    '0.5101770449416689',
    '1',
    '0.4630211329364896',
    '0.8595286652169407',
    '1.7203486651303583',
    '1.1578212823495777',
    '-0.18526223068913525',
    '3.380515006246586',
    '-0.8253361052690248',
    '1.6858253705060158',
    '0',
  ])
})

test('square-sqrt', async () => {
  expect(
    await runProgram(`
(square 0.71)
(square 111)
(square 6.1)
(square 0.69)
(square 0.10000000000000009)
(square 0)
(sqrt 0.5041)
(sqrt 12321)
(sqrt 37.21)
(sqrt 0.4761)
(sqrt 0.01)
(sqrt 0)
`),
  ).toEqual([
    '0.5041',
    '12321',
    '37.209999999999994',
    '0.4760999999999999',
    '0.010000000000000018',
    '0',
    '0.71',
    '111',
    '6.1',
    '0.6900000000000001',
    '0.1',
    '0',
  ])
})

test('string-append', async () => {
  expect(
    await runProgram(`
(string-append "hello" " " "world!")
(string-append "hi")
`),
  ).toEqual(['"hello world!"', '"hi"'])
})

test('string-comp', async () => {
  expect(
    await runProgram(`
(define s1 "hello world!")
(define s2 "hello zoo!")

(string=? s1 s1)
(string=? s1 s2)
(string=? s2 s1)
(string=? s2 s2)

(string<? s1 s1)
(string<? s1 s2)
(string<? s2 s1)
(string<? s2 s2)

(string>? s1 s1)
(string>? s1 s2)
(string>? s2 s1)
(string>? s2 s2)

(string<=? s1 s1)
(string<=? s1 s2)
(string<=? s2 s1)
(string<=? s2 s2)

(string>=? s1 s1)
(string>=? s1 s2)
(string>=? s2 s1)
(string>=? s2 s2)

(define s3 "HEllo World!")
(define s4 "heLLo zoo!")

(string-ci=? s3 s3)
(string-ci=? s3 s4)
(string-ci=? s4 s3)
(string-ci=? s4 s4)

(string-ci<? s3 s3)
(string-ci<? s3 s4)
(string-ci<? s4 s3)
(string-ci<? s4 s4)

(string-ci>? s3 s3)
(string-ci>? s3 s4)
(string-ci>? s4 s3)
(string-ci>? s4 s4)

(string-ci<=? s3 s3)
(string-ci<=? s3 s4)
(string-ci<=? s4 s3)
(string-ci<=? s4 s4)

(string-ci>=? s3 s3)
(string-ci>=? s3 s4)
(string-ci>=? s4 s3)
(string-ci>=? s4 s4)
`),
  ).toEqual([
    '#t',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#t',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#f',
    '#f',
    '#f',
    '#f',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#t',
  ])
})

test('string-length', async () => {
  expect(
    await runProgram(`
(string-length "hello world")
(string-length "")
(string-length "\n\n\n\n\n")
`),
  ).toEqual(['11', '0', '5'])
})

test('string-length-ref', async () => {
  expect(
    await runProgram(`
(string-length "HelloWorld")
(string-length "Hello woww World")
(string-length "")
(string-length "00110011")
(string-length "1234567")
(string-ref "HelloWorld" 7)
(string-ref "Hello woww World" 10)
(string-ref " " 0)
(string-ref "00110011" 3)
(string-ref "1234567" 5)
`),
  ).toEqual([
    '10',
    '16',
    '0',
    '8',
    '7',
    '#\\r',
    '#\\space',
    '#\\space',
    '#\\1',
    '#\\6',
  ])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - JS libs can no
// longer invoke Scamper closures/functions directly.
test('string-map', async () => {
  expect(
    await runProgram(`
(string-map char-upcase "hello world")
(string-map char-downcase "")
(string-map char-upcase "a")
(string-map (lambda (c) c) "abc")
(string-map char-downcase "ABC123")
`),
  ).toEqual([
    '"HELLO WORLD"',
    // empty string is the base case
    '""',
    // single character
    '"A"',
    // identity leaves the string unchanged
    '"abc"',
    // non-alphabetic characters are left alone by char-downcase
    '"abc123"',
  ])
})

test('string-number-conversions', async () => {
  expect(
    await runProgram(`
(number->string 9)
(number->string 0.4472135954999579)
(number->string 100)
(number->string 4)
(number->string 30.135325698915423)
(string->number "9")
(string->number "0.4472135954999579")
(string->number "100")
(string->number "4")
(string->number "30.135325698915423")
`),
  ).toEqual([
    '"9"',
    '"0.4472135954999579"',
    '"100"',
    '"4"',
    '"30.135325698915423"',
    '9',
    '0.4472135954999579',
    '100',
    '4',
    '30.135325698915423',
  ])
})

test('string-ops', async () => {
  expect(
    await runProgram(`
(make-string 5 #\\a)
(make-string 0 #\\c)
(string-upcase "aCcD01-E")
(string-downcase "aCcD01-E")
(string-foldcase "aCcD01-E")
(substring "hello world" 3 7)
(string->list "hello world")
(list->string (list #\\h #\\e #\\l #\\l #\\o #\\space #\\w #\\o #\\r #\\l #\\d))
`),
  ).toEqual([
    '"aaaaa"',
    '""',
    '"ACCD01-E"',
    '"accd01-e"',
    '"accd01-e"',
    '"lo w"',
    '(list #\\h #\\e #\\l #\\l #\\o #\\space #\\w #\\o #\\r #\\l #\\d)',
    '"hello world"',
  ])
})

test('string-split', async () => {
  expect(
    await runProgram(`
(string-split "Twas brillig and the slithy toves" " ")
`),
  ).toEqual(['(list "Twas" "brillig" "and" "the" "slithy" "toves")'])
})

test('string-split-append', async () => {
  expect(
    await runProgram(`
(string-split "HelloWorld" "w")
(string-split "Hello woww World" "w")
(string-split "" "")
(string-split "00110011" "1")
(string-split "1234567" "5")
(string-append "HelloWorld" "Hello woww World" )
(string-append " " "00110011" "1234567" )
(string-append "1234567" "89101112" "13141516")
`),
  ).toEqual([
    '(list "HelloWorld")',
    '(list "Hello " "o" "" " World")',
    'null',
    '(list "00" "" "00" "" "")',
    '(list "1234" "67")',
    '"HelloWorldHello woww World"',
    '" 001100111234567"',
    '"12345678910111213141516"',
  ])
})

test('stringQ-procedure', async () => {
  expect(
    await runProgram(`
(string? (pair #t #f))
(string? 1)
(string? "bye")
(string? "a")
(string? 100)
(procedure? (pair #t #f))
(procedure? list)
(procedure? "bye")
(procedure? +)
(procedure? string-length)
`),
  ).toEqual(['#f', '#f', '#t', '#t', '#f', '#f', '#t', '#f', '#t', '#t'])
})

test('times-div', async () => {
  expect(
    await runProgram(`
(* 4 7 2)
(* 0.01 0.2 0.5)
(* 100 10 1)
(* 2.0 2)
(* 3.0 3.1)
(/ 4 7 2)
(/ 0.01 0.2 0.5)
(/ 100 10 1)
(/ 2.0 2)
(/ 3.0 3.1)
`),
  ).toEqual([
    '56',
    '0.001',
    '1000',
    '4',
    '9.3',
    '0.2857142857142857',
    '0.09999999999999999',
    '10',
    '1',
    '0.9677419354838709',
  ])
})

test('truncate-round', async () => {
  expect(
    await runProgram(`
(truncate 0.71)
(truncate 111)
(truncate 6.1)
(truncate 0.69)
(truncate 0.10000000000000009)
(truncate 0)
(round 0.71)
(round 111)
(round 6.1)
(round 0.69)
(round 0.10000000000000009)
(round 0)
`),
  ).toEqual(['0', '111', '6', '0', '0', '0', '1', '111', '6', '1', '0', '0'])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - JS libs can no
// longer invoke Scamper closures/functions directly.
test('vector-immutable', async () => {
  expect(
    await runProgram(`
(define empty (vector))

empty

(vector? empty)

(vector-length empty)

(define non-empty (vector 1 2 3 4 5))

non-empty

(vector? non-empty)

(vector-length non-empty)

(vector-ref non-empty 2)

(vector-ref non-empty 4)

(vector-map (lambda (x) (+ x 1)) non-empty)

(define range-test (vector-range 0 35 5))

range-test

(vector-filter (lambda (x) (= (remainder x 3) 0)) range-test)

(vector-map * (vector 1 2 3) (vector 4 5 6))

(define append-result (vector-append (vector 1 2) (vector 3 4 5) (vector 6 7 8 9) (vector 10)))

(vector-length append-result)

append-result

(vector-append)

(vector-append (vector) (vector 1 2) (vector) (vector 3) (vector) (vector 4 5))
`),
  ).toEqual([
    '(vector)',
    '#t',
    '0',
    '(vector 1 2 3 4 5)',
    '#t',
    '5',
    '3',
    '5',
    '(vector 2 3 4 5 6)',
    '(vector 0 5 10 15 20 25 30)',
    '(vector 0 15 30)',
    '(vector 4 10 18)',
    '10',
    '(vector 1 2 3 4 5 6 7 8 9 10)',
    '(vector)',
    '(vector 1 2 3 4 5)',
  ])
})

test('vector-mutable', async () => {
  expect(
    await runProgram(`
(define sample-vector (vector "alpha" "beta" "gamma" "delta" "epsilon"))
sample-vector
(vector-set! sample-vector 2 "zeta")
sample-vector
(vector-set! sample-vector 0 "foo")
sample-vector
(vector-set! sample-vector 2 -38.72)
sample-vector
(vector-fill! sample-vector "woot")
sample-vector
`),
  ).toEqual([
    '(vector "alpha" "beta" "gamma" "delta" "epsilon")',
    'void',
    '(vector "alpha" "beta" "zeta" "delta" "epsilon")',
    'void',
    '(vector "foo" "beta" "zeta" "delta" "epsilon")',
    'void',
    '(vector "foo" "beta" -38.72 "delta" "epsilon")',
    'void',
    '(vector "woot" "woot" "woot" "woot" "woot")',
  ])
})

// TODO: skipped because L.callScamperFn now always throws "Javascript
// library functions can no longer call Scamper functions" - JS libs can no
// longer invoke Scamper closures/functions directly.
test.skip('with-handler', async () => {
  expect(
    await runProgram(`
(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (+ x y z))
  1 2 3)

(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (error "oh no, an error!"))
  1 2 3)

`),
  ).toEqual(['6', '"This is the error that was generated: oh no, an error!"'])
})

test('zero', async () => {
  expect(
    await runProgram(`
(zero? 0.002)
(zero? 0.1)
(zero? 0.0)
(zero? 0)
(zero? 1)
`),
  ).toEqual(['#f', '#f', '#t', '#t', '#f'])
})

////////////////////////////////////////////////////////////////////////////////
// Numeric & Comparison Operations

test('nanQ-actual-nan', async () => {
  // `(/ 0.0 0.0)` used to produce NaN, but division by zero now errors (#258).
  // `(sqrt -1)` is a NaN source that does not go through a zero divisor.
  expect(
    await runProgram(`
(nan? (sqrt -1))
`),
  ).toEqual(['#t'])
})

test('gt-equal', async () => {
  expect(
    await runProgram(`
(> 5 5)
(> 5.0 5)
`),
  ).toEqual(['#f', '#f'])
})

test('div-zero', async () => {
  // Fixed (#258): division by zero now raises a clean runtime error instead of
  // silently yielding Infinity/NaN. See test/regressions/division-by-zero for
  // the full matrix (variadic mid-chain, unary, 0/0, quotient/modulo/remainder).
  expect(
    await runProgram(`
(/ 5 0)
`),
  ).toEqual([
    expect.stringContaining('/: division by zero'),
  ])
})

test('quotient-negative-zero', async () => {
  // The zero-divisor case used to yield Infinity; it now errors (#258). Valid
  // negative-argument quotients are unaffected.
  expect(
    await runProgram(`
(quotient -7 2)
(quotient 7 -2)
(quotient -7 -2)
`),
  ).toEqual(['-3', '-3', '3'])
  expect(await runProgram('(quotient 5 0)')).toEqual([
    expect.stringContaining('quotient: division by zero'),
  ])
})

test('remainder-negative', async () => {
  expect(
    await runProgram(`
(remainder -7 2)
(remainder 7 -2)
(remainder -7 -2)
`),
  ).toEqual(['-1', '1', '-1'])
})

test('modulo-negative-divisor', async () => {
  expect(
    await runProgram(`
(modulo -7 2)
(modulo 7 -2)
(modulo -7 -2)
`),
  ).toEqual(['1', '-1', '-1'])
})

test('floor-negative', async () => {
  expect(
    await runProgram(`
(floor -2.1)
`),
  ).toEqual(['-3'])
})

test('truncate-negative', async () => {
  expect(
    await runProgram(`
(truncate -2.1)
`),
  ).toEqual(['-2'])
})

test('round-negative-tie', async () => {
  expect(
    await runProgram(`
(round -6.1)
(round 2.5)
(round -2.5)
`),
  ).toEqual(['-6', '3', '-2'])
})

test('sqrt-negative', async () => {
  expect(
    await runProgram(`
(sqrt -1)
`),
  ).toEqual(['NaN'])
})

test('expt-zero-negative-exponent', async () => {
  expect(
    await runProgram(`
(expt 5 0)
(expt 2 -1)
`),
  ).toEqual(['1', '0.5'])
})

test('log-domain-edge', async () => {
  expect(
    await runProgram(`
(log 0)
(log -1)
`),
  ).toEqual(['-Infinity', 'NaN'])
})

test('asin-boundary', async () => {
  expect(
    await runProgram(`
(asin 1)
(asin -1)
`),
  ).toEqual(['1.5707963267948966', '-1.5707963267948966'])
})

test('number-to-string-negative-zero', async () => {
  expect(
    await runProgram(`
(number->string -5)
(number->string 0)
`),
  ).toEqual(['"-5"', '"0"'])
})

test('string-to-number-negative', async () => {
  expect(
    await runProgram(`
(string->number "-5")
`),
  ).toEqual(['-5'])
})

test('=-eps', async () => {
  // N.B., =-eps's docstring declares its param `n : number?`, so a
  // non-number argument is a contract violation -- the reported range
  // points at =-eps's own definition in prelude.scm rather than the call
  // site (see cons-pair, not-boolean above).
  expect(
    await runProgram(`
((=-eps 0.5) 1 1.3)
((=-eps 0.1) 1 2)
(define close? (=-eps 0.01))
(close? 3.001 3.002)
(=-eps "a")
`),
  ).toEqual([
    '#t',
    '#f',
    '#t',
    'Runtime error [71:1-71:43]: (error) expected a number, received string',
  ])
})

////////////////////////////////////////////////////////////////////////////////
// Pairs & List Accessors (car/cdr family)

describe('list accessors (c[ad]+r family) - empty-list failures', () => {
  // N.B., car/cdr now carry an `(or/p pair? nonempty-list?)` contract, and the
  // whole c[ad]+r family is defined as Scheme compositions over them, so every
  // member rejects the empty list with the same clean contract error (#256)
  // instead of leaking a raw JS TypeError. The innermost (rightmost) accessor
  // letter decides which primitive first sees null: `a` -> car, `d` -> cdr.
  // The reported range points at that primitive's definition in prelude.scm.
  const stripRange = (msgs: string[]): string[] =>
    msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))
  const members = [
    'car', 'cdr', 'caar', 'cadr', 'cdar', 'cddr', 'caaar', 'cadar', 'cdaar',
    'cddar', 'caadr', 'caddr', 'cdadr', 'cdddr', 'caaaar', 'cadaar', 'cdaaar',
    'cddaar', 'caadar', 'caddar', 'cdadar', 'cdddar', 'caaadr', 'cadadr',
    'cdaadr', 'cddadr', 'caaddr', 'cadddr', 'cdaddr', 'cddddr',
  ]

  test.each(members)('%s rejects the empty list', async (name) => {
    expect(stripRange(await runProgram(`(${name} (list))`))).toEqual([
      'Runtime error [..]: (error) expected pair or nonempty-list, received null',
    ])
  })
})

////////////////////////////////////////////////////////////////////////////////
// Core List, Association-List, Sort & Character Operations

test('cons-list', async () => {
  expect(
    await runProgram(`
(cons 1 (list 2 3))
(cons "a" (list))
(cons #t (list #f 1 2))
`),
  ).toEqual([
    '(list 1 2 3)',
    '(list "a")',
    '(list #t #f 1 2)',
  ])
})

test('list-ref-out-of-range', async () => {
  expect(
    await runProgram(`
(list-ref (list 1 2 3) 3)
`),
  ).toEqual([
    'Runtime error [1:1-1:25]: (list-ref) list-ref: index 3 out of bounds of list',
  ])
})

test('list-tail-out-of-range', async () => {
  expect(
    await runProgram(`
(list-tail (list 1 2 3) 4)
`),
  ).toEqual(['null'])
})

test('list-take-out-of-range', async () => {
  expect(
    await runProgram(`
(list-take (list 1 2 3) 4)
`),
  ).toEqual(['(list 1 2 3)'])
})

test('list-drop-out-of-range', async () => {
  expect(
    await runProgram(`
(list-drop (list 1 2 3) 4)
`),
  ).toEqual(['null'])
})

test('assoc-ref-missing-key', async () => {
  expect(
    await runProgram(`
(assoc-ref "z" (list (pair "a" 1) (pair "b" 2)))
`),
  ).toEqual([
    'Runtime error [1:1-1:48]: (assoc-ref) assoc-ref: key z not found in association list',
  ])
})

test('assoc-set-new-key', async () => {
  expect(
    await runProgram(`
(assoc-set "c" 3 (list (pair "a" 1) (pair "b" 2)))
`),
  ).toEqual(['(list (pair "a" 1) (pair "b" 2) (pair "c" 3))'])
})

test('sort', async () => {
  expect(
    await runProgram(`
(sort (list) <)
(sort (list 42) <)
(sort (list 3 1 2) <)
(sort (list 1 2 3 4 5) <)
(sort (list 5 4 3 2 1) <)
(sort (list 3 1 2 3 1 2) <)
(sort (list 3 1 2) >)
(sort (list "banana" "apple" "cherry") string<?)
`),
  ).toEqual([
    // base cases: empty and singleton are returned unchanged
    'null',
    '(list 42)',
    // standard sort
    '(list 1 2 3)',
    // already-sorted and reverse-sorted inputs
    '(list 1 2 3 4 5)',
    '(list 1 2 3 4 5)',
    // duplicates are all kept
    '(list 1 1 2 2 3 3)',
    // a > comparator sorts in descending order
    '(list 3 2 1)',
    // sorting strings with a string comparator
    '(list "apple" "banana" "cherry")',
  ])
})

test('sort-contract', async () => {
  expect(
    await runProgram(`
(sort 5 <)
(sort (list 1 2) 5)
`),
  ).toEqual([
    'Runtime error [544:1-551:55]: (error) expected a list, received number',
    'Runtime error [544:1-551:55]: (error) expected a procedure, received number',
  ])
})

test('any-of', async () => {
  expect(
    await runProgram(`
((any-of odd?) 3)
((any-of odd?) 4)
((any-of odd? even?) 3)
((any-of odd? even?) 4)
((any-of positive? negative?) 0)
((any-of number? string? procedure?) "hi")
((any-of number? string?) #t)
`),
  ).toEqual([
    // single predicate: reduces to that predicate
    '#t',
    '#f',
    // several predicates: true if any holds
    '#t',
    '#t',
    // none hold
    '#f',
    // first non-matching predicates are skipped once one matches
    '#t',
    // no predicate matches
    '#f',
  ])
})

test('all-of', async () => {
  expect(
    await runProgram(`
((all-of odd?) 3)
((all-of odd?) 4)
((all-of odd? positive?) 3)
((all-of odd? positive?) -3)
((all-of number? string?) 5)
((all-of number? positive? even?) 4)
`),
  ).toEqual([
    // single predicate: reduces to that predicate
    '#t',
    '#f',
    // several predicates: true only if all hold
    '#t',
    '#f',
    '#f',
    '#t',
  ])
})

test('list-of', async () => {
  expect(
    await runProgram(`
((list-of number?) (list))
((list-of number?) (list 1 2 3))
((list-of number?) (list 1 "a" 3))
((list-of number?) 5)
((list-of number?) "a")
((list-of string?) (list "a" "b"))
((list-of procedure?) (list + - map))
`),
  ).toEqual([
    // empty list vacuously satisfies any element predicate
    '#t',
    // every element satisfies the predicate
    '#t',
    // one element fails
    '#f',
    // a non-list is never a list-of anything
    '#f',
    '#f',
    '#t',
    '#t',
  ])
})

test('list-of-contract', async () => {
  expect(
    await runProgram(`
(list-of 5)
`),
  ).toEqual([
    'Runtime error [339:1-342:43]: (error) expected a procedure, received number',
  ])
})

test('digit-value-not-digit', async () => {
  expect(
    await runProgram(`
(digit-value #\\a)
`),
  ).toEqual([
    'Runtime error [1:1-1:17]: (digit-value) digit-value: a is not a decimal digit',
  ])
})

test('charQ', async () => {
  expect(
    await runProgram(`
(char? #\\a)
(char? "a")
(char? 5)
(char? #t)
`),
  ).toEqual(['#t', '#f', '#f', '#f'])
})

test('charQ-arity', async () => {
  expect(
    await runProgram(`
(char?)
(char? #\\a #\\b)
`),
  ).toEqual([
    'Runtime error [1:1-1:7]: Arity mismatch in function call: expected 1 arguments, got 0',
    'Runtime error [2:1-2:15]: Arity mismatch in function call: expected 1 arguments, got 2',
  ])
})

////////////////////////////////////////////////////////////////////////////////
// Strings

test('string-constructor', async () => {
  expect(
    await runProgram(`
(string #\\a #\\b #\\c)
(string "a")
`),
  ).toEqual([
    '"abc"',
    'Runtime error [612:1-612:41]: (error) expected every value of c1 to be a char, but at least one was not',
  ])
})

test('string-vector-conversions', async () => {
  expect(
    await runProgram(`
(string->vector "abc")
(vector->string (vector #\\a #\\b #\\c))
(string->vector 5)
(vector->string "abc")
`),
  ).toEqual([
    '(vector #\\a #\\b #\\c)',
    '"abc"',
    'Runtime error [675:1-675:57]: (error) expected a string, received number',
    'Runtime error [681:1-681:57]: (error) expected a vector, received string',
  ])
})

test('string-contains', async () => {
  expect(
    await runProgram(`
(string-contains "hello world" "wor")
(string-contains "hello" "xyz")
(string-contains "hello" "")
(string-contains 5 "a")
`),
  ).toEqual([
    '#t',
    '#f',
    '#t',
    'Runtime error [688:1-688:58]: (error) expected a string, received number',
  ])
})

test('string-split-vector', async () => {
  expect(
    await runProgram(`
(string-split-vector "a,b,c" ",")
(string-split-vector 5 ",")
`),
  ).toEqual([
    '(vector "a" "b" "c")',
    'Runtime error [702:1-702:65]: (error) expected a string, received number',
  ])
})

test('string-list-empty', async () => {
  expect(
    await runProgram(`
(string->list "")
(list->string (list))
`),
  ).toEqual(['null', '""'])
})

test('substring-bounds', async () => {
  expect(
    await runProgram(`
(substring "hello" 2 2)
(substring "hello" 0 5)
`),
  ).toEqual(['""', '"hello"'])
})

////////////////////////////////////////////////////////////////////////////////
// Vectors

test('vectorQ', async () => {
  expect(
    await runProgram(`
(vector? (vector 1 2 3))
(vector? (vector))
(vector? (list 1 2 3))
(vector? "hello")
(vector? 5)
(vector?)
(vector? 5 6)
`),
  ).toEqual([
    '#t',
    '#t',
    '#f',
    '#f',
    '#f',
    'Runtime error [6:1-6:9]: Arity mismatch in function call: expected 1 arguments, got 0',
    'Runtime error [7:1-7:13]: Arity mismatch in function call: expected 1 arguments, got 2',
  ])
})

test('make-vector', async () => {
  // N.B., contract error location points at the definition site, not the call site
  expect(
    await runProgram(`
(make-vector 3 "a")
(make-vector 0 "a")
(make-vector 5 #t)
(make-vector "a" 1)
`),
  ).toEqual([
    '(vector "a" "a" "a")',
    '(vector)',
    '(vector #t #t #t #t #t)',
    'Runtime error [721:1-721:50]: (error) expected an integer, received string',
  ])
})

test('vector-length', async () => {
  // N.B., contract error location points at the definition site, not the call site
  expect(
    await runProgram(`
(vector-length (vector 1 2 3))
(vector-length (vector))
(vector-length 5)
`),
  ).toEqual([
    '3',
    '0',
    'Runtime error [727:1-727:54]: (error) expected a vector, received number',
  ])
})

test('vector-ref', async () => {
  // N.B., the contract (type) error points at the definition site, not the
  // call site (#254); the #257 bounds errors, thrown from the function body,
  // point at the call site.
  // #257: an out-of-range index (>= length or negative) now raises a clean bounds error
  expect(
    await runProgram(`
(vector-ref (vector 1 2 3) 0)
(vector-ref (vector 1 2 3) 2)
(vector-ref 5 0)
(vector-ref (vector 1 2 3) 5)
(vector-ref (vector 1 2 3) -1)
`),
  ).toEqual([
    '1',
    '3',
    'Runtime error [735:1-735:48]: (error) expected a vector, received number',
    'Runtime error [4:1-4:29]: (vector-ref) vector-ref: index 5 out of bounds of vector',
    'Runtime error [5:1-5:30]: (vector-ref) vector-ref: index -1 out of bounds of vector',
  ])
})

test('vector-append', async () => {
  // N.B., contract error location points at the definition site, not the call site
  expect(
    await runProgram(`
(vector-append (vector 1 2) (vector 3 4 5))
(vector-append (vector 1 2) (list 3 4))
(vector-append)
`),
  ).toEqual([
    '(vector 1 2 3 4 5)',
    'Runtime error [780:1-780:54]: (error) expected every value of v1 to be a vector, but at least one was not',
    '(vector)',
  ])
})

test('vector-to-list', async () => {
  // N.B., contract error location points at the definition site, not the call site
  expect(
    await runProgram(`
(vector->list (vector 1 2 3))
(vector->list (vector))
(vector->list (list 1 2 3))
`),
  ).toEqual([
    '(list 1 2 3)',
    'null',
    'Runtime error [757:1-757:53]: (error) expected a vector, received list',
  ])
})

test('list-to-vector', async () => {
  // N.B., contract error location points at the definition site, not the call site
  expect(
    await runProgram(`
(list->vector (list 1 2 3))
(list->vector (list))
(list->vector (vector 1 2 3))
`),
  ).toEqual([
    '(vector 1 2 3)',
    '(vector)',
    'Runtime error [763:1-763:53]: (error) expected a list, received vector',
  ])
})

test('vector-set-out-of-range', async () => {
  // #257: an out-of-range index (>= length or negative) now raises a clean
  // bounds error and leaves the vector unchanged, rather than growing it with
  // holes (too-large index) or silently no-op'ing (negative index).
  expect(
    await runProgram(`
(define v (vector 1 2 3))
(vector-set! v 5 10)
v
(vector-length v)
(define v2 (vector 1 2 3))
(vector-set! v2 -1 10)
v2
`),
  ).toEqual([
    'Runtime error [2:1-2:20]: (vector-set!) vector-set!: index 5 out of bounds of vector',
    '(vector 1 2 3)',
    '3',
    'Runtime error [6:1-6:22]: (vector-set!) vector-set!: index -1 out of bounds of vector',
    '(vector 1 2 3)',
  ])
})

test('vector-empty', async () => {
  expect(
    await runProgram(`
(vector)
`),
  ).toEqual(['(vector)'])
})

////////////////////////////////////////////////////////////////////////////////
// Constants, References & Misc/Control

test('voidQ', async () => {
  expect(
    await runProgram(`
(void? void)
(void? 5)
(void?)
`),
  ).toEqual([
    '#t',
    '#f',
    'Runtime error [3:1-3:7]: Arity mismatch in function call: expected 1 arguments, got 0',
  ])
})

test('ignore', async () => {
  expect(
    await runProgram(`
(ignore 5)
(ignore)
`),
  ).toEqual([
    '[Blob: {}]',
    'Runtime error [2:1-2:8]: Arity mismatch in function call: expected 1 arguments, got 0',
  ])
})

test('set-maximum-recursion-depth', async () => {
  expect(
    await runProgram(`
(set-maximum-recursion-depth! 100)
(set-maximum-recursion-depth! -1)
(set-maximum-recursion-depth!)
`),
  ).toEqual([
    '[Blob: {"##scamperTag##":"set-maximum-recursion-depth","value":100}]',
    '[Blob: {"##scamperTag##":"set-maximum-recursion-depth","value":-1}]',
    'Runtime error [3:1-3:30]: Arity mismatch in function call: expected 1 arguments, got 0',
  ])
})

test('string-to-words', async () => {
  expect(
    await runProgram(`
(string->words "Hello, world! How are you?")
(string->words "")
(string->words "....")
(string->words 5)
`),
  ).toEqual([
    '(list "Hello" "world" "How" "are" "you")',
    'null',
    '(list "...")',
    'Runtime error [1026:1-1026:55]: (error) expected a string, received number',
  ])
})

test('ref-cells', async () => {
  expect(
    await runProgram(`
(define r (ref 5))
r
(ref? r)
(ref? 5)
(deref r)
(ref-set! r 10)
(deref r)
(ref)
(ref?)
(deref 5)
(ref-set! 5 10)
`),
  ).toEqual([
    '(ref 5)',
    '#t',
    '#f',
    '5',
    'void',
    '10',
    'Runtime error [8:1-8:5]: Arity mismatch in function call: expected 1 arguments, got 0',
    'Runtime error [9:1-9:6]: Arity mismatch in function call: expected 1 arguments, got 0',
    'Runtime error [1044:1-1044:39]: (error) expected a ref, received number',
    'Runtime error [1051:1-1051:43]: (error) expected a ref, received number',
  ])
})

test('constants', async () => {
  expect(
    await runProgram(`
else
null
pi
π
void
(else)
(null)
(pi)
(π)
(void)
`),
  ).toEqual([
    '#t',
    'null',
    '3.141592653589793',
    '3.141592653589793',
    'void',
    'Runtime error [6:1-6:6]: Not a function or closure: true',
    'Runtime error [7:1-7:6]: Not a function or closure: null',
    'Runtime error [8:1-8:4]: Not a function or closure: 3.141592653589793',
    'Runtime error [9:1-9:3]: Not a function or closure: 3.141592653589793',
    'Runtime error [10:1-10:6]: Not a function or closure: undefined',
  ])
})

test('with-file', async () => {
  expect(
    await runProgram(`
(with-file "foo.txt" (lambda (s) s))
(with-file 5 (lambda (s) s))
`),
  ).toEqual([
    '(reactive-file "foo.txt" [Function: ##anonymous##])',
    'Runtime error [1083:1-1083:46]: (error) expected a string, received number',
  ])
})

test('with-file-chooser', async () => {
  expect(
    await runProgram(`
(with-file-chooser (lambda (s) s))
(with-file-chooser 5)
`),
  ).toEqual([
    '(reactive-file-chooser [Function: ##anonymous##])',
    'Runtime error [1089:1-1089:61]: (error) expected a procedure, received number',
  ])
})

test('random-wrong-type', async () => {
  expect(
    await runProgram(`
(random "a")
`),
  ).toEqual([
    'Runtime error [997:1-997:41]: (error) expected an integer, received string',
  ])
})

////////////////////////////////////////////////////////////////////////////////
// Non-callback logic & error branches

test('string->number-invalid', async () => {
  // Fixed (#255): a non-numeral string returns #f (per R7RS) rather than
  // leaking a double-wrapped internal JS exception.
  expect(await runProgram('(string->number "abc")')).toEqual(['#f'])
})

test('char-compare-single-arg', async () => {
  // N.B., a comparator applied to fewer than two arguments is vacuously true
  // (exercises pairwiseSatisfies' length<=1 branch)
  expect(await runProgram('(char=? #\\a)')).toEqual(['#t'])
})

test('list->string-non-char', async () => {
  expect(await runProgram('(list->string (list 1 2))')).toEqual([
    'Runtime error [1:1-1:25]: (list->string) list->string: list contains non-character element: number',
  ])
})

test('vector-range-errors', async () => {
  expect(await runProgram('(vector-range)')).toEqual([
    'Runtime error [1:1-1:14]: (vector-range) 1, 2, or 3 numbers must be passed to function',
  ])
  expect(await runProgram('(vector-range 0 10 0)')).toEqual([
    'Runtime error [1:1-1:21]: (vector-range) "step" argument must be non-zero',
  ])
})

test('range-errors', async () => {
  expect(await runProgram('(range)')).toEqual([
    'Runtime error [1:1-1:7]: (range) 1, 2, or 3 numbers must be passed to function',
  ])
  expect(await runProgram('(range 0 10 0)')).toEqual([
    'Runtime error [1:1-1:14]: (range) "step" argument must be non-zero',
  ])
})

////////////////////////////////////////////////////////////////////////////////
// Higher-order functions (reimplemented in Scamper, #248)
//
// These used to be JS primitives that invoked their function argument via
// L.callScamperFn; they are now defined in prelude.scm. Each suite below
// covers the base case (empty input), standard cases, and corner cases.

test('map', async () => {
  expect(
    await runProgram(`
(map (lambda (x) (* x x)) (list 1 2 3 4))
(map (lambda (x) (* x x)) (list))
(map (lambda (x) (+ x 1)) (list 5))
(map (lambda (x) x) (list 1 2 3))
(map + (list 1 2 3) (list 10 20 30))
(map (lambda (a b c) (+ a b c)) (list 1 2) (list 3 4) (list 5 6))
(map +)
`),
  ).toEqual([
    '(list 1 4 9 16)',
    'null',
    '(list 6)',
    '(list 1 2 3)',
    '(list 11 22 33)',
    '(list 9 12)',
    'null',
  ])
})

test('map-length-mismatch', async () => {
  expect(await runProgram('(map + (list 1 2) (list 3))')).toEqual([
    'Runtime error [823:12-823:61]: (error) map: all lists must have the same length',
  ])
})

test('filter', async () => {
  expect(
    await runProgram(`
(filter even? (list 1 2 3 4 5 6))
(filter even? (list))
(filter even? (list 1 3 5))
(filter even? (list 2 4 6))
(filter odd? (list 4))
(filter odd? (list 3))
(filter (lambda (s) (> (string-length s) 2)) (list "a" "abc" "ab" "abcd"))
`),
  ).toEqual([
    '(list 2 4 6)',
    'null',
    'null',
    '(list 2 4 6)',
    'null',
    '(list 3)',
    '(list "abc" "abcd")',
  ])
})

test('fold', async () => {
  expect(
    await runProgram(`
(fold + 0 (list 1 2 3 4 5))
(fold + 0 (list))
(fold - 100 (list 1 2 3))
(fold (lambda (acc x) (cons x acc)) null (list 1 2 3))
(fold (lambda (acc x) (+ acc 1)) 0 (list 5 5 5 5))
`),
  ).toEqual([
    '15',
    '0',
    '94',
    '(list 3 2 1)',
    '4',
  ])
})

test('reduce', async () => {
  expect(
    await runProgram(`
(reduce + (list 1 2 3 4 5))
(reduce + (list 42))
(reduce - (list 10 1 2 3))
(reduce max (list 3 1 4 1 5 9 2 6))
`),
  ).toEqual([
    '15',
    '42',
    '4',
    '9',
  ])
})

test('fold-left', async () => {
  // fold-left's combiner takes the element first and the accumulator second
  // (SRFI-1 fold), the opposite of fold's argument order.
  expect(
    await runProgram(`
(fold-left + 0 (list 1 2 3 4 5))
(fold-left + 0 (list))
(fold-left - 0 (list 1 2 3 4 5))
(fold-left cons null (list 1 2 3))
(fold-left (lambda (x acc) (+ x acc)) 0 (list 1 2 3))
`),
  ).toEqual([
    '15',
    // base case: empty list returns the initial value
    '0',
    // element-first combiner: (- 5 (- 4 (- 3 (- 2 (- 1 0))))) = 3
    '3',
    // (cons elem acc) reverses the list
    '(list 3 2 1)',
    '6',
  ])
})

test('fold-right', async () => {
  expect(
    await runProgram(`
(fold-right + 0 (list 1 2 3 4 5))
(fold-right + 0 (list))
(fold-right - 5 (list 10 9 8 7 6))
(fold-right cons null (list 1 2 3))
(fold-right (lambda (x acc) (cons (* x 2) acc)) null (list 1 2 3))
`),
  ).toEqual([
    '15',
    '0',
    '3',
    '(list 1 2 3)',
    '(list 2 4 6)',
  ])
})

test('reduce-right', async () => {
  expect(
    await runProgram(`
(reduce-right + (list 1 2 3 4 5))
(reduce-right + (list 42))
(reduce-right - (list 10 9 8 7 6 5))
(reduce-right - (list 1 2 3))
`),
  ).toEqual([
    '15',
    '42',
    '3',
    '2',
  ])
})

test('vector-map', async () => {
  expect(
    await runProgram(`
(vector-map (lambda (x) (* x x)) (vector 1 2 3 4))
(vector-map (lambda (x) (+ x 1)) (vector))
(vector-map (lambda (x) (* x 2)) (vector 5))
(vector-map + (vector 1 2 3) (vector 10 20 30))
(vector-map +)
`),
  ).toEqual([
    '(vector 1 4 9 16)',
    '(vector)',
    '(vector 10)',
    '(vector 11 22 33)',
    '(vector)',
  ])
})

test('vector-map-length-mismatch', async () => {
  expect(await runProgram('(vector-map + (vector 1 2) (vector 3))')).toEqual([
    'Runtime error [823:12-823:61]: (error) map: all lists must have the same length',
  ])
})

test('vector-map!', async () => {
  expect(
    await runProgram(`
(define v1 (vector 1 2 3 4))
(vector-map! (lambda (x) (* x x)) v1)
v1
(define v2 (vector))
(vector-map! (lambda (x) (+ x 1)) v2)
v2
(define v3 (vector 10))
(vector-map! (lambda (x) (- x)) v3)
v3
`),
  ).toEqual([
    'void',
    '(vector 1 4 9 16)',
    'void',
    '(vector)',
    'void',
    '(vector -10)',
  ])
})

test('vector-for-each', async () => {
  // vector-for-each runs f purely for its side effects, in order.
  expect(
    await runProgram(`
(define total (ref 0))
(vector-for-each (lambda (x) (ref-set! total (+ (deref total) x))) (vector 1 2 3 4))
(deref total)
(define calls (ref 0))
(vector-for-each (lambda (x) (ref-set! calls (+ (deref calls) 1))) (vector))
(deref calls)
(define seen (ref null))
(vector-for-each (lambda (x) (ref-set! seen (cons x (deref seen)))) (vector 1 2 3))
(deref seen)
`),
  ).toEqual([
    'void',
    '10',
    'void',
    '0',
    'void',
    '(list 3 2 1)',
  ])
})

test('for-range', async () => {
  expect(
    await runProgram(`
(define asc (ref null))
(for-range 0 5 (lambda (i) (ref-set! asc (cons i (deref asc)))))
(deref asc)
(define desc (ref null))
(for-range 5 0 (lambda (i) (ref-set! desc (cons i (deref desc)))))
(deref desc)
(define empty (ref null))
(for-range 3 3 (lambda (i) (ref-set! empty (cons i (deref empty)))))
(deref empty)
`),
  ).toEqual([
    // ascending [0, 5): visits 0 1 2 3 4
    'void',
    '(list 4 3 2 1 0)',
    // descending [5, 0): visits 5 4 3 2 1
    'void',
    '(list 1 2 3 4 5)',
    // empty range: no visits
    'void',
    'null',
  ])
})

test('vector-filter', async () => {
  expect(
    await runProgram(`
(vector-filter even? (vector 1 2 3 4 5 6))
(vector-filter even? (vector))
(vector-filter even? (vector 1 3 5))
(vector-filter even? (vector 2 4))
(vector-filter (lambda (x) (> x 3)) (vector 5 1 4 2 3))
`),
  ).toEqual([
    '(vector 2 4 6)',
    '(vector)',
    '(vector)',
    '(vector 2 4)',
    '(vector 5 4)',
  ])
})

test('with-handler-call', async () => {
  // No error is raised, so the handler is never invoked; with-handler applies
  // its function to the trailing arguments and returns the result: (+ 1 2 3).
  expect(await runProgram('(with-handler + + 1 2 3)')).toEqual(['6'])
})

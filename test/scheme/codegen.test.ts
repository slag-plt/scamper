import { expect, test, describe } from 'vitest'

import * as S from '../../src/scheme'
import * as L from '../../src/lpm'
import { diagnosticToError } from '../../src/scheme/diagnostic'
import { Fiber } from '../../src/lpm/fiber'
import { runProgram } from '../harness.js'

// `stripRanges` drops the `[line:col]` location from error output, for the
// rare case whose error range points into the .scm library source (contract
// errors report the offending definition's line) and would otherwise shift
// whenever a library file is edited. User-source ranges are stable, so most
// callers leave it off and assert the range.
async function checkMachineOutput (src: string, expected: L.Value[], stripRanges = false) {
  src = src.trim()
  const out = new L.LoggingChannel(false, false)
  const { prog, diagnostics } = await S.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })
  expect(out.errLog).toEqual([])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const fiber = new Fiber(prog!, S.mkInitialEnv())
  // TODO: this should be refactored once we've re-established a common
  // entry point for running Scamper programs
  while (!fiber.isDone()) {
    try {
      const res = fiber.step()
      if (res.tag === 'display') {
        out.send(fiber.lastResult)
      }
    } catch (e) {
      if (e instanceof L.ScamperError) {
        out.log.push((stripRanges ? e.stripRange() : e).toString())
        fiber.advanceStmt()
      } else {
        throw e
      }
    }
  }
  expect(out.log).toEqual(expected)
}

describe('Basic codegen', () => {
  test('simple arithmetic', async () => {
    await checkMachineOutput(`
      (display (+ 1 1))
    `, [2])
  })
})

describe('End-to-end cases', () => {
  test('factorial', async () => {
    await checkMachineOutput(`
      (define fact
        (lambda (n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))))

      (display (fact 5))
    `, [120])
  })

  test('basic list operations', async () => {
    await checkMachineOutput(`
      (define list-length
        (lambda (l)
          (if (null? l)
              0
              (+ 1 (list-length (cdr l))))))
      (display (list-length '()))
    `, [0])
  })

  test('basic struct operations', async () => {
    await checkMachineOutput(`
      (struct point (x y))
      (define p (point 1 2))
      (display (point-x p))
    `, [1])
  })

  test('nullary functions', async () => {
    await checkMachineOutput(`
      (define f (lambda () 1))
      (f)
    `, [1])
  })

  test('and-or-short-circuit', async () => {
    await checkMachineOutput(`
(and (error "hello")
     #f)

(and #f
     (error "hello"))

(or (error "hello")
     #t)

(or #t
    (error "hello"))
`, [
      'Runtime error [1:6-1:20]: (error) hello',
      false,
      'Runtime error [7:5-7:19]: (error) hello',
      true,
    ])
  })

  test('chained-defs', async () => {
    await checkMachineOutput(`
(define x 10)
(define y x)
(define z y)

(+ z 50)

(define f +)
(define g f)
(define h g)
(define i h)

(i x y z)
`, [60, 30])
  })

  test('closures', async () => {
    await checkMachineOutput(`
(define x 10)

(define f1
  (lambda (y) (+ x y)))

(f1 20)

(define f2
  (let ([x 100])
    (lambda (y) (+ x y))))

(f2 20)

(define f3
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (+ x y z)))))

(((f3 11) 3) 7)

(define f4
  (let* ([x 51]
         [f (lambda (y) (+ x y))]
         [g (lambda (x) (+ (f x) 1))])
    g))

(f4 100)
`, [30, 120, 21, 152])
  })

  test('cond-else-test', async () => {
    await checkMachineOutput(`
(import image)

(define factorial
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (factorial (- n 1)))])))

(factorial 5)

(define red-square (rectangle 15 15 "solid" "red"))

(define type-of
  (lambda (datum)
    (cond
      [(number? datum) "number"]
      [(string? datum) "string"]
      [else "some-other-type"])))

(type-of red-square)

`, [120, 'some-other-type'])
  })

  test('contract-check', async () => {
    // N.B., the reported ranges point at string-length's/+'s own definitions
    // in prelude.scm rather than the call site -- a known limitation of
    // contract-wrapped errors (#254; see cons-pair, range, not-boolean in
    // prelude.test.ts). We assert with ranges stripped so this test stays
    // decoupled from prelude.scm's line numbers.
    await checkMachineOutput(`
(string-length (list 1 2 3))

(+ 1 2 3 "bye")
`, [
      'Runtime error: (error) expected a string, received list',
      // N.B., "+" is documented as a rest param (`. v1`), so its contract
      // check is a single all-satisfy? over the whole argument list rather
      // than a per-argument check -- it can report that *some* argument
      // failed, not *which one*.
      'Runtime error: (error) expected every value of v1 to be a number, but at least one was not',
    ], true)
  })

  test('define-test1', async () => {
    await checkMachineOutput(`
(define x 10)

(define f
  (lambda (y) (+ x y)))

(f x)
`, [20])
  })

  test('fact', async () => {
    await checkMachineOutput(`
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 0)

(fact 5)
`, [1, 120])
  })

  test('fizzbuzz', async () => {
    await checkMachineOutput(`
(define fizzbuzz
  (lambda (n)
    (cond
      [(and (zero? (modulo n 3)) (zero? (modulo n 5))) "fizzbuzz"]
      [(zero? (modulo n 3)) "fizz"]
      [(zero? (modulo n 5)) "buzz"]
      [#t (number->string n)])))

(fizzbuzz 1)
(fizzbuzz 2)
(fizzbuzz 3)
(fizzbuzz 4)
(fizzbuzz 5)
(fizzbuzz 6)
(fizzbuzz 7)
(fizzbuzz 8)
(fizzbuzz 9)
(fizzbuzz 10)
(fizzbuzz 11)
(fizzbuzz 12)
(fizzbuzz 13)
(fizzbuzz 14)
(fizzbuzz 15)
`, [
      '1',
      '2',
      'fizz',
      '4',
      'buzz',
      'fizz',
      '7',
      '8',
      'fizz',
      'buzz',
      '11',
      'fizz',
      '13',
      '14',
      'fizzbuzz',
    ])
  })

  test('let-binding', async () => {
    await checkMachineOutput(`
; bindings are not dependent on each other
(let
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

(let*
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

; bindings telescope
(let*
  ([x 1]
   [y (+ x 6)]
   [z (+ y 4)])
  (+ x y z))
`, [19, 19, 19])
  })

  test('let-binding with patterns', async () => {
    await checkMachineOutput(`
; a constructor pattern destructures the bound value
(let ([(pair a b) (pair 1 2)])
  (+ a b))

; a wildcard binding runs the value for effect, then returns the body
(let ([_ 99]) 42)

; patterns work in let* too, and still telescope
(let* ([(pair a b) (pair 3 4)]
       [c (+ a b)])
  c)
`, [3, 42, 7])
  })

  test('let-binding: a value not matching the pattern is a runtime error', async () => {
    const out = await runProgram('(let ([(pair a b) 5]) a)')
    expect(out.length).toBe(1)
    expect(out[0]).toContain('let: value did not match pattern (pair a b)')
  })

  test('list-length', async () => {
    await checkMachineOutput(`
(define list-length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l))))))

(list-length null)

(list-length (cons 9 null))

(list-length (cons 9 (cons 9 (cons 9 (cons 9 (cons 9 null))))))

(list-length (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" null))))))
`, [0, 1, 5, 5])
  })

  test('match-lists', async () => {
    await checkMachineOutput(`
(define list-length
  (lambda (l)
    (match l
      [null 0]
      [(cons _ tail) (+ 1 (list-length tail))])))

(list-length (list 0 0 0 0 0))

(list-length null)

(list-length (list 0 0 0 0 0 0 0 0 0 0))

(define list-append
  (lambda (l1 l2)
    (match l1
      [null l2]
      [(cons head tail) (cons head (list-append tail l2))])))

(list-append (list 1 2 3) (list 4 5 6))

(define intersperse
  (lambda (x l)
    (match l
      [null null]
      [(cons _ null) l]
      [(cons x1 (cons x2 tail)) (cons x1 (cons x (intersperse x (cons x2 tail))))])))

(intersperse "," (list "a" "b" "c"))
`, [
      5,
      0,
      10,
      L.mkList(1, 2, 3, 4, 5, 6),
      L.mkList('a', ',', 'b', ',', 'c'),
    ])
  })

  test('match-lit', async () => {
    await checkMachineOutput(`
(match 5
  [1 "fail"]
  [5 "numbers"])

(match "baz"
  ["foo" "fail"]
  ["bar" "fail"]
  ["baz" "strings"]
  ["boop" "fail"])

(match #\\q
  [#\\a "fail"]
  [#\\q "chars"]
  [#\\z "fail"])

(match #t
  [#f "fail"]
  [#t "bools"])

(match null
  [null "null"])

(match (list "lists" "a" "b")
  [null "fail"]
  [(cons head _) head])
`, ['numbers', 'strings', 'chars', 'bools', 'null', 'lists'])
  })

  test('match-struct', async () => {
    await checkMachineOutput(`
(struct leaf (value))

(struct node (left right))

(define tree-count
  (lambda (t)
    (match t
      [(leaf _) 1]
      [(node l r) (+ (tree-count l) (tree-count r))])))

(tree-count (leaf "a"))

(tree-count
  (node (leaf "a")
        (node (leaf "b")
              (node (leaf "c")
                    (leaf "d")))))
`, [1, 4])
  })

  test('mixed-brackets', async () => {
    await checkMachineOutput(`
{- {* 3
     (+ {* 1
           { / 5 8}}
         12)}
   (- 5 1)}
`, [33.875])
  })

  test('numbers', async () => {
    await checkMachineOutput(`
4129
0
-48902
+48902
00142
-089
+098

3.14
.14
0.14
314.

-3.14
-.14
-0.14
-314.

+3.14
+.14
+0.14
+314.

3e2
3.0e2
.3e2
3E2
3.0E2
.3E2

-3e2
-.3e2
-3e-2
-.3e-2
`, [
      4129,
      0,
      -48902,
      48902,
      142,
      -89,
      98,
      3.14,
      0.14,
      0.14,
      314,
      -3.14,
      -0.14,
      -0.14,
      -314,
      3.14,
      0.14,
      0.14,
      314,
      300,
      300,
      30,
      300,
      300,
      30,
      -300,
      -30,
      -0.03,
      -0.003,
    ])
  })

  test('simple-exp', async () => {
    await checkMachineOutput(`
(let ([x 1] [y (+ 1 1)]) (+ (- 1 1) y (* x 5 8) x))

(+ (car (cdr (cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null)))))))) 100)
`, [43, 103])
  })

  test('tree-test', async () => {
    const leaf = (v: L.Value) => L.mkStruct('leaf', ['value'], [v])
    const node = (l: L.Value, r: L.Value) => L.mkStruct('node', ['left', 'right'], [l, r])
    await checkMachineOutput(`
(struct leaf (value))

(struct node (left right))

(define tree-size
  (lambda (t)
    (if (leaf? t)
        1
        (+ (tree-size (node-left t))
           (tree-size (node-right t))))))

(define tree-to-list
  (lambda (t)
    (if (leaf? t)
        (list (leaf-value t))
        (append (tree-to-list (node-left t))
                (tree-to-list (node-right t))))))

(define t1
  (node (leaf "a")
        (node (leaf "b")
              (leaf "c"))))

t1

(leaf-value (node-left (node-right t1)))

(tree-size t1)

(tree-to-list t1)
`, [
      node(leaf('a'), node(leaf('b'), leaf('c'))),
      'b',
      3,
      L.mkList('a', 'b', 'c'),
    ])
  })

  test('section desugars to a lambda and applies (simple holes)', async () => {
    await checkMachineOutput(`
((section + _ 1) 1)
((section + 1 _) 10)
((section - 10 _) 3)
((section * _ _) 5 6)
`, [2, 11, 7, 30])
  })

  // Still blocked: `map` here is a JS library function receiving a Scamper
  // closure `(section string-upcase _)`; invoking that closure from JS routes
  // through L.callScamperFn, which is permanently disabled ("Javascript library
  // functions can no longer call Scamper functions"). Re-enable once the LPM
  // unification lets library higher-order functions drive Scamper closures.
  test.skip('section as an argument to a JS higher-order function (map)', async () => {
    expect(
      await runProgram(`
(|> (list "a" "b" "c" "d" "e")
    (section map (section string-upcase _) _))
`),
    ).toEqual(['(list "A" "B" "C" "D" "E")'])
  })
})

describe('Rest parameters', () => {
  // Coverage for rest parameters: (lambda (x y & z) ...) binds the trailing
  // arguments beyond the fixed parameters as a proper list bound to z.

  test('rest param collects extra arguments into a list', async () => {
    expect(await runProgram(`
    (display ((lambda (x & y) y) 1 2 3))
    `)).toEqual(['(list 2 3)'])
  })

  test('rest param is an empty list when no extra arguments are given', async () => {
    expect(await runProgram(`
    (display ((lambda (x & y) y) 1))
    `)).toEqual(['null'])
  })

  test('fixed parameter still binds correctly alongside a rest parameter', async () => {
    expect(await runProgram(`
    (display ((lambda (x & y) x) 1 2 3))
    `)).toEqual(['1'])
  })

  test('multiple fixed parameters combine with one rest parameter', async () => {
    expect(await runProgram(`
    (display ((lambda (a b & rest) (list a b rest)) 1 2 3 4 5))
    `)).toEqual(['(list 1 2 (list 3 4 5))'])
  })

  test('same rest-param lambda works across calls with varying argument counts', async () => {
    expect(await runProgram(`
    (define f (lambda (x & y) y))
    (display (f 1))
    (display (f 1 2))
    (display (f 1 2 3))
    `)).toEqual(['null', '(list 2)', '(list 2 3)'])
  })

  test('rest parameter is a real list usable with car/length', async () => {
    expect(await runProgram(`
    (define f (lambda (x & rest) (list (car rest) (length rest))))
    (display (f 1 2 3 4))
    `)).toEqual(['(list 2 3)'])
  })

  test('empty rest parameter is recognized by null?/list?', async () => {
    expect(await runProgram(`
    (define f (lambda (x & rest) (list (null? rest) (list? rest))))
    (display (f 1))
    `)).toEqual(['(list #t #t)'])
  })

  test('rest parameter works with recursion over the resulting list', async () => {
    expect(await runProgram(`
    (define sum-list
      (lambda (lst)
        (if (null? lst)
            0
            (+ (car lst) (sum-list (cdr lst))))))
    (define my-sum
      (lambda (x & rest)
        (+ x (sum-list rest))))
    (display (my-sum 1 2 3 4 5))
    `)).toEqual(['15'])
  })

  test('rest parameter is captured correctly by a closure returned from a lambda', async () => {
    expect(await runProgram(`
    (define make-adder
      (lambda (x & rest)
        (lambda (y) (+ x y (length rest)))))
    (display ((make-adder 1 2 3) 10))
    `)).toEqual(['13'])
  })

  test('regression: a fixed-arity lambda (no rest param) still works with exact args', async () => {
    expect(await runProgram(`
    (display ((lambda (x y) (+ x y)) 3 4))
    `)).toEqual(['7'])
  })

  test('arity mismatch: fewer than the required fixed args, even with a rest param present', async () => {
    expect(await runProgram(`
    ((lambda (x y & z) z) 1)
    `)).toEqual([
      'Runtime error [1:1-1:24]: Arity mismatch in function call: expected 2 arguments, got 1',
    ])
  })

  test('arity mismatch: zero args against a lambda requiring one fixed arg plus a rest param', async () => {
    expect(await runProgram(`
    ((lambda (x & y) y))
    `)).toEqual([
      'Runtime error [1:1-1:20]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })

  test('regression: arity mismatch on extra args still fires when there is no rest param', async () => {
    expect(await runProgram(`
    ((lambda (x y) (+ x y)) 1 2 3)
    `)).toEqual([
      'Runtime error [1:1-1:30]: Arity mismatch in function call: expected 2 arguments, got 3',
    ])
  })

  test('malformed: rest parameter missing its trailing identifier', async () => {
    expect(await runProgram(`
    (lambda (x &) x)
    `)).toEqual([
      'Parser error [1:1-1:16]: Malformed lambda expression (a list of parameters and a body).',
    ])
  })

  test('malformed: more than one identifier following the rest marker', async () => {
    expect(await runProgram(`
    (lambda (x & y z) x)
    `)).toEqual([
      'Parser error [1:1-1:20]: Malformed lambda expression (a list of parameters and a body).',
    ])
  })

  test('rest-only parameter list (no fixed parameters) collects all arguments (#272)', async () => {
    expect(await runProgram(`
    (display ((lambda (& xs) xs) 1 2 3))
    (display ((lambda (& xs) xs)))
    `)).toEqual(['(list 1 2 3)', 'null'])
  })
})

describe('Construct semantics (comprehensiveness audit)', () => {
  describe('begin', () => {
    test('a single-expression begin yields that expression', async () => {
      await checkMachineOutput('(begin 42)', [42])
    })

    test('a multi-expression begin yields its last expression', async () => {
      await checkMachineOutput('(begin 1 2 3)', [3])
    })

    test('begin evaluates earlier expressions (an error in the first propagates)', async () => {
      await checkMachineOutput('(begin (error "boom") 99)',
        ['Runtime error: (error) boom'], true)
    })
  })

  describe('match', () => {
    test('a match with no matching branch is an inexhaustive-match error', async () => {
      await checkMachineOutput('(match 5 [6 "six"])',
        ['Runtime error: Inexhaustive pattern match failure'], true)
    })

    test('plit matches a char literal', async () => {
      await checkMachineOutput('(match #\\a [#\\a "yes"] [_ "no"])', ['yes'])
    })

    test('plit matches a string literal', async () => {
      await checkMachineOutput('(match "hi" ["hi" 1] [_ 2])', [1])
    })

    test('plit matches a boolean literal', async () => {
      await checkMachineOutput('(match #f [#f 1] [_ 2])', [1])
    })

    test('nested constructor patterns bind sub-pattern variables', async () => {
      await checkMachineOutput(`
        (struct node (l r))
        (match (node (node 1 2) 3)
          [(node (node a b) c) (+ (+ a b) c)])
      `, [6])
    })

    test('a repeated pattern variable takes the last binding (no repeat check at runtime)', async () => {
      await checkMachineOutput('(match (pair 1 1) [(pair a a) a])', [1])
    })

    test('overlapping branches take the first match', async () => {
      await checkMachineOutput('(match 5 [x "first"] [_ "second"])', ['first'])
    })
  })

  describe('quote', () => {
    test('nested quoted lists', async () => {
      expect(await runProgram("'(1 (2 (3)))")).toEqual([
        '(list 1 (list 2 (list 3)))',
      ])
    })

    test('mixed literal types in a quoted list', async () => {
      expect(await runProgram('\'(1 "two" #\\c #t)')).toEqual([
        '(list 1 "two" #\\c #t)',
      ])
    })

    test('the empty quoted list is null', async () => {
      expect(await runProgram("'()")).toEqual(['null'])
    })

    test('a quoted symbol', async () => {
      expect(await runProgram("'foo")).toEqual(['foo'])
    })
  })

  describe('struct', () => {
    test('a single-field struct round-trips through its accessor', async () => {
      await checkMachineOutput('(struct box (v)) (box-v (box 42))', [42])
    })

    test('a struct with many fields exposes each accessor', async () => {
      await checkMachineOutput(
        '(struct hex (a b c d e f)) (hex-f (hex 1 2 3 4 5 6))', [6])
    })

    test('a zero-field struct constructs and its predicate holds', async () => {
      await checkMachineOutput('(struct unit ()) (unit? (unit))', [true])
    })

    test('a predicate distinguishes its own struct from other values', async () => {
      await checkMachineOutput(
        '(struct pt (x y)) (pt? (pt 1 2)) (pt? 5)', [true, false])
    })

    test('an accessor applied to the wrong struct is a runtime error', async () => {
      await checkMachineOutput(
        '(struct a (x)) (struct b (y)) (a-x (b 1))',
        ['Runtime error: Accessor function expects a a, received [Struct: b]'], true)
    })
  })

  describe('apply', () => {
    test('apply spreads a list as the call arguments', async () => {
      await checkMachineOutput('(apply + (list 1 2 3))', [6])
    })

    test('apply with an empty argument list', async () => {
      await checkMachineOutput('(apply + (list))', [0])
    })

    test('apply with a non-list argument is a runtime error', async () => {
      await checkMachineOutput('(apply + 5)',
        ['Runtime error: (apply) expected a list, received number'], true)
    })

    // apply is now an ordinary first-class procedure (not a special form): it
    // can be tested with procedure?, sectioned, and passed to higher-order fns.
    test('apply is a first-class value (procedure?, section, passed to a HOF)', async () => {
      await checkMachineOutput(`
        (procedure? apply)
        ((section apply _ _) + (list 1 2))
        (map (lambda (p) (apply + p)) (list (list 1 2) (list 3 4)))
      `, [true, 3, L.mkList(3, 7)])
    })
  })

  describe('js-var', () => {
    test('a js-var resolves to its JS internal, callable as a function', async () => {
      await checkMachineOutput('((js-var "prelude_numberQ") 5)', [true])
    })
  })

  describe('define', () => {
    test('a later top-level define shadows the earlier binding', async () => {
      await checkMachineOutput('(define x 1) (define x 2) x', [2])
    })

    test('a function body can forward-reference a later top-level define', async () => {
      await checkMachineOutput(`
        (define f (lambda () (g)))
        (define g (lambda () 42))
        (f)
      `, [42])
    })
  })

  describe('import', () => {
    test('importing an unknown builtin library is a runtime error', async () => {
      await checkMachineOutput('(import no-such-lib)',
        ['Runtime error: No such built-in library: no-such-lib'], true)
    })
  })
})
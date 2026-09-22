import { describe, expect, test } from 'vitest'
import { reductionTrace } from '../harness.js'

// Regression for #569: codegen names every lambda `##anonymous##`, and that
// placeholder escaped into the raised expression -- from an unevaluated `cls`
// op and from a closure already sitting on a value stack. `visibleReduction`
// drops any step whose text contains `##`, so the placeholder did not merely
// print badly: it deleted the whole step. A trace that began with a lambda had
// no starting point at all, and one that ended at a closure showed
// `[Function: ##anonymous##]`.
describe('a lambda takes visible trace steps (#569)', () => {
  test("the issue's own program reduces its arguments first", async () => {
    expect(
      await reductionTrace(
        '((lambda (a b) (+ (* 2 a) (* 3 b))) (+ 1 2) (+ 3 (* 4 5)))',
      ),
    ).toEqual([
      '((lambda (a b) (+ (* 2 a) (* 3 b))) (+ 1 2) (+ 3 (* 4 5)))',
      '((lambda (a b) (+ (* 2 a) (* 3 b))) 3 (+ 3 (* 4 5)))',
      '((lambda (a b) (+ (* 2 a) (* 3 b))) 3 (+ 3 20))',
      '((lambda (a b) (+ (* 2 a) (* 3 b))) 3 23)',
      '(+ (* 2 3) (* 3 23))',
      '(+ 6 (* 3 23))',
      '(+ 6 69)',
      '75',
    ])
  })

  test('defining a function takes a step rather than none at all', async () => {
    expect(await reductionTrace('(define f (lambda (x) x))')).toEqual([
      '(lambda (x) x)',
    ])
  })

  test('a lambda passed to a library function gives the trace its opening', async () => {
    expect(
      await reductionTrace('(define n 3)\n(map (lambda (x) (+ x n)) (list 1 2))'),
    ).toEqual(['3', '(map (lambda (x) (+ x 3)) (list 1 2))', '(list 4 5)'])
  })

  test('a returned closure shows as its lambda, not as [Function: ...]', async () => {
    expect(
      await reductionTrace(
        '(define mk (lambda (n) (lambda (x) (+ x n))))\n(mk 5)',
      ),
    ).toEqual([
      '(lambda (n) (lambda (x) (+ x n)))',
      '(mk 5)',
      '(lambda (x) (+ x 5))',
    ])
  })

  test('a closure keeps the #(...) it was written as', async () => {
    // What Closure.provenance is for. The opening step raises the `cls` op,
    // the next raises the closure it produced; without the provenance riding
    // across, the same function would spell itself two different ways.
    expect(await reductionTrace('(#(* % 2) (+ 1 2))')).toEqual([
      '(#(* %1 2) (+ 1 2))',
      '(#(* %1 2) 3)',
      '(* 3 2)',
      '6',
    ])
  })

  test('a closure spells its free names the same way at every step', async () => {
    // A statement's final value is raised with no frame to take an environment
    // from, so it is handed the statement's own. Without it `k` substitutes to
    // `10` mid-trace and reverts to `k` at the end -- one state, two spellings.
    expect(
      await reductionTrace(
        '(define k 10)\n(define mk (lambda () (lambda () k)))\n(mk)',
      ),
    ).toEqual(['10', '(lambda () (lambda () 10))', '(mk)', '(lambda () 10)'])
  })

  test('a rest parameter survives the round trip', async () => {
    expect(
      await reductionTrace('(define h (lambda (x & rest) x))\n(h 1 2 3)'),
    ).toEqual(['(lambda (x & rest) x)', '(h 1 2 3)', '1'])
  })
})

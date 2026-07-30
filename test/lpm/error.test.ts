import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// End-to-end coverage of runtime error raising (`error`) and catching (the
// `with-handler` special form, backed by the LPM in-fiber handler stack). These
// exercise the stack discipline: unwinding to the installing frame, restoring the
// value stack, and resuming -- including handlers nested within a single frame,
// across frames, and installed mid-computation.

describe('with-handler basics', () => {
  test('catches a raised error and returns the handler result', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "caught") (lambda () (error "boom")))
    `)).toEqual(['"caught"'])
  })

  test('returns the guarded result verbatim when no error is raised', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "caught") (lambda () 42))
    `)).toEqual(['42'])
  })

  test('passes the error message string to the handler', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) e) (lambda () (error "boom")))
    `)).toEqual(['"boom"'])
  })

  test('applies the guarded function to the trailing arguments', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) 0) + 1 2 3)
    `)).toEqual(['6'])
  })

  test('the form\'s value flows into the surrounding expression', async () => {
    expect(await runProgram(`
    (+ 100 (with-handler (lambda (e) 5) (lambda () (error "x"))))
    `)).toEqual(['105'])
  })

  test('restores the value stack around the form (surrounding args preserved)', async () => {
    expect(await runProgram(`
    (+ 1 (with-handler (lambda (e) 10) (lambda () (error "x"))) 100)
    `)).toEqual(['111'])
  })

  test('works in tail position', async () => {
    expect(await runProgram(`
    (define f (lambda () (with-handler (lambda (e) "caught") (lambda () (error "x")))))
    (f)
    `)).toEqual(['"caught"'])
  })
})

describe('which errors are caught', () => {
  test('catches a contract/type violation', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "caught-type") (lambda () (car 5)))
    `)).toEqual(['"caught-type"'])
  })

  test('catches an arity mismatch', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "caught-arity") (lambda () ((lambda (x) x))))
    `)).toEqual(['"caught-arity"'])
  })
})

describe('nested handlers within a single frame', () => {
  test('an inner guarded argument catches its own error; the outer application proceeds', async () => {
    // outer f is `+`; its second argument is itself a with-handler that throws.
    expect(await runProgram(`
    (with-handler (lambda (e) "outer-caught")
      +
      1
      (with-handler (lambda (e) 99) (lambda () (error "x"))))
    `)).toEqual(['100'])
  })

  test('two sequential handlers in one frame each catch independently', async () => {
    expect(await runProgram(`
    (+ (with-handler (lambda (e) 1) (lambda () (error "x")))
       (with-handler (lambda (e) 2) (lambda () (error "y"))))
    `)).toEqual(['3'])
  })

  test('a normal-completion and an error-completion handler coexist in one frame', async () => {
    expect(await runProgram(`
    (+ (with-handler (lambda (e) 0) (lambda () 10))
       (with-handler (lambda (e) 5) (lambda () (error "x"))))
    `)).toEqual(['15'])
  })
})

describe('nested handlers across frames', () => {
  test('the innermost handler catches; the outer handler is untouched', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) (string-append "outer:" e))
      (lambda ()
        (with-handler (lambda (e) (string-append "inner:" e))
          (lambda () (error "boom")))))
    `)).toEqual(['"inner:boom"'])
  })

  test('an inner handler that completed normally is uninstalled (outer catches a later error)', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) (string-append "outer:" e))
      (lambda ()
        (begin
          (with-handler (lambda (e) "inner-ran") (lambda () 1))
          (error "outer-boom"))))
    `)).toEqual(['"outer:outer-boom"'])
  })

  test('three levels deep, the innermost catches', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "L1")
      (lambda ()
        (with-handler (lambda (e) "L2")
          (lambda ()
            (with-handler (lambda (e) "L3")
              (lambda () (error "deep")))))))
    `)).toEqual(['"L3"'])
  })
})

describe('handlers installed mid-computation (stack discipline)', () => {
  test('unwinds a deep guarded call stack back to the handler and resumes', async () => {
    // sof recurses 5 frames deep before raising; the handler must unwind all of
    // them, restore the stack, and let the outer (+ 1000 _) resume.
    expect(await runProgram(`
    (define sof
      (lambda (n)
        (if (equal? n 0)
            (error "hit-zero")
            (+ n (sof (- n 1))))))
    (+ 1000 (with-handler (lambda (e) 0) (lambda () (sof 5))))
    `)).toEqual(['1000'])
  })

  test('subsequent statements evaluate normally after a caught error', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) "caught") (lambda () (error "x")))
    (+ 1 2)
    `)).toEqual(['"caught"', '3'])
  })
})

describe('handler errors and unguarded errors', () => {
  test('an error raised by a handler propagates to the enclosing handler', async () => {
    expect(await runProgram(`
    (with-handler (lambda (e) (string-append "outer:" e))
      (lambda ()
        (with-handler (lambda (e) (error "handler-failed"))
          (lambda () (error "inner-boom")))))
    `)).toEqual(['"outer:handler-failed"'])
  })

  test('an error raised by a handler with no enclosing handler is surfaced', async () => {
    const out = await runProgram(`
    (with-handler (lambda (e) (error "handler-failed")) (lambda () (error "boom")))
    `)
    expect(out).toHaveLength(1)
    expect(out[0]).toContain('handler-failed')
  })

  test('an error outside any handler is surfaced and does not consume a stale handler', async () => {
    // The handler installed by the first statement must be fully uninstalled, so
    // the second statement's error is reported (not swallowed) and the third runs.
    const out = await runProgram(`
    (with-handler (lambda (e) "caught") (lambda () (error "x")))
    (error "unguarded")
    (+ 1 2)
    `)
    expect(out).toHaveLength(3)
    expect(out[0]).toBe('"caught"')
    expect(out[1]).toContain('unguarded')
    expect(out[2]).toBe('3')
  })
})

import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import builtinLibs from '../../src/lib/index.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge.js'
import type { ScamperDiagnostic } from '../../src/scheme/diagnostic.js'

// Derived forms lower into applications of names expansion injects by
// reference -- `[...]` becomes `(##mkVec## ...)`, `struct` becomes
// `(##mkCtorFn## ...)`, and so on. Those names resolve against the *user's*
// top-level environment, so a user binding with the same name silently
// rewrote what the form meant (#336). The `##...##` shape is now reserved: no
// program may bind one, so no form's meaning depends on its surroundings.
describe('internal `##...##` names cannot be bound', () => {
  const reserved = (name: string) =>
    expect.stringContaining(
      `The identifier "${name}" is reserved for Scamper's internal use`,
    )

  test('the reported repros are rejected at parse time', async () => {
    // Previously each of these compiled, then failed at runtime with "Not a
    // function or closure: 5" from a form the user never touched.
    expect(await runProgram('(define ##mkVec## 5)\n[1 2]')).toEqual([
      reserved('##mkVec##'),
    ])
    expect(await runProgram('(define ##mkObj## 5)\n{"a" 1}')).toEqual([
      reserved('##mkObj##'),
    ])
    expect(
      await runProgram('(define ##mkCtorFn## 5)\n(struct p (x))\n(p 1)'),
    ).toEqual([reserved('##mkCtorFn##')])
  })

  test('every binder position rejects an internal name', async () => {
    for (const [src, name] of [
      ['(define ##mkVec## 5)', '##mkVec##'],
      ['(define-export ##mkVec## 5)', '##mkVec##'],
      ['(lambda (##mkVec##) 1)', '##mkVec##'],
      ['(lambda (x & ##mkVec##) 1)', '##mkVec##'],
      ['(let ([##mkVec## 5]) [1 2])', '##mkVec##'],
      ['(match 1 [##mkVec## 2])', '##mkVec##'],
      ['(struct ##foo## (x))', '##foo##'],
      ['(struct p (##bar##))', '##bar##'],
    ] as const) {
      expect(await runProgram(src), src).toContainEqual(reserved(name))
    }
  })

  test('the reserved shape is `##...##`, not any `#`-ish name', async () => {
    // Ordinary names that merely brush the convention stay bindable -- the
    // rule must not quietly outlaw plausible student identifiers.
    for (const name of ['x##', '##x', '####', 'a##b##']) {
      expect(await runProgram(`(define ${name} 5)\n${name}`), name).toEqual([
        '5',
      ])
    }
  })

  test('referring to an internal name is still allowed', async () => {
    // Only *binding* is reserved, exactly as with the `%` parameters of
    // `#(...)`. Sugaring already round-trips a hand-written `(##mkObj## ...)`
    // (test/scheme/sugar.test.ts), so references must keep parsing.
    expect(await runProgram('(##mkVec## 1 2)')).toEqual(['(vector 1 2)'])
  })
})

// Reserving the `##...##` shape only helps names that already live in it.
// `cond`'s fall-through and the contract checks injected the *ordinary* name
// `error`, which a student may plausibly bind -- so they now inject the
// internal `##error##` instead.
describe('the injected error primitive cannot be shadowed', () => {
  test('a cond fall-through raises even when `error` is bound', async () => {
    // Regression: this reported "Not a function or closure: 5" from a form the
    // user never touched.
    expect(await runProgram('(define error 5)\n(cond [#f 1])')).toEqual([
      'Runtime error [2:1-2:13]: (error) No matching clause in cond',
    ])
    expect(await runProgram('(let ([error 5]) (cond [#f 1]))')).toEqual([
      'Runtime error [1:18-1:30]: (error) No matching clause in cond',
    ])
  })

  test('a contract violation still raises when `error` is bound', async () => {
    expect(await runProgram('(define error 5)\n(list-ref 1 2)')).toEqual([
      'Runtime error [2:1-2:14]: (error) expected a list, received number',
    ])
    expect(await runProgram('(define error 5)\n(+ 1 "a")')).toEqual([
      'Runtime error [2:1-2:9]: (error) expected every value of v1 to be a number, but at least one was not',
    ])
  })

  test('the raise still reports itself as `error`, not `##error##`', async () => {
    // ##error## fixes its ScamperError source to "error", so the internal
    // spelling never surfaces to a student.
    expect(await runProgram('(cond [#f 1])')).toEqual([
      'Runtime error [1:1-1:13]: (error) No matching clause in cond',
    ])
  })

  test('`error` is still an ordinary, bindable prelude name', async () => {
    // Only the *injected* use is reserved; the user-facing name is untouched.
    expect(await runProgram('(define error 5)\nerror')).toEqual(['5'])
    expect(await runProgram('(error "boom")')).toEqual([
      'Runtime error [1:1-1:14]: (error) boom',
    ])
  })
})

// The exemption: runtime.scm is the interop layer that *defines* the
// primitives expansion injects, so it alone may bind the shape.
describe('the runtime library keeps its internal bindings', () => {
  test('runtime exports the internal primitives', () => {
    for (const name of [
      '##mkVec##',
      '##mkObj##',
      '##mkCtorFn##',
      '##mkPredFn##',
      '##mkGetFn##',
      '##typeOf##',
      '##report##',
      '##error##',
    ]) {
      expect([...(builtinLibs.get('runtime')?.bindings.keys() ?? [])]).toContain(
        name,
      )
    }
  })

  test('the exemption is a parse option, off by default', () => {
    // src/lib/index.ts parses runtime.scm twice -- once to load it, once for
    // the doc registry -- so both paths have to pass the option.
    const parse = (allowInternalNames: boolean) => {
      const diagnostics: ScamperDiagnostic[] = []
      parseProgramFromSource(diagnostics, '(define-export ##mkVec## 5)', {
        allowInternalNames,
      })
      return diagnostics
    }
    expect(parse(true)).toEqual([])
    expect(parse(false)).toHaveLength(1)
  })

  test('the forms those primitives back still work', async () => {
    expect(await runProgram('[1 2]')).toEqual(['(vector 1 2)'])
    expect(await runProgram('{"a" 1}')).toEqual(['{ "a" : 1 }'])
    expect(await runProgram('(struct p (x))\n(p-x (p 1))')).toEqual(['1'])
  })
})

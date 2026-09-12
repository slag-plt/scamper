import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import builtinLibs from '../../src/lib/index.js'
import { compile } from '../../src/scheme/index.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge.js'
import type { ScamperDiagnostic } from '../../src/scheme/diagnostic.js'

// Derived forms lower into applications of names expansion injects by
// reference -- `[...]` becomes `(##mkVec## ...)`, `struct` becomes
// `(##mkCtorFn## ...)`, and so on. Those names resolve against the *user's*
// top-level environment, so a user binding with the same name silently
// rewrote what the form meant (#336). The `##...##` shape is now reserved: no
// program may bind one, so no form's meaning depends on its surroundings. The
// reference direction is the block below (#532).
describe('internal `##...##` names cannot be bound', () => {
  const reserved = (name: string): unknown =>
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
})

// #532: the reservation was only ever read in *binder* positions, so a program
// could still name an internal. `mkInitialEnv` imports the whole `runtime`
// library into a user program's environment, and the scope checker seeds its
// globals from the same module, so `(##report## 5)` passed `--check` cleanly and
// then ran the query machinery's abort primitive. Nothing documents these names
// and nothing in the library or the samples writes one; every reference
// expansion, contract insertion, and the query/example paths inject is built as
// an AST node and never goes through the reader, so denying the shape in source
// costs them nothing.
describe('internal `##...##` names cannot be referenced either (#532)', () => {
  /**
   * What `--check` (compile plus the optional scope-check pass) reports for
   * `src`. Deliberately only asked *whether* it complains and about which name:
   * the reference can be denied by the reader or by the scope checker, and this
   * suite is about the hole, not about which pass closes it.
   */
  const checkMessages = async (src: string): Promise<string[]> => {
    const { diagnostics } = await compile(src, { scopeCheck: true })
    return diagnostics.map((d) => d.message)
  }

  // Everything runtime.scm exports under the reserved shape -- the whole
  // surface mkInitialEnv put within a program's reach.
  const internals = [
    '##mkVec##',
    '##mkObj##',
    '##mkCtorFn##',
    '##mkPredFn##',
    '##mkGetFn##',
    '##typeOf##',
    '##error##',
    '##report##',
    '##optArg##',
    '##optRest##',
    '##checkArity##',
    '##voidQ##',
    '##contracted##',
  ]

  test('the reported repro is rejected, and never reports', async () => {
    expect(await checkMessages('(##report## 5)')).not.toEqual([])
    // ##report## stops the fiber and hands its argument back as the answer to
    // an IDE query. A student's program must not be able to ask for that.
    expect(await runProgram('(##report## 5)')).not.toContainEqual(
      expect.stringContaining('Reported value'),
    )
    expect(
      await runProgram('(struct point (x y))\n(##report## (point 1 2))'),
    ).not.toContainEqual(expect.stringContaining('Reported value'))
  })

  test('every internal the runtime exports is out of reach', async () => {
    for (const name of internals) {
      expect((await checkMessages(`(${name} 1)`)).join('\n'), name).toContain(
        name,
      )
    }
  })

  test('an explicit `(import runtime)` does not let one back in', async () => {
    // runtime is a real builtin module, so importing it re-extends the
    // environment with exactly these bindings -- qualified or not. Keeping the
    // library out of the *initial* environment would therefore not be enough on
    // its own.
    expect(await checkMessages('(import runtime)\n(##report## 5)')).not.toEqual(
      [],
    )
    expect(
      await checkMessages('(import runtime r)\n(r.##report## 5)'),
    ).not.toEqual([])
  })

  test('`##ap-spread##` is not a form source can ask for', async () => {
    // Not an environment binding at all: codegen recognizes this name in an
    // application head and lowers an inline spread-application, and contract
    // insertion is its only author. So it is reachable no matter what the
    // environment holds, and only denying the *reference* closes it.
    expect(await checkMessages('(##ap-spread## + (list 1 2))')).not.toEqual([])
    expect(await runProgram('(##ap-spread## + (list 1 2))')).not.toEqual(['3'])
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

import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/683
//
// A runtime error raised by a native that sets no source of its own is blamed
// on a name chosen in applyFn (src/lpm/handlers/op-handlers.ts), which read:
//
//   e.source ??= currFrame.origin === 'builtin' && !currFrame.name.startsWith('##')
//     ? currFrame.name
//     : fn.name
//
// The first arm was guarded against the internal `##...##` spellings; the
// `fn.name` fallback was not -- and every internal primitive runtime.scm binds
// *is* named that way, since Module.registerValue renames the Javascript
// function to its Scamper binding. So whenever an internal native raises
// without naming someone, its own reserved spelling is what the student reads:
//
//   {1 2}
//   -> Runtime error [1:1-1:5]: (##mkObj##) A map key must be a string, ...
//
// That is the asymmetry #683 records: the closure-arity check a few lines below
// calls its own `##` guard load-bearing, and this sibling has none. #669 named
// the site and #670 scoped it out as unreachable, having only `##checkArity##`
// in view -- but a map literal needs no contracts to reach it, so the leak was
// a student's to see.
//
// These tests say only that no internal spelling is blamed, not which name
// replaces it: a map literal has no procedure at fault, while the contract
// wrapper's own frame does know one.

describe('#683: an internal `##...##` name is never the blamed procedure', () => {
  test('a map literal with a non-string key blames no internal', async () => {
    // Plain student code: no contracts, the default compile options the IDE
    // and the CLI use.
    const out = await runProgram('{1 2}', { stripRanges: true })
    expect(out).toHaveLength(1)
    expect(out[0]).toContain('A map key must be a string, received number')
    expect(out[0]).not.toContain('##')
  })

  // #683's own repro. Contracts are inserted for the standard library only
  // (src/lib/index.ts), so this reaches `##checkArity##` the way a library
  // procedure does -- except that the wrapper is user code, which is what
  // takes the guarded first arm out of play.
  test('over-applying an optional parameter blames no internal', async () => {
    const out = await runProgram(
      `
      ;;; (greet name [greeting]) -> string?
      ;;;  name : string?
      ;;;  greeting : string?
      ;;;   defaults to "Hello"
      ;;; Greets someone.
      (define greet
        (lambda (name greeting)
          (string-append (if (void? greeting) "Hello" greeting) ", " name)))
      (greet "Ada" "Howdy" "extra")
      `,
      { insertContracts: true, stripRanges: true },
    )
    expect(out).toHaveLength(1)
    expect(out[0]).toContain(
      'Arity mismatch in function call: expected at most 2 arguments, got 3',
    )
    expect(out[0]).not.toContain('##')
  })

  // The same ceiling check on a *library* procedure, which takes the guarded
  // arm and is named correctly today. Pinned so a fix cannot regress it.
  test('a library procedure still names itself at its arity ceiling', async () => {
    expect(
      await runProgram('(substring "alphabetical" 1 2 3)', { stripRanges: true }),
    ).toEqual([
      'Runtime error: (substring) Arity mismatch in function call: expected at most 3 arguments, got 4',
    ])
  })
})

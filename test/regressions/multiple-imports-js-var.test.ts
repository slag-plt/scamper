import { describe, expect, test } from 'vitest'
import builtinLibs from '../../src/lib'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// Regression test for #663.
//
// js-var is the FFI root, so it cannot be bound via itself: src/lib/index.ts
// injects it into every builtin library's load environment rather than defining
// it in Scheme. It used to be added to every one of those libraries' *export*
// sets as well, so all 14 modules claimed to export it -- and importing any two
// of them brought one name in from two modules, which the scope checker rightly
// reports. `(import html)` then `(import image)` warned "Global variable
// 'js-var' is already defined".
//
// Only runtime exports it now. That one export is what already made it global:
// scopeCheckProgram seeds its globals from runtime, and runtime is in every
// program's default environment, so js-var needs no import at all.

async function scopeErrors(src: string): Promise<string[]> {
  const errors: ScamperDiagnostic[] = []
  const parseErrs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(parseErrs, src)
  expect(parseErrs, 'test source should parse cleanly').toEqual([])
  await scopeCheckProgram(errors, expandProgram(prog))
  return errors.map((e) => e.message)
}

/** Every builtin library name, in load order. */
const libs = [...builtinLibs.keys()]

describe('#663: two library imports do not collide on js-var', () => {
  test('the reported repro is clean', async () => {
    expect(await scopeErrors('(import html)\n(import image)')).toEqual([])
  })

  test('the other reported pairings are clean', async () => {
    expect(await scopeErrors('(import image)\n(import test)')).toEqual([])
    expect(await scopeErrors('(import image)\n(import html)')).toEqual([])
  })

  test('no pair of builtin libraries collides on js-var', async () => {
    // Filtered on js-var rather than asserted empty: a handful of pairs
    // genuinely re-export the same native under one name (canvas/image on
    // canvas?, html/reactive on button?, ...), which was a separate issue at
    // the time. #682 has since fixed those, and its sweep asserts the stronger
    // claim -- no pair warns at all -- so this one can no longer fail on its
    // own. It stays as #663's own record of what it was about; the assertion
    // below it is the pin that can still fail independently.
    const offenders: string[] = []
    for (const [i, a] of libs.entries()) {
      for (const b of libs.slice(i + 1)) {
        const messages = await scopeErrors(`(import ${a})\n(import ${b})`)
        offenders.push(
          ...messages
            .filter((m) => m.includes('js-var'))
            .map((m) => `(import ${a}) + (import ${b}): ${m}`),
        )
      }
    }
    expect(offenders).toEqual([])
  })

  test('exactly one library exports js-var', () => {
    expect(
      libs.filter((name) => builtinLibs.get(name)?.bindings.has('js-var') === true),
    ).toEqual(['runtime'])
  })

  test('js-var is still reachable with no imports at all', async () => {
    // What makes the assertion above a de-duplication rather than the removal
    // of a capability: runtime's single export is already global everywhere.
    expect(await scopeErrors('((js-var "prelude_numberQ") 5)')).toEqual([])
  })
})

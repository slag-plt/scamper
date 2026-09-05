// @vitest-environment node
import { describe, expect, test } from 'vitest'
import { ICE, ScamperError } from '../../src/lpm'
import * as U from '../../src/lpm/util'
import { runProgram } from '../harness'

// #514: `toString` tests `v instanceof HTMLElement` before it tests for a
// struct. `instanceof` evaluates its right operand as an ordinary identifier
// reference, so outside the browser -- where nothing declares `HTMLElement` --
// that test is a `ReferenceError` rather than a `false`, and it takes down
// *every* value that reaches it: a struct, a map, an error, the fallback. The
// answer outside the browser is that nothing is an HTMLElement, so the test
// guards its global with `typeof`, as the six predicates in #508 do.
//
// N.B., this file overrides the suite's jsdom environment on purpose. Under
// jsdom `HTMLElement` *is* defined and this bug is invisible --
// test/lpm/util.test.ts stringifies these same values there and always has.
describe('#514: toString where there is no DOM', () => {
  test('the DOM global really is absent here', () => {
    expect(typeof HTMLElement).toBe('undefined')
  })

  test('a struct stringifies rather than throwing', () => {
    expect(U.toString(U.mkStruct('point', ['x', 'y'], [1, 2]))).toBe('(point 1 2)')
  })

  // Reordering the struct branch above the HTMLElement one would rescue only
  // the first of these; everything else still falls past it.
  test('so does every other value that falls past the pair branch', () => {
    expect(U.toString({ a: 1 })).toBe('{ "a" : 1 }')
    for (const e of [
      new ScamperError('Runtime', 'boom'),
      new ICE('someFn', 'unreachable'),
      new Error('oops'),
    ]) {
      expect(U.toString(e)).toBe(e.toString())
    }
    const blob = new (class { foo = 1 })()
    expect(U.toString(blob)).toBe(`[Blob: ${JSON.stringify(blob)}]`)
  })

  // The path a program takes there. `##report##` builds a ReportError, whose
  // message is `toString` of the reported value; a query and every docstring
  // example (src/scheme/examples.ts) wrap their target in it. Both are IDE-only
  // -- which is why nobody has hit this -- but the name is referenceable from
  // ordinary source, so a program run on the CLI can reach it too. N.B., that
  // referenceability is itself pinned, in internal-name-hygiene.test.ts; if it
  // is ever tightened, this case goes and cases 2-3 carry the regression.
  test('a reported struct comes back as its value, not a ReferenceError', async () => {
    expect(
      await runProgram('(struct point (x y))\n(##report## (point 1 2))', {
        stripRanges: true,
      }),
    ).toEqual(['Runtime error: (##report##) Reported value: (point 1 2)'])
  })
})

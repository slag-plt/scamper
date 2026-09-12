import { describe, expect, test } from 'vitest'
import { docRegistry } from '../../src/lib'
import { expToString } from '../../src/scheme/ast'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/531
//
// `sample-node`'s docstring declared `v : any` while the refinement line
// directly under it said `vector? of numbers between -1.0 and 1.0`. The
// declared half is the one src/lib/index.ts turns into a contract, so the
// looser half won and a list reached the native -- where `for (const sample of
// data)` (src/js/audio/index.ts) rejected it as a bare
//
//   Runtime error: Unexpected error in Javascript function call:
//   TypeError: data is not iterable
//
// naming a Javascript parameter the student never wrote. `sample-node` touches
// no Web Audio at all, so this is reachable from the CLI and a Gradescope
// autograder as readily as from the IDE, which is why these run under the
// suite's default environment rather than a browser one.
//
// These go through the Scheme binding rather than calling `audio_sampleNode`
// directly: the contract is the thing under test, and a direct call never
// builds the wrapper.
//
// Ranges are stripped -- they point at the student's own call, which says
// nothing these tests are about.

/** What `typeOf` calls each value the contract should now turn away. */
const REJECTED: [label: string, expr: string, received: string][] = [
  // The issue's own program.
  ['a list of numbers', '(list 0.1 0.2)', 'list'],
  // The empty list is the same mistake with nothing in it, and `null` rather
  // than `list` is what the student is told it was.
  ['the empty list', '(list)', 'null'],
  ['a number', '5', 'number'],
  ['a boolean', '#t', 'boolean'],
  // A string is the one non-vector Javascript happily iterates, so this one
  // never reached the for-of guard's complaint about `data` -- it reached the
  // *element* check instead, and was told to supply "a list of numbers" by the
  // one function that does not take a list.
  ['a string', '"abc"', 'string'],
]

describe('#531: sample-node turns away a non-vector through its contract', () => {
  test.each(REJECTED)('%s', async (_label, expr, received) => {
    expect(
      await runProgram(`(import audio)\n(sample-node ${expr})`, {
        stripRanges: true,
      }),
    ).toEqual([`Runtime error: (error) expected a vector, received ${received}`])
  })

  test('no raw Javascript error reaches the student', async () => {
    for (const [, expr] of REJECTED) {
      const out = (
        await runProgram(`(import audio)\n(sample-node ${expr})`, {
          stripRanges: true,
        })
      ).join('\n')
      expect(out).not.toContain('TypeError')
      expect(out).not.toContain('Unexpected error in Javascript function call')
    }
  })

  // Behaviour alone cannot see a docstring that has stopped parsing: the
  // binding would simply lose its contract and every assertion above would
  // start passing for the wrong reason (see #181's note in
  // audio-pipeline-sample.browser.test.ts). So assert the declared type too.
  test('the parameter is declared a vector, not `any`', () => {
    const doc = docRegistry.get('audio')?.get('sample-node')
    expect(doc).toBeDefined()
    expect(doc?.params.map((p) => [p.name, expToString(p.predicate)])).toEqual([
      ['v', 'vector?'],
    ])
  })
})

describe('#531: what sample-node already accepted, it still accepts', () => {
  test('a vector of numbers in range makes a sample', async () => {
    expect(
      await runProgram(
        '(import audio)\n(sample? (sample-node (vector 0 0.5 -0.5)))',
        { stripRanges: true },
      ),
    ).toEqual(['#t'])
  })

  // The range check belongs to the native and stays there: the contract only
  // has to get a vector to it. Asserted by shape rather than verbatim -- the
  // `(sample-node)` source is what says the native raised it rather than the
  // contract, and the noun in that message is still the old "list" -- a
  // correction that belongs to src/js/audio/index.ts rather than here.
  test("an element out of range is still the native's own complaint", async () => {
    const out = await runProgram('(import audio)\n(sample-node (vector 2.0))', {
      stripRanges: true,
    })
    expect(out).toHaveLength(1)
    expect(out[0]).toContain('(sample-node)')
    expect(out[0]).toContain('between -1.0 and 1.0')
    expect(out[0]).not.toContain('(error)')
  })
})

import { describe, expect, test } from 'vitest'
import { docRegistry } from '../../src/lib'
import { expToString } from '../../src/scheme/ast'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/590
//
// Four parameters outside `data.scm` (#589) declared `: any` while the
// refinement line directly under each named a real type. Only the *declared*
// predicate becomes a contract (src/lib/index.ts); the refinement is free
// prose (`Param.description`) and is never checked. So the looser half won and
// the value reached the implementation:
//
//   `any-of`/`all-of` -- a non-procedure was stored and only complained about
//   later, from the *derived* closure's call rather than the `(all-of 5)` that
//   made the mistake, with a message naming neither `all-of` nor `procedure?`.
//
//   `on-timer` -- accepted silently. `setInterval(fn, "hi")` coerces to NaN,
//   so the timer simply never behaved and nothing said why. The worst of the
//   four.
//
//   `tag-set-children!` -- the native demands an `HTMLElement` of `elt` *and*
//   of every child. On the CLI #516's `requireBrowser()` shields it, but a
//   student is in the browser, where it did not.
//
// Same shape as #531 and #589: name the predicate the prose already names.
//
// These run under the suite's default jsdom environment, which is what lets
// the `html` cases exercise the browser path the CLI's guard hides.
//
// Ranges are stripped -- they point at the student's own call, which says
// nothing these tests are about.

/** `program`'s output with error ranges dropped -- see RunOptions.stripRanges. */
const report = (program: string): Promise<string[]> =>
  runProgram(program, { stripRanges: true })

/**
 * The declared predicates of a library binding's parameters, in call order,
 * as `[name, predicate]` pairs.
 *
 * Behaviour alone cannot see a docstring that has stopped parsing: the binding
 * would simply lose its contract and every assertion below would start passing
 * for the wrong reason. So each site asserts its declared type too.
 */
function declared(module: string, name: string): [string, string][] {
  const doc = docRegistry.get(module)?.get(name)
  expect(doc, `${module}:${name} has no parsed docstring`).toBeDefined()
  const params = [
    ...(doc?.params ?? []),
    ...(doc?.optParams ?? []),
    ...(doc?.restParam ? [doc.restParam] : []),
  ]
  return params.map((p) => [p.name, expToString(p.predicate)])
}

describe('#590: any-of and all-of take procedures', () => {
  test.each(['any-of', 'all-of'])(
    '%s rejects a non-procedure at the call that supplied it',
    async (name) => {
      expect(await report(`(${name} 5)`)).toEqual([
        'Runtime error: (error) expected every value of f1 to be a procedure, but at least one was not',
      ])
    },
  )

  // The defect's own program. The complaint used to arrive here, one call too
  // late, as "Not a function or closure: 5" -- which names neither the
  // function that was misused nor the type it wanted.
  test.each(['any-of', 'all-of'])(
    'the error arrives before %s hands back a closure',
    async (name) => {
      // The define is what fails now, so `p` is never bound and the call
      // below it has nothing to reach -- the scheduler's usual "carry on at
      // the next statement". What matters is the first line.
      const out = await report(`(define p (${name} 5))\n(p 1)`)
      expect(out[0]).toBe(
        'Runtime error: (error) expected every value of f1 to be a procedure, but at least one was not',
      )
      expect(out.join('\n')).not.toContain('Not a function or closure')
    },
  )

  test.each(['any-of', 'all-of'])('%s declares f1 a procedure', (name) => {
    expect(declared('prelude', name)).toEqual([['f1', 'procedure?']])
  })

  test('what they already accepted, they still accept', async () => {
    expect(
      await report(
        '(list ((any-of string? number?) 4) ((all-of number? even?) 4) ((all-of number? even?) 3))',
      ),
    ).toEqual(['(list #t #t #f)'])
  })
})

describe('#590: on-timer takes an integer', () => {
  // The worst of the four: this was accepted without a word.
  test('a string is refused rather than silently taken', async () => {
    expect(await report('(import reactive)\n(on-timer "hi")')).toEqual([
      'Runtime error: (error) expected an integer, received string',
    ])
  })

  test('the declared type is integer?, not any', () => {
    expect(declared('reactive', 'on-timer')).toEqual([['interval', 'integer?']])
  })

  // Non-negativity stays prose: the library has no predicate for it, and a
  // negative interval is `setInterval`'s own business.
  test('an interval it documents still makes a subscription', async () => {
    expect(
      await report('(import reactive)\n(subscription? (on-timer 100))'),
    ).toEqual(['#t'])
  })
})

describe('#590: tag-set-children! takes elements', () => {
  test('a non-element parent is refused', async () => {
    expect(
      await report('(import html)\n(tag-set-children! "div" (tag "span"))'),
    ).toEqual([
      'Runtime error: (error) expected an element as the first argument, received string',
    ])
  })

  test('a non-element child is refused', async () => {
    expect(
      await report('(import html)\n(tag-set-children! (tag "div") "kid")'),
    ).toEqual([
      'Runtime error: (error) expected every value of c to be an element, but at least one was not',
    ])
  })

  // The native's own complaints are still there behind the contract; the point
  // is that a student no longer has to reach them to find out what was wrong.
  test('no raw Javascript error reaches the student', async () => {
    for (const program of [
      '(import html)\n(tag-set-children! "div" (tag "span"))',
      '(import html)\n(tag-set-children! (tag "div") "kid")',
      '(import html)\n(tag-set-children! 5 5)',
    ]) {
      const out = (await report(program)).join('\n')
      expect(out).not.toContain('TypeError')
      expect(out).not.toContain('Unexpected error in Javascript function call')
    }
  })

  test('both parameters are declared element?, not any', () => {
    expect(declared('html', 'tag-set-children!')).toEqual([
      ['elt', 'element?'],
      ['c', 'element?'],
    ])
  })

  // The leading `void` is what `tag-set-children!` displays: the native
  // returns nothing, despite the signature saying `element?`. Pinned as it is
  // rather than corrected -- a return type is not what this issue is about.
  test('setting real element children still works', async () => {
    expect(
      await report(
        '(import html)\n(define d (tag "div"))\n(tag-set-children! d (tag "span"))\n(element? d)',
      ),
    ).toEqual(['void', '#t'])
  })
})

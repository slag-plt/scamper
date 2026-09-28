import { describe, expect, test } from 'vitest'
import { docRegistry } from '../../src/lib'
import { expToString } from '../../src/scheme/ast'
import { html_tag, html_tagSetChildren } from '../../src/js/html'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/634
//
// Ten places where a docstring, a message, or a predicate said something the
// code did not do. They are unrelated defects with one thing in common: each
// misleads a student who reads it, and none of them is visible to a test that
// only asks whether the right values come back. So each is pinned by the text
// it produces.
//
// Ranges are stripped throughout -- they point at the student's own call, which
// is not what any of these are about.

/** `program`'s output, with ranges dropped. */
function run(program: string): Promise<string[]> {
  return runProgram(program, { stripRanges: true })
}

describe('a message names the value it is about', () => {
  // The child check read `L.typeOf(elt)` -- the *parent* -- so every rejected
  // child was reported as "a object", the parent element's type, whatever the
  // child actually was. It also counted from zero ("position 0") where every
  // contract message counts from one ("the first"), and carried a stray `$`
  // before its full stop.
  test('a rejected child is described, not its parent', () => {
    const parent = html_tag('div')
    expect(() => { html_tagSetChildren(parent, 'not an element') }).toThrow(
      'expected an HTML element as the first child, received string',
    )
  })

  test('the ordinal counts from one, as a contract message does', () => {
    const parent = html_tag('div')
    expect(() => { html_tagSetChildren(parent, html_tag('p'), 42) }).toThrow(
      'expected an HTML element as the second child, received number',
    )
  })

  // `hsv:` was spelled into the message as well as supplied as the error's
  // source, so a student saw the name twice; and the hue message read "in the
  // an angle".
  test('hsv says the name once, and says it in English', async () => {
    expect(await run('(import image)\n(hsv 400 50 50)')).toEqual([
      'Runtime error: (hsv) expected hue to be an angle between 0 and 360, ' +
        'received 400',
    ])
  })

  // "Data for dataset-bubble must be a list of three numbers" said the `data`
  // argument had to be a triple, where it is each *point* that does.
  test('a bubble point, not the whole argument, must be a triple', async () => {
    expect(
      await run('(import data)\n(dataset-bubble "t" (list (list 1 2)))'),
    ).toEqual([
      'Runtime error: (dataset-bubble) every data point must be a list of ' +
        'three numbers',
    ])
  })
})

describe('the article follows how a name is said', () => {
  // The article was chosen from the first *letter*, so an initialism got the
  // wrong one: `rgb` is said "ar-gee-bee", hence "an rgb".
  test.each([
    ['(rgb-red 5)', '(rgb-red) expected an rgb as the first argument, received number'],
    ['(hsv->rgb 5)', '(hsv->rgb) expected an hsv as the first argument, received number'],
    ['(rgb 300 0 0)', '(rgb) expected an rgb-component as the first argument, received number'],
  ])('%s', async (call, message) => {
    expect(await run(`(import image)\n${call}`)).toEqual([
      `Runtime error: ${message}`,
    ])
  })

  // A name that is a word keeps the ordinary spelling rule, so the letter L
  // being said "el" does not make `list?` read "an list".
  test.each([
    ['(car 5)', 'pair or nonempty-list'],
    ['(length 5)', 'a list'],
    ['(string-length 5)', 'a string'],
    ['(char->integer 5)', 'a char'],
    ['(vector-length 5)', 'a vector'],
    ['(quotient 5.5 2)', 'an integer'],
  ])('%s', async (call, expected) => {
    const out = (await run(call)).join('\n')
    expect(out).toContain(`expected ${expected}`)
  })
})

describe('a derived predicate is described, not printed', () => {
  // `(list-of pair?)` fell through to "a value matching `(list-of pair?)`",
  // handing a student the predicate's source. Every `list-of` parameter in the
  // library is covered here: image.scm's three point lists, and the data.scm
  // element types this issue also declared.
  test.each([
    ['(import image)\n(polygon 5 "red" "solid")', 'a list of pairs'],
    ['(import data)\n(dataset-bar "t" 5)', 'a list of numbers'],
    ['(import data)\n(dataset-scatter "t" 5)', 'a list of pairs'],
    ['(import data)\n(dataset-bubble "t" 5)', 'a list of lists'],
    ['(import data)\n(plot-category 5 (dataset-bar "c" (list 1)))', 'a list of strings'],
    ['(import data)\n(dataset-line "t" 5)', 'a list of numbers or pairs'],
  ])('%s', async (program, expected) => {
    const out = (await run(program)).join('\n')
    expect(out).toContain(`expected ${expected}`)
    expect(out).not.toContain('a value matching')
  })

  // The element type is what the contract now checks, which is the half of
  // #589 that was left: the outer `list?` passed and the native built a chart
  // out of strings.
  test('a list of the wrong elements is turned away', async () => {
    expect(await run('(import data)\n(dataset-bar "t" (list "a" "b"))')).toEqual([
      'Runtime error: (dataset-bar) expected a list of numbers as the second ' +
        'argument, received list',
    ])
  })
})

describe('a docstring says what the binding does', () => {
  // `-> element?` while html_tagSetChildren returns nothing. A return
  // predicate is documentation only -- contract insertion never checks one --
  // so nothing but a reader was misled.
  test('tag-set-children! is documented as returning nothing', () => {
    const doc = docRegistry.get('html')?.get('tag-set-children!')
    expect(doc).toBeDefined()
    expect(doc && expToString(doc.signature.predicate)).toBe('void?')
  })

  // rgb-component? tests the range and not integrality. Per #609 the loose
  // check is deliberate -- `(rgb (/ x 2) 0 0)` should keep working -- so it is
  // the docstring that was wrong.
  test('rgb-component? documents the range it actually tests', async () => {
    const doc = docRegistry.get('image')?.get('rgb-component?')
    expect(doc?.description).toContain('a number between 0 and 255')
    expect(doc?.description).not.toContain('an integer between 0 and 255')
    expect(
      await run('(import image)\n(rgb-component? 32.5)\n(rgb-component? 300)'),
    ).toEqual(['#t', '#f'])
  })
})

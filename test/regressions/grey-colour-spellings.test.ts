import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/757
//
// CSS defines 148 named colours; Scamper had 141. The seven missing ones were
// exactly the `grey` spellings, so the British spelling failed where the
// American one worked -- `(color-name? "grey")` was #f and
// `(color->rgb "dimgrey")` was a runtime error.
//
// That is its own small trap, and #738 sharpened it: the colour documentation
// now tells a student to use "dimgray" for a grey darker than "gray", advice
// that errored if they typed "dimgrey".
//
// The aliases are *derived*, not listed: exactly seven canonical names contain
// "gray", and replacing that with "grey" in each produces exactly the seven
// names CSS has and Scamper lacked. So the set cannot drift from the table.
//
// The thing this must not break is `color->color-name`, which walks the table
// in insertion order and answers the first nearest name. An alias entered into
// that table would become a tie it could pick, turning
// `(color->color-name (rgb 128 128 128))` from "gray" into "grey" -- a visible
// regression in the opposite direction. The last describe block pins that.

const GREYS: [string, string][] = [
  ['grey', 'gray'],
  ['darkgrey', 'darkgray'],
  ['dimgrey', 'dimgray'],
  ['lightgrey', 'lightgray'],
  ['slategrey', 'slategray'],
  ['darkslategrey', 'darkslategray'],
  ['lightslategrey', 'lightslategray'],
]

describe('the grey spellings are colour names too (#757)', () => {
  test.each(GREYS)('%s is a colour name', async (grey) => {
    expect(await runProgram(`(import image)\n(color-name? "${grey}")`)).toEqual([
      '#t',
    ])
  })

  test.each(GREYS)('%s resolves to the same colour as %s', async (grey, gray) => {
    expect(
      await runProgram(`
(import image)
(equal? (color->rgb "${grey}") (color->rgb "${gray}"))
`),
    ).toEqual(['#t'])
  })

  test('and the spelling works wherever a colour name is taken', async () => {
    // `color?` is the predicate the drawing procedures contract on, so this is
    // what decides whether `(circle 20 "solid" "dimgrey")` is accepted at all.
    expect(
      await runProgram(`
(import image)
(color? "dimgrey")
(image-width (circle 20 "solid" "dimgrey"))
`),
    ).toEqual(['#t', '20'])
  })

  test('case is ignored, as it is for the canonical spellings', async () => {
    // `color_isColorName` lower-cases what it checks; #154 is why. An alias
    // that skipped that would accept "grey" and reject "Grey".
    expect(
      await runProgram(`
(import image)
(color-name? "Grey")
(equal? (color->rgb "DimGrey") (color->rgb "dimgray"))
`),
    ).toEqual(['#t', '#t'])
  })
})

describe('searching in the other spelling finds them (#757)', () => {
  test('find-colors "grey" answers the canonical names', async () => {
    // The search a student who cannot find a grey actually tries. It answers
    // the gray spellings, not the grey ones, so what they get back is the
    // name `color->color-name` will also use.
    expect(
      await runProgram('(import image)\n(find-colors "grey")'),
    ).toEqual([
      '(list "darkgray" "darkslategray" "dimgray" "gray" "lightgray" "lightslategray" "slategray")',
    ])
  })

  test('and searching the canonical spelling is unchanged', async () => {
    expect(
      await runProgram('(import image)\n(find-colors "gray")'),
    ).toEqual([
      '(list "darkgray" "darkslategray" "dimgray" "gray" "lightgray" "lightslategray" "slategray")',
    ])
  })

  test('but a query that merely shares letters with "grey" does not', async () => {
    // `gray` holds no `e`, so consulting the alias unconditionally made
    // `(find-colors "e")` answer with the whole grey family -- and worse,
    // `(find-colors "re")`, which is how a student hunts for the reds. Only
    // a query that spells out `grey` looks at the alias.
    const [reds] = await runProgram('(import image)\n(find-colors "re")')
    expect(reds).toContain('darkred')
    for (const grey of ['"gray"', '"dimgray"', '"lightgray"', '"darkgray"']) {
      expect(reds, `"re" should not find ${grey}`).not.toContain(grey)
    }
    // `darkslategray` and `lightslategray` do not contain "re" either.
    expect(reds).not.toContain('slategray')
  })

  test('a partial spelling of grey still reaches the aliases', async () => {
    expect(await runProgram('(import image)\n(find-colors "kgrey")')).toEqual([
      '(list "darkgray")',
    ])
  })

  test('a query matching both a name and its alias returns it once', async () => {
    // "slate" matches slategray directly *and* slategrey as an alias, so a
    // naive implementation lists those three twice. The slate blues have no
    // alias and are here only because they match the query on their own.
    expect(
      await runProgram('(import image)\n(find-colors "slate")'),
    ).toEqual([
      '(list "darkslateblue" "darkslategray" "lightslategray" "mediumslateblue" "slateblue" "slategray")',
    ])
  })
})

describe('but naming a colour still answers the canonical spelling (#757)', () => {
  test('color->color-name is unchanged for all seven', async () => {
    // The regression an alias in the table itself would have caused.
    expect(
      await runProgram(`
(import image)
(color->color-name (rgb 128 128 128))
(color->color-name (rgb 169 169 169))
(color->color-name (rgb 105 105 105))
(color->color-name (rgb 211 211 211))
`),
    ).toEqual(['"gray"', '"darkgray"', '"dimgray"', '"lightgray"'])
  })

  test('naming an alias string answers the canonical spelling', async () => {
    // `color->color-name` short-circuits on a string that already names a
    // colour, so widening the guard made it hand the alias straight back --
    // the one place an alias escaped as an answer. It also split the two
    // halves of `describe-color`: a bare string kept the grey spelling while
    // a shape, which stores an Rgb, came back through the nearest-name walk
    // and gave the gray one. The same colour, described two ways.
    expect(
      await runProgram(`
(import image)
(color->color-name "dimgrey")
(describe-color "slategrey")
(describe-color (color->rgb "dimgrey"))
(equal? (describe-color "dimgrey") (describe-color (color->rgb "dimgrey")))
`),
    ).toEqual(['"dimgray"', '"slategray"', '"dimgray"', '#t'])
  })

  test('and a name that merely looks grey is still no colour', async () => {
    expect(
      await runProgram(`
(import image)
(color-name? "greyish")
(color-name? "xgrey")
(color-name? "grey ")
`),
    ).toEqual(['#f', '#f', '#f'])
  })

  test('and all-color-names still lists each colour once', async () => {
    // 141 canonical names, not 148: an alias is a way to *write* a colour, not
    // another colour. Listing both spellings would show a student seven
    // duplicate swatches.
    expect(
      await runProgram(`
(import image)
(length (all-color-names))
(length (filter (lambda (n) (equal? n "gray")) (all-color-names)))
(length (filter (lambda (n) (equal? n "grey")) (all-color-names)))
`),
    ).toEqual(['141', '1', '0'])
  })
})

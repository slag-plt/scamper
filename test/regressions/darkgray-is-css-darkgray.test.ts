import { describe, expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { runProgram } from '../harness.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { isCategoryTag } from '../../src/scheme/docstring/tags/category-tag'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// https://github.com/slag-plt/scamper/issues/738
//
// `(color->rgb "darkgray")` is `(rgb 169 169 169)` while `(color->rgb "gray")`
// is `(rgb 128 128 128)`, so "darkgray" is *lighter* than "gray". It reads as
// a bug and is not one: those are exactly the values CSS defines, and every
// browser agrees. The inversion is inherited, and specifically it is CSS's
// own: "gray" came from the VGA palette by way of HTML as #808080, while
// "darkgray" came from X11 as #A9A9A9 -- and X11's own "gray" was #BEBEBE, so
// within X11 "darkgray" really was darker. Keeping the one and importing the
// other is what put them the wrong way round. CSS Color 4 says so itself, in
// the same breath as this example: the names are standardised "not because
// they are good, but because their use and implementation has been widespread
// for decades and the standard needs to reflect reality".
//
// So the resolution of #738 was to document the surprise rather than to
// deviate from CSS for one name: a student comparing Scamper against any CSS
// reference still finds agreement, and nothing already drawn changes colour.
// `dimgray` (#696969) is the name that actually is darker than `gray`.
//
// This file therefore pins a *decision*, not a repair. If a later change makes
// `darkgray` darker, these tests are what should stop it long enough for
// whoever is doing it to read #738 first.

describe('the CSS gray names keep their CSS values (#738)', () => {
  test('gray, darkgray and dimgray are exactly what CSS says', async () => {
    expect(await runProgram(`
(import image)
(color->rgb "gray")
(color->rgb "darkgray")
(color->rgb "dimgray")
`)).toEqual([
      // Printed as `rgba` -- the stored struct carries its alpha, and the
      // issue's transcript abbreviated it. 128/169/105 are the point here.
      '(rgba 128 128 128 255)',
      '(rgba 169 169 169 255)',
      '(rgba 105 105 105 255)',
    ])
  })

  test('so darkgray is lighter than gray, deliberately', async () => {
    // Not a typo and not a bug to be fixed here -- see the header.
    expect(await runProgram(`
(import image)
(> (rgb-red (color->rgb "darkgray")) (rgb-red (color->rgb "gray")))
(< (rgb-red (color->rgb "dimgray")) (rgb-red (color->rgb "gray")))
`)).toEqual([
      '#t',
      '#t',
    ])
  })

  test('and the colour-name documentation warns about it', () => {
    // The fix for #738 is this sentence. A student who wonders why their
    // "dark" gray came out light has to be able to find the answer where they
    // are already looking, which is the library documentation.
    const src = readFileSync(
      resolve(__dirname, '../../src/lib/image.scm'),
      'utf-8',
    )
    const diagnostics: ScamperDiagnostic[] = []
    const prog = parseProgramFromSource(diagnostics, src)
    expect(diagnostics.map((d) => d.message)).toEqual([])
    const stmt = prog.find(
      (s) =>
        (s.tag === 'define' || s.tag === 'defexport') &&
        s.name.name === 'all-color-names',
    )
    expect(stmt).toBeDefined()
    if (stmt?.tag !== 'define' && stmt?.tag !== 'defexport') return
    const { doc } = parseFunctionDocFromComments(stmt.docComments ?? [])
    expect(doc).toBeDefined()
    if (doc === undefined) return
    const description = doc.description.toLowerCase()
    expect(description).toContain('css')
    expect(description).toContain('darkgray')
    expect(description).toContain('dimgray')
    // The claim itself, not just the names: a rewrite could keep all three
    // words and still lose which way round the two greys go.
    expect(description).toContain('lighter')
    // Scamper has 141 of CSS's 148 names -- the seven "grey" spellings are
    // absent -- so the note must not promise more than that, or a student
    // taking its advice and typing "dimgrey" gets an error from the very
    // sentence meant to help them.
    expect(description).toContain('grey')
  })

  test('and the two lookups a student arrives through point at that note', () => {
    // `color->rgb` is the procedure the issue itself used, and `color-name->rgb`
    // is the other way in. The docs search indexes `@category` but never the
    // description, so without these cross-references the note is unreachable
    // from where the surprise actually happens.
    const src = readFileSync(
      resolve(__dirname, '../../src/lib/image.scm'),
      'utf-8',
    )
    const diagnostics: ScamperDiagnostic[] = []
    const prog = parseProgramFromSource(diagnostics, src)
    for (const name of ['color->rgb', 'color-name->rgb']) {
      const stmt = prog.find(
        (s) =>
          (s.tag === 'define' || s.tag === 'defexport') && s.name.name === name,
      )
      expect(stmt, name).toBeDefined()
      if (stmt?.tag !== 'define' && stmt?.tag !== 'defexport') continue
      const { doc } = parseFunctionDocFromComments(stmt.docComments ?? [])
      expect(doc, name).toBeDefined()
      if (doc === undefined) continue
      const categories = doc.tags
        .filter(isCategoryTag)
        .flatMap((t) => t.contents)
      const mentions =
        categories.includes('all-color-names') ||
        doc.description.includes('all-color-names')
      expect(mentions, `${name} should route to all-color-names`).toBe(true)
    }
  })
})

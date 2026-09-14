import { describe, expect, test } from 'vitest'
import { functionDocName } from '../../src/scheme/docstring/render'
import { searchByName } from '../../src/app/docs/search'
import { initializeLibs } from '../../src/lib'

await initializeLibs()

const names = (term: string) =>
  searchByName(term).matches.map((e) => functionDocName(e.doc))

// #603: the search box matched a name only in full, so a student who typed
// what they remembered of a name found nothing. `image-save!` is the issue's
// own example -- the trailing `!` is exactly the kind of character nobody
// recalls.
describe('#603: a partial name finds the functions containing it', () => {
  test('the reported case', () => {
    expect(names('image-save')).toContain('image-save!')
  })

  test('a partial name finds every function containing it', () => {
    expect(names('map')).toEqual(
      expect.arrayContaining([
        'map',
        'string-map',
        'vector-map',
        'vector-map!',
        'pixel-map',
      ]),
    )
  })

  test('a full name still finds itself', () => {
    expect(names('image-save!')).toContain('image-save!')
    expect(names('map')).toContain('map')
  })

  // A substring search would otherwise return the whole library, since "" is a
  // substring of everything. The docs page never asks -- it treats an empty box
  // as "no search" -- so this pins the function's own answer.
  test('an empty term still matches nothing', () => {
    expect(searchByName('').matches).toHaveLength(0)
  })

  // The ranking is the difference between a useful result and a list the
  // reader has to scan: in library order `string-map` comes before `map`, so
  // typing a name in full would not show it first.
  test('an exact name ranks above the names containing it', () => {
    expect(names('map')).toEqual([
      'map',
      'string-map',
      'vector-map',
      'vector-map!',
      'pixel-map',
    ])
  })

  test('a prefix ranks above a name that merely contains the term', () => {
    // `canvas-` names start with the term; `make-canvas` and `draw-canvas`
    // only contain it, so every prefix match comes first.
    const found = names('canvas-')
    const prefixes = found.filter((n) => n.startsWith('canvas-'))
    expect(prefixes.length).toBeGreaterThan(1)
    expect(found.slice(0, prefixes.length)).toEqual(prefixes)
  })

  test('the term is matched ignoring case', () => {
    expect(names('Map')).toEqual(names('map'))
    expect(names('IMAGE-SAVE')).toContain('image-save!')
  })
})

// Cross-references answer "what else is like this one", which needs a `this
// one` to be about. A fragment matching twenty functions has no such centre,
// so #603 draws them from an exact match only.
describe('#603: cross-references follow an exact name', () => {
  test('an exact term still offers its cross-references', () => {
    const { relatives } = searchByName('map')
    expect(relatives.map((e) => functionDocName(e.doc))).toEqual([
      'reduce',
      'reduce-right',
      'set-maximum-recursion-depth!',
    ])
  })

  test('a partial term offers none', () => {
    expect(searchByName('ma').relatives).toHaveLength(0)
    expect(searchByName('image-save').relatives).toHaveLength(0)
  })

  test('a cross-reference that is itself a match is not repeated', () => {
    // `map` names `string-map`, `vector-map` and `vector-map!` among its
    // relatives, and all three now match `map` in their own right.
    const { matches, relatives } = searchByName('map')
    const matched = new Set(matches.map((e) => functionDocName(e.doc)))
    for (const r of relatives.map((e) => functionDocName(e.doc))) {
      expect(matched.has(r)).toBe(false)
    }
  })
})

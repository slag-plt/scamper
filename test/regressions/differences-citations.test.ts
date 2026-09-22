import { existsSync, readFileSync } from 'fs'
import { resolve } from 'path'
import { describe, expect, test } from 'vitest'

// docs/DIFFERENCES.md is built entirely out of pointers into this tree: every
// claim it makes is either a citation or a transcript. That makes it uniquely
// prone to a failure with no symptom -- four pull requests merged between the
// document being written (#617) and being read, and its ~110 citations came to
// point at real code that was the wrong code. Nothing looked broken, because
// nothing was past end-of-file.
//
// These tests are what notices next time. They check three things:
//
//   1. every cited path exists, and every cited line is inside that file --
//      the cheap check, which catches a rename or a deletion;
//   2. every `file:line` citation still lands on the thing the sentence is
//      about, via ANCHORS below -- the check that catches a silent shift;
//   3. the two files that move on almost every pull request are cited by path
//      alone, so that a citation into either cannot go stale at all.
//
// Adding a numbered citation to the document means adding its anchor here.
// That is the point: the anchor is what makes the number checkable, and a
// number nobody can check is what got us here.

const ROOT = resolve(__dirname, '../..')
const DOC = 'docs/DIFFERENCES.md'
const doc = readFileSync(resolve(ROOT, DOC), 'utf-8').split('\n')

// The two files every pull request touches. A line number into either is stale
// within a release, so the document names the binding and cites the path bare.
const HOT_FILES = ['src/js/prelude/index.ts', 'src/lib/prelude.scm']

// The summary table abbreviates a path once the body has given it in full.
const ALIASES: Record<string, string | undefined> = {
  'index.ts': 'src/js/prelude/index.ts',
  'lang.ts': 'src/lpm/lang.ts',
  'prelude.scm': 'src/lib/prelude.scm',
  'syntax.grammar': 'src/scheme/syntax.grammar',
}

/** What a bare `:line` resolves to when no file has been named before it. */
const NO_FILE_NAMED = '(no file named before this citation)'

/** The lines a citation points at, inclusive. */
interface Lines {
  lo: number
  hi: number
}

/**
 * A citation as the document spells it. `lines` is absent for a path cited on
 * its own; `docLine` is where it sits in DIFFERENCES.md, so a failure says
 * where to look.
 */
interface Citation {
  docLine: number
  text: string
  path: string
  lines?: Lines
}

type NumberedCitation = Citation & { lines: Lines }

// A citation is always a code span. Three shapes appear: a path alone, a path
// with a line or range, and -- for a file the same sentence already named -- a
// bare `:line`. `.vue`/`.html` are here so a future citation into one is
// checked too, not because the document has one today.
const SPAN =
  /`((?:[A-Za-z0-9_.-]+\/)*[A-Za-z0-9_.-]+\.(?:ts|scm|grammar|md|vue|html))(?::(\d+)(?:-(\d+))?)?`|`:(\d+)(?:-(\d+))?`/g

function rangeOf(lo: string, hi: string | undefined): Lines {
  const start = Number(lo)
  return { lo: start, hi: hi ? Number(hi) : start }
}

function parseCitations(): Citation[] {
  const found: Citation[] = []
  // A bare `:line` means "the file named most recently", which is the whole
  // reason it can go wrong: the nearest preceding path is not always the file
  // the sentence is about. Resolving it here is what puts it under the same
  // existence and anchor checks as a spelled-out one.
  let mostRecent = NO_FILE_NAMED
  doc.forEach((line, i) => {
    for (const m of line.matchAll(SPAN)) {
      const text = m[0]
      const docLine = i + 1
      if (text.startsWith('`:')) {
        found.push({ docLine, text, path: mostRecent, lines: rangeOf(m[4], m[5]) })
      } else {
        mostRecent = ALIASES[m[1]] ?? m[1]
        found.push({
          docLine,
          text,
          path: mostRecent,
          lines: m[2] ? rangeOf(m[2], m[3]) : undefined,
        })
      }
    }
  })
  return found
}

const citations = parseCitations()
const numbered = citations.filter((c): c is NumberedCitation => c.lines !== undefined)

function linesOf(path: string): string[] {
  return readFileSync(resolve(ROOT, path), 'utf-8').split('\n')
}

/**
 * What each numbered citation is pointing at, in the words of the source. The
 * text must appear inside the cited lines; if the block moves, the test says
 * which citation to re-anchor and what to search for.
 *
 * Keyed by the citation exactly as the document spells it, so an alias in the
 * summary table and its full path in the body are separate entries -- they are
 * separate citations, and either can drift on its own.
 */
const ANCHORS: Record<string, string | undefined> = {
  // The shape of a program
  '`src/scheme/syntax.grammar:86-100`': 'Struct { paren<kw<"struct">',
  '`src/scheme/expansion.ts:98-117`': "case 'begin':",
  '`src/scheme/syntax.grammar:162`': 'LineComment',
  // Binding and control
  '`src/scheme/scope.ts:164-189`': 'letrec: every binder is in scope throughout',
  '`src/scheme/expansion.ts:118-150`': "case 'and':",
  '`src/scheme/expansion.ts:151-172`': "case 'cond':",
  '`src/scheme/syntax.grammar:139-145`': 'Amp { "&" }',
  // No macros / no quotation
  '`src/scheme/expansion.ts:250-278`': "case 'struct':",
  '`src/scheme/syntax.grammar:137`': 'baseIdentifier',
  // Numbers
  '`src/scheme/syntax.grammar:113-118`': 'Number {',
  // Pairs, lists and mutation
  '`src/lpm/lang.ts:582-584`': "We follow Clojure's lead",
  // Exceptions
  '`src/scheme/raise.ts:197`': 'with-handler is now an ordinary procedure',
  // Recursion
  '`src/lpm/limits.ts:12-23`': 'frames live in an array',
  '`src/lpm/limits.ts:10`': 'DEFAULT_MAX_CALL_STACK_DEPTH = 10_000',
  '`src/lpm/fiber.ts:290-298`': 'pushFrame',
  '`src/lpm/limits.ts:23`': 'MAX_CALL_STACK_DEPTH = 200_000',
  // The one prelude.scm citation that keeps its number: the sentence describes
  // the tail-recursive fold helpers without naming one, so there is nothing to
  // grep for and the line is the only way in.
  '`src/lib/prelude.scm:878-903`': 'fold-right-onto',
  // The module system
  '`src/scheme/syntax.grammar:90`': 'kw<"import">',
  '`src/scheme/scope.ts:449-460`': 'This matches Racket module semantics',
  // Summary table
  '`lang.ts:582`': "We follow Clojure's lead",
  '`syntax.grammar:5-9`': 'is a map literal',
  // From Racket
  '`docs/formatting.md:3`': "DrRacket's rules",
  '`src/scheme/style.ts:2`': 'following DrRacket',
  '`src/lib/image.scm:343-355`': '(circle diameter fill color',
  // From Clojure
  '`src/scheme/syntax.grammar:5-9`': 'is a map literal',
  '`:59`': 'Clojure-style map literal',
  '`:70`': 'Clojure-style anonymous function',
  '`src/scheme/ast.ts:231`': 'Clojure-style anonymous function',
  '`src/scheme/anon-tokens.ts:4`': 'Clojure-style anonymous function',
  // Scamper's own
  '`src/lib/index.ts:41-51`': 'js-var is the FFI root primitive',
  '`:83`': "addExports(['js-var'])",
  '`src/lib/music.scm:1`': 'Euterpea',
}

describe('every citation in docs/DIFFERENCES.md points somewhere real', () => {
  test('the document still has the citations it is made of', () => {
    // A guard on the parser above, not on the document: if the code-span shape
    // ever changes, every other test here would pass vacuously.
    expect(citations.length).toBeGreaterThan(100)
    expect(numbered.length).toBeGreaterThan(20)
  })

  test('every cited path exists', () => {
    for (const c of citations) {
      expect(
        existsSync(resolve(ROOT, c.path)),
        `${DOC}:${c.docLine.toString()} cites ${c.text}, but ${c.path} does not exist`,
      ).toBe(true)
    }
  })

  test('every cited line is inside the file it cites', () => {
    for (const c of numbered) {
      const { lo, hi } = c.lines
      const length = linesOf(c.path).length
      const where = `${DOC}:${c.docLine.toString()} cites ${c.text}`
      expect(lo, `${where}, but line numbers start at 1`).toBeGreaterThan(0)
      expect(hi, `${where}, but the range runs backwards`).toBeGreaterThanOrEqual(lo)
      expect(
        hi,
        `${where}, but ${c.path} has only ${length.toString()} lines`,
      ).toBeLessThanOrEqual(length)
    }
  })
})

describe('a cited line still holds what the sentence says it holds', () => {
  test('every numbered citation has an anchor recorded for it', () => {
    const missing = numbered.map((c) => c.text).filter((t) => ANCHORS[t] === undefined)
    expect(
      [...new Set(missing)],
      'a numbered citation with no entry in ANCHORS cannot be checked -- add one, ' +
        'or cite the path alone if the sentence already names what to search for',
    ).toEqual([])
  })

  test('each anchor appears within the lines cited for it', () => {
    for (const c of numbered) {
      const anchor = ANCHORS[c.text]
      if (anchor === undefined) continue // reported by the test above
      const { lo, hi } = c.lines
      const cited = linesOf(c.path).slice(lo - 1, hi).join('\n')
      expect(
        cited.includes(anchor),
        `${DOC}:${c.docLine.toString()} cites ${c.text} for "${anchor}", ` +
          `which is no longer in ${c.path}:${lo.toString()}-${hi.toString()}`,
      ).toBe(true)
    }
  })

  test('no anchor is recorded for a citation the document no longer makes', () => {
    const cited = new Set(numbered.map((c) => c.text))
    const stale = Object.keys(ANCHORS).filter((t) => !cited.has(t))
    expect(stale, 'ANCHORS has entries for citations that are gone').toEqual([])
  })
})

describe('the two files that move constantly are cited by path alone', () => {
  test('no line number is given for the prelude or the standard library', () => {
    const offenders = numbered
      // The tail-recursive fold helpers are the exception: nothing in that
      // sentence names a binding, so the line is the only way to find them.
      .filter((c) => c.text !== '`src/lib/prelude.scm:878-903`')
      .filter((c) => HOT_FILES.includes(c.path))
      .map((c) => `${DOC}:${c.docLine.toString()} ${c.text}`)
    expect(
      offenders,
      'cite these by path and name the binding in the sentence -- a line ' +
        'number into either file is wrong by the next pull request',
    ).toEqual([])
  })

  test('both files are still cited, so the rule has something to hold', () => {
    for (const path of HOT_FILES) {
      expect(
        citations.some((c) => c.path === path),
        `${DOC} no longer cites ${path}`,
      ).toBe(true)
    }
  })
})

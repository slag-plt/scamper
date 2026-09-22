import { readFileSync } from 'fs'
import { resolve } from 'path'
import { EditorSelection } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { describe, expect, test } from 'vitest'
import { mkFreshEditorState } from '../../src/app/web/codemirror/codemirror'
import { modeFor } from '../../src/app/web/codemirror/modes'
import { initialize } from '../../src/scamper'

// #436: a student selected the second of two identical calls and watched the
// *first* one light up instead. Selecting text appeared to highlight the wrong
// occurrence, and a multi-line selection lost exactly one of its lines.
//
// One cause, and it is a layering one. drawSelection() paints the selection
// into .cm-selectionLayer, which sits *behind* .cm-content; highlightActiveLine
// puts a background on the .cm-line holding the caret, which is *in* it. So
// while --editor-active-line was opaque it painted over the selection on that
// one line. What stayed visible was highlightSelectionMatches' work -- inline
// marks on the *other* occurrences, drawn above the line's background -- and
// those look exactly like a selection landing in the wrong place.
//
// CodeMirror's own theme uses #cceeff44 / #99eeff33 for this; both carry alpha,
// for precisely this reason. Scamper's palette had dropped it.

await initialize()
// Importing scamper.ts kicks off its renderer registration as a fire-and-forget
// module-load side effect; a dynamic import landing after teardown is reported
// as an unhandled error. Settle it here.
await import('../../src/app/web/renderers.js')

const NEEDLE = '(string-length str)'
const DOC = [
  '(define string-split-n',
  '  (lambda (str n)',
  '    (cond',
  '      [(<= (string-length str) n) (list str)]',
  '      [else (cons (substring str 0 n) (string-length str))])))',
].join('\n')

/** A file editor with the *second* occurrence of NEEDLE selected. */
function mountWithSecondOccurrenceSelected(): EditorView {
  const parent = document.createElement('div')
  document.body.appendChild(parent)
  const view = new EditorView({
    state: mkFreshEditorState(DOC, {
      dirtyAction: () => {
        /* nothing is saved here */
      },
      isReadOnly: false,
      mode: modeFor('strings.scm'),
    }),
    parent,
  })
  const second = DOC.indexOf(NEEDLE, DOC.indexOf(NEEDLE) + 1)
  view.dispatch({
    selection: EditorSelection.range(second, second + NEEDLE.length),
  })
  return view
}

describe('the arrangement that makes the active line hide a selection', () => {
  test('the selected line is the active line, and its selection is behind it', () => {
    const view = mountWithSecondOccurrenceSelected()
    try {
      const lines = [...view.contentDOM.querySelectorAll('.cm-line')]
      const active = lines.filter((l) => l.classList.contains('cm-activeLine'))
      // The head's line is decorated whether or not the range is empty, so the
      // line a selection ends on always carries the active-line background.
      expect(active.map((l) => lines.indexOf(l) + 1)).toEqual([5])

      // ...and the selection on it is drawn in a layer behind .cm-content,
      // which is what an opaque background on that line covers up.
      const layer = view.scrollDOM.querySelector<HTMLElement>('.cm-selectionLayer')
      expect(layer).not.toBeNull()
      expect(Number(layer?.style.zIndex)).toBeLessThan(0)
    } finally {
      view.destroy()
    }
  })

  test('nothing above the line background marks the selected occurrence', () => {
    const view = mountWithSecondOccurrenceSelected()
    try {
      // highlightSelectionMatches skips the range that *is* the selection, so
      // the inline marks -- the one thing here drawn above the line background
      // -- land only on the other occurrence. Hide the selection and the
      // remaining highlight is on the wrong one, which is what #436 reported.
      const marks = [...view.contentDOM.querySelectorAll('.cm-selectionMatch')]
      expect(marks.map((m) => m.textContent)).toEqual([NEEDLE])
      // ...and it is on line 4, the occurrence the student did *not* select.
      const lines = [...view.contentDOM.querySelectorAll('.cm-line')]
      expect(marks[0].closest('.cm-line')).toBe(lines[3])
    } finally {
      view.destroy()
    }
  })

  test('the active line is painted from --editor-active-line', () => {
    const view = mountWithSecondOccurrenceSelected()
    try {
      // Ties the token checked below to what actually paints the line: the
      // theme's rules are injected as real CSS, so this fails if the rule is
      // ever pointed at some other colour.
      const css = [...document.head.querySelectorAll('style')]
        .map((s) => s.textContent)
        .join('\n')
      expect(css).toContain(
        '.cm-activeLine {background-color: var(--editor-active-line);}',
      )
    } finally {
      view.destroy()
    }
  })
})

const THEME_CSS = readFileSync(
  resolve(__dirname, '../../public/css/theme.css'),
  'utf-8',
)

/** The value of a custom property declared in theme.css. */
function tokenValue(name: string): string {
  const decl = new RegExp(`${name}:([^;]*);`).exec(THEME_CSS)
  if (decl === null) throw new Error(`${name} is not declared in theme.css`)
  return decl[1].trim().replace(/\s+/g, ' ')
}

/** The light and dark halves of a `light-dark(a, b)` value. */
function lightDarkHalves(value: string): string[] {
  const inner = /^light-dark\((.*)\)$/.exec(value)
  if (inner === null) throw new Error(`not a light-dark() value: ${value}`)
  const halves: string[] = []
  let depth = 0
  let start = 0
  const text = inner[1]
  for (let i = 0; i < text.length; i++) {
    if (text[i] === '(') depth++
    else if (text[i] === ')') depth--
    else if (text[i] === ',' && depth === 0) {
      halves.push(text.slice(start, i).trim())
      start = i + 1
    }
  }
  halves.push(text.slice(start).trim())
  return halves
}

/**
 * The alpha a colour declares, as a fraction.
 *
 * Understands the two spellings the palette uses -- `oklch(L C H / A)` and
 * `rgba(r, g, b, a)`. Anything else reads as fully opaque, so a colour this
 * cannot make sense of fails the check rather than passing it by default.
 */
function alphaOf(color: string): number {
  const modern = /\/\s*([\d.]+)(%?)\s*\)\s*$/.exec(color)
  const legacy = /,\s*([\d.]+)(%?)\s*\)\s*$/.exec(color)
  const match = modern ?? (color.startsWith('rgba(') ? legacy : null)
  if (match === null) return 1
  return Number(match[1]) / (match[2] === '%' ? 100 : 1)
}

describe('--editor-active-line stays translucent', () => {
  // The bug is not the colour, it is the opacity, and the arithmetic is exact:
  // compositing is linear, so at alpha a the selection on the active line keeps
  // exactly (1 - a) of the contrast it has anywhere else. a = 1 keeps none of
  // it, which is #436; the bound below is what keeps most of it. The lower
  // bound is the other half of the job -- the line must still be tinted enough
  // to show where the caret is.
  const MOST_OF_THE_SELECTION_SURVIVES = 0.5
  const THE_LINE_IS_STILL_TINTED = 0.1

  for (const [theme, index] of [
    ['light', 0],
    ['dark', 1],
  ] as const) {
    test(`the ${theme} half lets the selection show through`, () => {
      const half = lightDarkHalves(tokenValue('--editor-active-line'))[index]
      const alpha = alphaOf(half)
      expect(alpha, `${half} is opaque`).toBeLessThanOrEqual(
        MOST_OF_THE_SELECTION_SURVIVES,
      )
      expect(alpha, `${half} is invisible`).toBeGreaterThanOrEqual(
        THE_LINE_IS_STILL_TINTED,
      )
    })
  }
})

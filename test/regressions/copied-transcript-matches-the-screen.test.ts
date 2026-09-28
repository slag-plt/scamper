import { describe, expect, test } from 'vitest'
import { transcriptText } from '../../src/app/web/repl-transcript'
import { mkList } from '../../src/lpm/util'

// https://github.com/slag-plt/scamper/issues/635
//
// #612 made a void value render as nothing in the web output, replacing the word
// `void`. The Copy button renders the same values through TextRenderer, which
// has no way to draw nothing -- so a student who ran `(vector-set! v 0 5)`, saw
// no output, and copied the transcript got a line reading `void` that had never
// been on the screen.
//
// The CLI keeps printing `void`, deliberately: there is no hidden element to be
// had in a terminal, and a Gradescope autograder reads that text. The
// divergence that remains is between the editor and the command line, not
// between what a student sees and what they paste.

/** Void, as a statement that mutates something produces it. */
const VOID = undefined

describe('the Copy button writes what was on the screen', () => {
  test('a statement that printed nothing contributes only its source', () => {
    expect(
      transcriptText([{ source: '(vector-set! v 0 5)', values: [VOID] }]),
    ).toBe('(vector-set! v 0 5)')
  })

  test('a void between two printed values leaves no blank line', () => {
    expect(
      transcriptText([
        { source: '(+ 1 2)', values: [3] },
        { source: '(vector-set! v 0 5)', values: [VOID] },
        { source: '(vector-ref v 0)', values: [5] },
      ]),
    ).toBe('(+ 1 2)\n3\n(vector-set! v 0 5)\n(vector-ref v 0)\n5')
  })

  test('an entry that printed only voids is not dropped, just quiet', () => {
    // The source line still stands: it is what was typed, and a transcript
    // whose statements went missing would be worse than one with a stray word.
    expect(
      transcriptText([{ source: '(begin-mutating)', values: [VOID, VOID] }]),
    ).toBe('(begin-mutating)')
  })

  // The skip is deliberately top-level only. #612 weighed a void nested in an
  // aggregate and left it spelled, and both renderers still agree to disagree
  // there: the Vue one draws an empty element between the elements around it,
  // where TextRenderer writes the word. Recursing here would decide that
  // question in passing, which is not this issue's to decide.
  test('a void inside a list is still spelled out', () => {
    expect(
      transcriptText([
        { source: '(list 1 (vector-set! v 0 5) 2)', values: [mkList(1, VOID, 2)] },
      ]),
    ).toBe('(list 1 (vector-set! v 0 5) 2)\n(list 1 void 2)')
  })
})

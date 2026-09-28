import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import {
  ensureSyntaxTree,
  getIndentation,
  IndentContext,
  indentRange,
  syntaxTree,
  syntaxTreeAvailable,
} from '@codemirror/language'
import { ScamperSupport } from '../../src/app/web/codemirror/extensions/language'
import { formPathAt } from '../../src/app/web/codemirror/enclosing-form'
import { mkParsedState } from '../apps/web/parsed-state'

// https://github.com/slag-plt/scamper/issues/636
//
// Two of the flaky tests #636 reports -- indentation.test.ts leaving a buffer
// flush-left, and enclosing-form.test.ts handing back an empty breadcrumb --
// have one cause, and it is not what their own code said it was.
//
// `EditorState.create` parses under a 20ms budget of CodeMirror's own
// (`Work.Apply`) and over the first 3000 characters only (`Work.InitViewport`),
// keeping whatever tree that bought. Both files called `ensureSyntaxTree`
// afterwards to finish the job. It does finish the parse -- but it advances the
// state field's `ParseContext` without republishing it, and `LanguageState`
// captured `context.tree` at construction, so `syntaxTree(state)` still returns
// the short one. `getIndentation` answers `null` for a position its tree does
// not reach, and `formPathAt` answers `[]`.
//
// On an idle machine a test's 20-character document fits inside the 20ms, so
// step one already produced a complete tree and the mistake was invisible.
// Under contention the budget ran out and the tests failed with a symptom that
// read like a broken indent rule.
//
// Nothing here is probabilistic: passing the 3000-character initial viewport
// puts a fast machine in exactly the position a loaded one reaches with a short
// document, so the bug and its fix are both deterministic.

/** A well-formed Scamper document of at least `chars` characters. */
function longDocument(chars: number): string {
  const lines: string[] = []
  for (let i = 0; lines.join('\n').length < chars; i++) {
    lines.push(`(define x${i.toString()}\n(f ${i.toString()} ${i.toString()}))`)
  }
  return lines.join('\n')
}

/** The document, flush-left, comfortably past the initial viewport. */
const DOC = longDocument(6_000)

/** What both files used to do: create, then force the parse, and read on. */
function mkStateTheOldWay(): EditorState {
  const state = EditorState.create({ doc: DOC, extensions: [ScamperSupport()] })
  ensureSyntaxTree(state, state.doc.length, 5_000)
  return state
}

describe('forcing a parse has to reach the state the tests read', () => {
  test("the initial parse stops at CodeMirror's own viewport", () => {
    const state = EditorState.create({ doc: DOC, extensions: [ScamperSupport()] })
    expect(state.doc.length).toBeGreaterThan(6_000)
    expect(syntaxTree(state).length).toBeLessThan(state.doc.length)
  })

  test('ensureSyntaxTree finishes the parse but does not publish it', () => {
    const state = mkStateTheOldWay()
    // Its return value is the complete tree...
    const forced = ensureSyntaxTree(state, state.doc.length, 5_000)
    expect(forced?.length).toBe(state.doc.length)
    // ...while the tree every reader goes through is still the short one.
    expect(syntaxTree(state).length).toBeLessThan(state.doc.length)
  })

  test('syntaxTreeAvailable answers about the context, not the field', () => {
    // Which is why it cannot stand in as the check: it says yes while the tree
    // the tests read is still short.
    const state = mkStateTheOldWay()
    expect(syntaxTreeAvailable(state, state.doc.length)).toBe(true)
    expect(syntaxTree(state).length).toBeLessThan(state.doc.length)
  })

  test('mkParsedState publishes it', () => {
    const state = mkParsedState({ doc: DOC, extensions: [ScamperSupport()] })
    expect(syntaxTree(state).length).toBe(state.doc.length)
  })
})

describe('what the two flaky tests were actually seeing', () => {
  /** Every body line -- the `(f ...)` half of each definition -- indented? */
  function everyBodyIndented(text: string): boolean {
    return text
      .split('\n')
      .filter((l) => l.trimStart().startsWith('(f '))
      .every((l) => l.startsWith('  ('))
  }

  /** `state` re-indented whole, as Ctrl-I does it. */
  function reindented(state: EditorState): string {
    return state
      .update({ changes: indentRange(state, 0, state.doc.length) })
      .state.doc.toString()
  }

  // indentation.test.ts: a buffer left as it was found. `indentRange` walks the
  // lines and stops at the first one `getIndentation` has no answer for, so a
  // tree that ends at 3005 characters indents up to there and abandons the rest
  // -- and for the three-line document that test uses, "the rest" is all of it.
  test('a partial tree leaves every line past it flush-left', () => {
    expect(everyBodyIndented(reindented(mkStateTheOldWay()))).toBe(false)
    expect(
      everyBodyIndented(
        reindented(mkParsedState({ doc: DOC, extensions: [ScamperSupport()] })),
      ),
    ).toBe(true)
  })

  test('a partial tree gives no indent for a line past it', () => {
    // The start of a body line well past the initial viewport, which the rules
    // put at two spaces.
    const at = DOC.lastIndexOf('\n', DOC.indexOf('(f ', 4_000)) + 1
    const indentAt = (state: EditorState) =>
      getIndentation(new IndentContext(state), at)

    expect(indentAt(mkStateTheOldWay())).toBeNull()
    expect(
      indentAt(mkParsedState({ doc: DOC, extensions: [ScamperSupport()] })),
    ).toBe(2)
  })

  // enclosing-form.test.ts: an empty breadcrumb for a cursor in a real form.
  test('a partial tree gives an empty form path past it', () => {
    const at = DOC.indexOf('(f ', 4_000) + 3
    expect(formPathAt(syntaxTree(mkStateTheOldWay()), at)).toEqual([])
    expect(
      formPathAt(
        syntaxTree(mkParsedState({ doc: DOC, extensions: [ScamperSupport()] })),
        at,
      ),
    ).not.toEqual([])
  })
})

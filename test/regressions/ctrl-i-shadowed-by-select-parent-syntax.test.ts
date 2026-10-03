import { describe, expect, test } from 'vitest'
import { EditorSelection, type EditorState } from '@codemirror/state'
import { EditorView, keymap } from '@codemirror/view'
import { defaultKeymap } from '@codemirror/commands'
import { ensureSyntaxTree, syntaxTree } from '@codemirror/language'
import { mkFreshEditorState } from '../../src/app/web/codemirror/codemirror'
import { scamperMode } from '../../src/app/web/codemirror/modes'
import { reindentScamperDocument } from '../../src/app/web/codemirror/extensions/indentation'
import { initialize } from '../../src/scamper'

// Regression test for https://github.com/slag-plt/scamper/issues/686.
//
// Ctrl-I re-indents the buffer (docs/formatting.md), and
// IndentationExtension binds it. CodeMirror's defaultKeymap binds `Mod-i` to
// selectParentSyntax, and `Mod` is Ctrl everywhere but macOS -- so on Linux and
// Windows the two bindings are the same chord.
//
// A keymap registered *later* does not win. buildKeymap collects every command
// bound to a chord into one list in facet order, and runHandlers runs them in
// that order until one returns true. The base keymap is installed first in
// mkExtensions, so selectParentSyntax -- which returns true whenever it widened
// the selection -- answers Ctrl-I and the re-indent never runs. The fix is
// precedence, not position in the array.
//
// jsdom reports a non-mac platform, which is what makes the collision
// reproducible here: on macOS Mod-i is Cmd-I and the chords are distinct.

await initialize()
// scamper.ts registers its renderers as a fire-and-forget module-load import;
// settle it here so a late resolution is not reported after teardown.
await import('../../src/app/web/renderers.js')

const FLAT = ['(define area', '(lambda (r)', '(* 3.14 r r)))'].join('\n')
const INDENTED = [
  '(define area',
  '  (lambda (r)',
  '    (* 3.14 r r)))',
].join('\n')

/** How long to let the parser finish, as in test/apps/web/parsed-state.ts. */
const PARSE_BUDGET_MS = 5_000

/**
 * The file editor's state, with the parts a test has no use for stubbed.
 *
 * The parse is forced and republished -- the three steps
 * test/apps/web/parsed-state.ts documents. `mkFreshEditorState` parses under
 * CodeMirror's own 20ms `Work.Apply` budget and keeps whatever partial tree
 * that bought, and `indentRange` leaves every line the tree does not reach
 * flush-left, so on a loaded machine the re-indent below would fail with the
 * fix in place (#636). The exposure is false-red only, never false-green: with
 * no tree, `selectParentSyntax` has nothing to widen against either.
 * `mkParsedState` builds its own state from a config, so it cannot wrap
 * `mkFreshEditorState`; these are its steps inline.
 */
function fileState(doc: string): EditorState {
  const created = mkFreshEditorState(doc, {
    dirtyAction: () => {
      /* nothing here tracks unsaved changes */
    },
    isReadOnly: false,
    mode: scamperMode,
  })
  expect(
    ensureSyntaxTree(created, created.doc.length, PARSE_BUDGET_MS),
    'the parser did not reach the end of the document within its budget',
  ).not.toBeNull()
  // An empty transaction rebuilds the field from the context finished above,
  // which is what makes the complete tree visible to syntaxTree.
  const state = created.update({}).state
  expect(
    syntaxTree(state).length,
    'the finished parse did not reach the state the commands read',
  ).toBeGreaterThanOrEqual(state.doc.length)
  return state
}

/** A mounted file editor, since a chord is only delivered to a view. */
function mount(doc: string, selection: EditorSelection): EditorView {
  const parent = document.createElement('div')
  document.body.appendChild(parent)
  const view = new EditorView({ state: fileState(doc), parent })
  view.dispatch({ selection })
  return view
}

/** Presses Ctrl-I, as a person on Linux or Windows does. */
function pressCtrlI(view: EditorView): void {
  view.contentDOM.dispatchEvent(
    new KeyboardEvent('keydown', {
      key: 'i',
      code: 'KeyI',
      ctrlKey: true,
      bubbles: true,
      cancelable: true,
    }),
  )
}

/** Where a student's caret or selection plausibly is when they press Ctrl-I. */
const places: [string, EditorSelection][] = [
  [
    'the caret inside an identifier',
    EditorSelection.single(FLAT.lastIndexOf('r')),
  ],
  [
    'the body line selected',
    EditorSelection.single(
      FLAT.lastIndexOf('(* 3.14'),
      FLAT.lastIndexOf('(* 3.14') + '(* 3.14 r r)'.length,
    ),
  ],
]

describe('#686: Ctrl-I re-indents rather than expanding the selection', () => {
  for (const [where, selection] of places) {
    test(`with ${where}, the document is re-indented`, () => {
      const view = mount(FLAT, selection)
      try {
        pressCtrlI(view)
        expect(view.state.doc.toString()).toBe(INDENTED)
      } finally {
        view.destroy()
        document.body.innerHTML = ''
      }
    })

    test(`with ${where}, the selection is not widened instead`, () => {
      const view = mount(FLAT, selection)
      try {
        const before = view.state.selection.main
        pressCtrlI(view)
        const after = view.state.selection.main
        // The re-indent maps the selection through its own changes, so the
        // offsets move; what must not happen is the range *growing*, which is
        // selectParentSyntax having answered the chord.
        expect(after.to - after.from).toBe(before.to - before.from)
      } finally {
        view.destroy()
        document.body.innerHTML = ''
      }
    })
  }
})

describe('#686: the chord collision that makes precedence necessary', () => {
  /** Normalized the way buildKeymap does on a non-mac platform. */
  const chord = (key: string | undefined) => key?.replace('Mod-', 'Ctrl-')

  test('the base keymap still binds the same chord', () => {
    // Without this the tests above could pass for the wrong reason, were
    // upstream ever to drop the binding.
    const clash = defaultKeymap.filter((b) => chord(b.key) === 'Ctrl-i')
    expect(clash.map((b) => b.run?.name)).toEqual(['selectParentSyntax'])
  })

  test('the re-indent is the first command the chord reaches', () => {
    // runHandlers consults the keymap facet in order and stops at the first
    // command returning true, so being first is the whole of winning.
    const bound = fileState(FLAT)
      .facet(keymap)
      .flat()
      .filter((b) => chord(b.key) === 'Ctrl-i')
      .map((b) => b.run)
    expect(bound[0]).toBe(reindentScamperDocument)
  })
})

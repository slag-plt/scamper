import { expect } from 'vitest'
import { EditorState, type EditorStateConfig } from '@codemirror/state'
import { ensureSyntaxTree, syntaxTree } from '@codemirror/language'

/**
 * How long to let the parser finish. Generous: it costs nothing when the parse
 * is prompt, and stays well inside the suite's test budget.
 */
const PARSE_BUDGET_MS = 5_000

/**
 * An editor state whose syntax tree covers the whole document.
 *
 * Three steps, all of them needed:
 *
 * 1. `EditorState.create` parses under a 20ms budget of CodeMirror's own
 *    (`Work.Apply`) and over the first 3000 characters only
 *    (`Work.InitViewport`), keeping whatever tree that bought.
 * 2. `ensureSyntaxTree` finishes the parse -- but it advances the state field's
 *    `ParseContext` *without republishing it*. `LanguageState` captured
 *    `context.tree` when it was constructed, and `syntaxTree(state)` returns
 *    that captured tree, so the complete one is only the return value here.
 * 3. Any transaction rebuilds the field from its context, so an empty one is
 *    what makes the finished parse visible to `syntaxTree`.
 *
 * Step 2 alone looks like it should be enough, and on an idle machine it appears
 * to be: a short document fits inside the 20ms, so step 1 already produced a
 * complete tree and nothing else matters. Under contention the budget runs out
 * mid-document, and `getIndentation` -- which reads `syntaxTree(state)` and
 * returns `null` for a position the tree does not reach -- then answers `null`
 * for every line, leaving a whole buffer flush-left. `formPathAt` answers `[]`
 * the same way. Both were reported as flaky tests on a loaded CI runner and
 * nowhere else (#636).
 *
 * `syntaxTreeAvailable` is no use as a check here: it asks the *context*, which
 * is done, rather than the field, which is stale.
 */
export function mkParsedState(config: EditorStateConfig): EditorState {
  const created = EditorState.create(config)
  expect(
    ensureSyntaxTree(created, created.doc.length, PARSE_BUDGET_MS),
    'the parser did not reach the end of the document within its budget',
  ).not.toBeNull()
  // An empty transaction: no changes, no selection, just a new state built from
  // the context the line above finished.
  const state = created.update({}).state
  expect(
    syntaxTree(state).length,
    'the finished parse did not reach the state the tests read',
  ).toBeGreaterThanOrEqual(state.doc.length)
  return state
}

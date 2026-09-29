import {
  EditorState,
  Extension,
  Line,
  StateCommand,
  TransactionSpec,
} from '@codemirror/state'
import { keymap } from '@codemirror/view'
import { lineUncomment, type CommentTokens } from '@codemirror/commands'

/**
 * Commenting in two directions rather than one toggle (issue #662).
 *
 * Scamper gives comment *depth* meaning: `;` annotates a line of code, `;;`
 * sits between lines, and `;;; ` opens a docstring (see
 * src/scheme/docstring/docstring.ts). A single toggling chord cannot express
 * that -- it can never deepen a comment, and pressing it on a docstring
 * silently demotes it -- so adding and removing a layer are separate commands.
 *
 * Removing one is `lineUncomment` unchanged; the library already does exactly
 * that. Adding one had to be written here, because the library's `lineComment`
 * is a no-op on a line that is already commented.
 */

/** The line-comment marker of the language at `pos`, if it has one. */
function markerAt(state: EditorState, pos: number): string | undefined {
  const data = state.languageDataAt<CommentTokens>('commentTokens', pos, 1)
  return data.length > 0 ? data[0].line : undefined
}

/** One line a marker may go on, with where its text starts. */
interface Target {
  line: Line
  /** Length of the line's leading whitespace. */
  indent: number
  /** Whether the line is whitespace to its end. */
  blank: boolean
}

/**
 * The edit that adds one marker to every line the selection touches, or null if
 * there is nothing to add.
 *
 * The geometry matches `lineUncomment`'s, since the two have to be inverses:
 * a line is considered once however many ranges cover it, and within a range
 * every marker lands at the *shallowest* indentation, so a block of mixed
 * indents keeps its shape. A blank line is left as a gap unless it is the whole
 * selection, in which case it is where someone is starting a comment.
 */
function addition(state: EditorState): TransactionSpec | null {
  const changes: { from: number; insert: string }[] = []
  let prevLine = -1
  for (const range of state.selection.ranges) {
    const marker = markerAt(state, range.from)
    // No line comment in this language -- a .txt or .csv buffer (#385).
    if (marker === undefined) continue
    const targets: Target[] = []
    let minIndent = Infinity
    for (let pos = range.from; pos <= range.to; ) {
      const line = state.doc.lineAt(pos)
      if (line.from > prevLine && (range.empty || range.to > line.from)) {
        prevLine = line.from
        const indent = line.text.length - line.text.trimStart().length
        const blank = indent === line.text.length
        if (!blank) minIndent = Math.min(minIndent, indent)
        targets.push({ line, indent, blank })
      }
      pos = line.to + 1
    }
    const single = targets.length === 1
    for (const { line, indent, blank } of targets) {
      if (blank && !single) continue
      const at = blank ? indent : minIndent
      // Exactly one marker per line, unconditionally: differences in depth
      // across the selection are preserved rather than levelled to the deepest
      // line, because levelling could not be undone.
      //
      // A space follows the marker only where the insertion point does not
      // already hold one. That is what makes the depths compose --
      // `foo` -> `; foo` -> `;; foo` -> `;;; foo`, which is byte for byte the
      // docstring prefix -- where always inserting "; " would give `; ; foo`
      // and never form one.
      const butted = line.text.slice(at, at + marker.length) === marker
      changes.push({
        from: line.from + at,
        insert: butted ? marker : marker + ' ',
      })
    }
  }
  if (changes.length === 0) return null
  const changeSet = state.changes(changes)
  // Map the selection forward so the caret stays with its text rather than
  // ending up in front of the marker just inserted.
  return { changes: changeSet, selection: state.selection.map(changeSet, 1) }
}

/**
 * Ctrl-; -- deepens every selected line's comment by one semicolon, commenting
 * out a line that was code.
 */
export const addCommentLayer: StateCommand = ({ state, dispatch }) => {
  if (state.readOnly) return false
  const spec = addition(state)
  if (spec === null) return false
  dispatch(state.update(spec))
  return true
}

/**
 * Ctrl-Shift-; -- takes one semicolon off every selected line that has one,
 * which is `lineUncomment` exactly: it strips one marker and the single space
 * after it, leaves an uncommented line and a trailing comment alone, and
 * declines when no selected line is commented.
 */
export const removeCommentLayer: StateCommand = lineUncomment

/**
 * The pair, on one binding: `shift` is CodeMirror's same-key variant, as the
 * Tab binding in codemirror.ts uses. `Mod` is Cmd on macOS and Ctrl elsewhere.
 *
 * Installed at top level rather than only for a Scamper file: both commands ask
 * the language for its comment marker and decline when there is none, so a
 * plain text file needs no special case. Ctrl-/ is left as CodeMirror's own
 * toggle, and nothing in any installed keymap binds `;`, so this needs no
 * precedence of its own.
 */
export const CommentExtension: Extension = keymap.of([
  { key: 'Mod-;', run: addCommentLayer, shift: removeCommentLayer },
])

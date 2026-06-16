// Adapted from CodeMirror's basicSetup:
// https://github.com/codemirror/basic-setup/blob/main/src/codemirror.ts

import {
  keymap, highlightSpecialChars, drawSelection, highlightActiveLine, dropCursor,
  rectangularSelection, crosshairCursor,
  lineNumbers, highlightActiveLineGutter,
  EditorView
} from "@codemirror/view"
import { Extension, EditorState } from "@codemirror/state"
import {
  defaultHighlightStyle, syntaxHighlighting, indentOnInput, bracketMatching,
  foldGutter, foldKeymap
} from "@codemirror/language"
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands"
import { searchKeymap, highlightSelectionMatches } from "@codemirror/search"
import { autocompletion, completionKeymap, closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete"
import { lintKeymap } from "@codemirror/lint"
import { ScamperSupport } from "../codemirror/language"
import makeScamperLinter from "../codemirror/linter"
import { format } from "prettier"
import ScamperPlugin from "../prettier/prettier-plugin-scamper"
import { diff } from "@codemirror/merge"

const noLoadedFileText = '; Create and/or load a file from the left-hand sidebar!'

export type EditorStateConfig = {
  output?: HTMLElement
  dirtyAction: () => void
  isReadOnly: boolean
}

const prettierExtension: Extension = keymap.of([
  {
    // Prefer Mod on macOS (Cmd) and Ctrl elsewhere
    key: "Mod-Shift-i",
    run: (view) => {
      const doc = view.state.doc
      const sel = view.state.selection.main
      const oldAnchor = sel.anchor
      const oldHead = sel.head
      const oldText = doc.toString()

      void format(oldText, {
        parser: "scamper-scheme",
        plugins: [ScamperPlugin],
      }).then((formatted) => {
        // Normalize line endings to match CodeMirror's internal model
        const newText = formatted.replace(/\r\n?/g, "\n")
        if (newText === oldText) { return }

        // Compute a minimal edit script so selection positions can be mapped.
        const chunks = diff(oldText, newText)
        const changes = chunks.map((c) => ({
          from: c.fromA,
          to: c.toA,
          insert: newText.slice(c.fromB, c.toB),
        }))
        const changeSet = view.state.changes(changes)
        const newAnchor = changeSet.mapPos(oldAnchor, 1)
        const newHead = changeSet.mapPos(oldHead, 1)

        view.dispatch({
          changes,
          selection: { anchor: newAnchor, head: newHead },
          scrollIntoView: true,
        })
      }).catch(() => {
        // If formatting fails, do nothing; keep editor state unchanged.
      })

      return true
    }
  },
])

function mkExtensions(config: EditorStateConfig): Extension {
  return [
    // basicSetup
    lineNumbers(),
    highlightActiveLineGutter(),
    highlightSpecialChars(),
    history(),
    foldGutter(),
    drawSelection(),
    dropCursor(),
    EditorState.allowMultipleSelections.of(true),
    indentOnInput(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true}),
    bracketMatching(),
    closeBrackets(),
    autocompletion(),
    rectangularSelection(),
    crosshairCursor(),
    highlightActiveLine(),
    highlightSelectionMatches(),
    keymap.of([
      ...closeBracketsKeymap,
      ...defaultKeymap,
      ...searchKeymap,
      ...historyKeymap,
      ...foldKeymap,
      ...completionKeymap,
      ...lintKeymap
    ]),
    // Scamper-specific extensions,
    EditorState.readOnly.of(config.isReadOnly),
    prettierExtension,
    keymap.of([
      indentWithTab,
      {
        key: "'",
        run: (view) => {
          const { from, to } = view.state.selection.main
          view.dispatch({
            changes: { from, to, insert: "'" },
            selection: { anchor: from + 1 }
          })
          return true
        }
      }
    ]),
    ScamperSupport(),
    makeScamperLinter(config.output),
    EditorView.updateListener.of((update) => {
      if (update.docChanged) { config.dirtyAction() }
    })
  ]
}

export function mkFreshEditorState (doc: string, config: EditorStateConfig): EditorState {
  return EditorState.create({
    doc, extensions: mkExtensions(config)
  })
}

export function mkNoFileEditorState (): EditorState {
  return EditorState.create({
    doc: noLoadedFileText,
    extensions: mkExtensions({
      dirtyAction: () => { },
      isReadOnly: true
    })
  })
}
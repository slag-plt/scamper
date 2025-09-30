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
import { defaultKeymap, history, historyKeymap, indentSelection, indentWithTab } from "@codemirror/commands"
import { searchKeymap, highlightSelectionMatches } from "@codemirror/search"
import { autocompletion, completionKeymap, closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete"
import { lintKeymap } from "@codemirror/lint"
import { ScamperSupport } from "../codemirror/language"
import makeScamperLinter from "../codemirror/linter"

const noLoadedFileText = '; Create and/or load a file from the left-hand sidebar!'

export type EditorStateConfig = {
  output: HTMLElement
  dirtyAction: () => void
  isReadOnly: boolean
}

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
    keymap.of([
      indentWithTab,
      {
        key: "Ctrl-Shift-i",
        run: (view) => {
          const doc = view.state.doc
          const sel = view.state.selection.main
          const line = doc.lineAt(sel.head)
          const visualColumn = sel.head - line.from
          view.dispatch({
            selection: { anchor: 0, head: doc.length }
          })
          const success = indentSelection(view)
          const updatedLine = view.state.doc.line(line.number)
          const nonWhitespacePrefix = updatedLine.text.match(/^\s*/)?.[0].length || 0
          const newHead = Math.min(updatedLine.from + nonWhitespacePrefix + visualColumn, updatedLine.to)
          view.dispatch({
            selection: { anchor: newHead },
            scrollIntoView: true
          })
          return success
        }
      },
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

export function mkNoFileEditorState (output: HTMLElement): EditorState {
  return EditorState.create({
    doc: noLoadedFileText,
    extensions: mkExtensions({
      output,
      dirtyAction: () => { },
      isReadOnly: true
    })
  })
}
// Adapted from CodeMirror's basicSetup:
// https://github.com/codemirror/basic-setup/blob/main/src/codemirror.ts

import {
  crosshairCursor,
  drawSelection,
  dropCursor,
  EditorView,
  highlightActiveLine,
  highlightActiveLineGutter,
  highlightSpecialChars,
  keymap,
  lineNumbers,
  rectangularSelection,
} from "@codemirror/view"
import { EditorState, Extension } from "@codemirror/state"
import {
  bracketMatching,
  defaultHighlightStyle,
  foldGutter,
  foldKeymap,
  indentOnInput,
  syntaxHighlighting,
} from "@codemirror/language"
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
} from "@codemirror/commands"
import { highlightSelectionMatches, searchKeymap } from "@codemirror/search"
import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  completionKeymap,
} from "@codemirror/autocomplete"
import { lintKeymap } from "@codemirror/lint"
import { ScamperSupport } from "../../codemirror/language"
import makeScamperLinter from "../../codemirror/linter"
import { PrettierExtension } from "./extensions/prettier"
import { QueryExtension } from "./extensions/query"

export const noLoadedFileText =
  "; Create and/or load a file from the left-hand sidebar!"

export interface EditorStateConfig {
  output?: HTMLElement
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
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
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
      ...lintKeymap,
    ]),
    // Scamper-specific extensions,
    EditorState.readOnly.of(config.isReadOnly),
    PrettierExtension,
    // TODO: probably extend this out into a separate extension file
    keymap.of([
      indentWithTab,
      {
        key: "'",
        run: (view) => {
          const { from, to } = view.state.selection.main
          view.dispatch({
            changes: { from, to, insert: "'" },
            selection: { anchor: from + 1 },
          })
          return true
        },
      },
    ]),
    ScamperSupport(),
    makeScamperLinter(config.output),
    EditorView.updateListener.of((update) => {
      if (update.docChanged) {
        config.dirtyAction()
      }
    }),
    QueryExtension,
  ]
}

export function mkFreshEditorState(
  doc: string,
  config: EditorStateConfig,
): EditorState {
  return EditorState.create({
    doc,
    extensions: mkExtensions(config),
  })
}

export function mkNoFileEditorState(): EditorState {
  return EditorState.create({
    doc: noLoadedFileText,
    extensions: mkExtensions({
      dirtyAction: () => {
        /* empty */
      },
      isReadOnly: true,
    }),
  })
}

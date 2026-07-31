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
} from '@codemirror/view'
import { Compartment, EditorState, Extension } from '@codemirror/state'
import {
  bracketMatching,
  defaultHighlightStyle,
  foldGutter,
  foldKeymap,
  HighlightStyle,
  indentOnInput,
  syntaxHighlighting,
} from '@codemirror/language'
import { tags as t } from '@lezer/highlight'
import { currentTheme, type Theme } from '../../../theme'
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentWithTab,
} from '@codemirror/commands'
import { highlightSelectionMatches, searchKeymap } from '@codemirror/search'
import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  completionKeymap,
} from '@codemirror/autocomplete'
import { lintGutter, lintKeymap } from '@codemirror/lint'
import { ScamperSupport } from './extensions/language'
import makeScamperLinter from './extensions/linter'
import { PrettierExtension } from './extensions/prettier'
import { QueryExtension } from './extensions/query'
import { enclosingFormPath } from './enclosing-form'

export const noLoadedFileText =
  '; Create and/or load a file from the left-hand sidebar!'

// Editor theme. Light keeps CodeMirror's default light chrome + highlight; dark
// is a GitHub-dark-inspired theme covering the tags assigned in
// extensions/language.ts. The active theme lives in a Compartment so it can be
// swapped live (see CodeMirrorEditor.vue) without rebuilding editor state.
const lightThemeExtension: Extension = syntaxHighlighting(defaultHighlightStyle, {
  fallback: true,
})

const darkHighlightStyle = HighlightStyle.define([
  { tag: t.keyword, color: '#ff7b72' },
  { tag: t.variableName, color: '#e6edf3' },
  { tag: [t.bool, t.null, t.atom], color: '#79c0ff' },
  { tag: t.number, color: '#79c0ff' },
  { tag: [t.string, t.character], color: '#a5d6ff' },
  { tag: [t.lineComment, t.comment], color: '#8b949e', fontStyle: 'italic' },
  { tag: [t.paren, t.squareBracket, t.brace, t.punctuation], color: '#c9d1d9' },
])

const darkEditorTheme = EditorView.theme(
  {
    '&': { color: '#e6edf3', backgroundColor: '#0d1117' },
    '.cm-content': { caretColor: '#e6edf3' },
    '.cm-cursor, .cm-dropCursor': { borderLeftColor: '#e6edf3' },
    '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection':
      { backgroundColor: '#264f78' },
    '.cm-gutters': {
      backgroundColor: '#0d1117',
      color: '#6e7681',
      border: 'none',
    },
    '.cm-activeLine': { backgroundColor: 'rgba(110, 118, 129, 0.1)' },
    '.cm-activeLineGutter': { backgroundColor: 'rgba(110, 118, 129, 0.1)' },
    '.cm-foldPlaceholder': {
      backgroundColor: 'transparent',
      border: 'none',
      color: '#8b949e',
    },
    '.cm-tooltip': {
      backgroundColor: '#161b22',
      border: '1px solid #30363d',
      color: '#e6edf3',
    },
    '.cm-tooltip-autocomplete ul li[aria-selected]': {
      backgroundColor: '#264f78',
      color: '#e6edf3',
    },
  },
  { dark: true },
)

const darkThemeExtension: Extension = [darkEditorTheme, syntaxHighlighting(darkHighlightStyle)]

/** The editor theme+highlight extension for a given app theme. */
export function editorThemeExtension(theme: Theme): Extension {
  return theme === 'dark' ? darkThemeExtension : lightThemeExtension
}

/** Compartment holding the active editor theme, for live reconfiguration. */
export const editorThemeCompartment = new Compartment()

export interface EditorStateConfig {
  output?: HTMLElement
  dirtyAction: () => void
  /** Notified with the enclosing-form breadcrumb whenever the cursor moves. */
  onFormChange?: (path: string[]) => void
  isReadOnly: boolean
}

function mkExtensions(config: EditorStateConfig): Extension {
  // Deduped so cursor movement within the same form doesn't re-notify on every
  // keystroke. Scoped per state (mkExtensions runs once per state creation).
  let lastFormKey: string | null = null
  return [
    // basicSetup
    lineNumbers(),
    highlightActiveLineGutter(),
    highlightSpecialChars(),
    history(),
    foldGutter(),
    // Gutter markers for lint diagnostics -- easier to spot than the inline
    // squiggles alone. Colored by severity and shows the messages on hover.
    lintGutter(),
    drawSelection(),
    dropCursor(),
    EditorState.allowMultipleSelections.of(true),
    indentOnInput(),
    editorThemeCompartment.of(editorThemeExtension(currentTheme.value)),
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
      if (config.onFormChange && (update.selectionSet || update.docChanged)) {
        const path = enclosingFormPath(update.state)
        const key = path.join(' ')
        if (key !== lastFormKey) {
          lastFormKey = key
          config.onFormChange(path)
        }
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

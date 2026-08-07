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
import { unifiedMergeView } from '@codemirror/merge'
import { ScamperSupport } from './extensions/language'
import { PrettierExtension } from './extensions/prettier'
import { QueryExtension } from './extensions/query'
import { scamperLspExtensions } from './lsp'
import {
  cursorStatus,
  dedupeCursorStatus,
  type CursorStatus,
} from './enclosing-form'

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
  dirtyAction: () => void
  /** Notified with the cursor's status whenever the cursor moves or edits. */
  onCursorChange?: (status: CursorStatus) => void
  isReadOnly: boolean
}

function mkExtensions(config: EditorStateConfig): Extension {
  // Per-state deduper (mkExtensions runs once per state creation), so redundant
  // updates -- e.g. an edit that leaves the cursor put -- don't re-notify.
  const notifyCursor = config.onCursorChange
    ? dedupeCursorStatus(config.onCursorChange)
    : undefined
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
    // In-process LSP features: hover, completion, signature help, and
    // diagnostics (which feed the lintGutter above). See codemirror/lsp.
    scamperLspExtensions(),
    EditorView.updateListener.of((update) => {
      if (update.docChanged) {
        config.dirtyAction()
      }
      if (notifyCursor && (update.selectionSet || update.docChanged)) {
        notifyCursor(cursorStatus(update.state))
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

/**
 * A read-only state showing `doc` with the edits that turned `original` into
 * it marked inline, for previewing a file's saved history.
 *
 * Deliberately far smaller than {@link mkFreshEditorState}: an old version is
 * being read, not worked on, so it gets no LSP, lint, queries, or editing
 * keymaps -- diagnostics against a snapshot would be noise at best.
 */
export function mkDiffEditorState(doc: string, original: string): EditorState {
  return EditorState.create({
    doc,
    extensions: [
      lineNumbers(),
      highlightSpecialChars(),
      editorThemeCompartment.of(editorThemeExtension(currentTheme.value)),
      EditorState.readOnly.of(true),
      EditorView.editable.of(false),
      ScamperSupport(),
      unifiedMergeView({ original, mergeControls: false }),
    ],
  })
}

export function mkNoFileEditorState(
  onCursorChange?: (status: CursorStatus) => void,
): EditorState {
  return EditorState.create({
    doc: noLoadedFileText,
    extensions: mkExtensions({
      dirtyAction: () => {
        /* empty */
      },
      onCursorChange,
      isReadOnly: true,
    }),
  })
}

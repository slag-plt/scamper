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
  foldGutter,
  foldKeymap,
  HighlightStyle,
  indentOnInput,
  syntaxHighlighting,
} from '@codemirror/language'
import { tags as t } from '@lezer/highlight'
import { currentTheme, type Theme } from '../../../theme'
import { editorFontSize, editorWordWrap } from '../editor-prefs'
import {
  defaultKeymap,
  history,
  historyKeymap,
  indentLess,
  indentSelection,
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
import { IndentationExtension } from './extensions/indentation'
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

// Editor theme.
//
// One set of rules serves both themes: every color is a theme.css token, and
// those are light-dark() pairs that resolve against the active color-scheme. So
// the light and dark extensions differ only in CodeMirror's own `dark` flag,
// which is what tells its extensions (and any add-on that asks) which way round
// the editor is.
//
// Light used to have no theme at all -- it inherited CodeMirror's stock chrome
// and defaultHighlightStyle, so the gutter, selection and syntax colors were
// whatever the library shipped rather than anything the design system chose.
//
// The active theme lives in a Compartment so it can be swapped live (see
// CodeMirrorEditor.vue) without rebuilding editor state.
const editorChrome = {
  '&': { color: 'var(--fg)', backgroundColor: 'var(--surface)' },
  '.cm-content': { caretColor: 'var(--fg)' },
  '.cm-cursor, .cm-dropCursor': { borderLeftColor: 'var(--fg)' },
  '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection':
    { backgroundColor: 'var(--editor-selection)' },
  '.cm-gutters': {
    backgroundColor: 'var(--surface)',
    color: 'var(--editor-gutter-fg)',
    border: 'none',
  },
  '.cm-activeLine': { backgroundColor: 'var(--editor-active-line)' },
  '.cm-activeLineGutter': { backgroundColor: 'var(--editor-active-line)' },
  '.cm-foldPlaceholder': {
    backgroundColor: 'transparent',
    border: 'none',
    color: 'var(--syntax-comment)',
  },
  '.cm-tooltip': {
    backgroundColor: 'var(--surface)',
    border: '1px solid var(--border)',
    color: 'var(--fg)',
  },
  '.cm-tooltip-autocomplete ul li[aria-selected]': {
    backgroundColor: 'var(--surface-hover)',
    color: 'var(--fg)',
  },
}

// Covers exactly the tags extensions/language.ts assigns, which is a closed
// set -- hence no defaultHighlightStyle fallback beneath it, and with it the
// last of the editor's hardcoded light-only colors.
const scamperHighlightStyle = HighlightStyle.define([
  { tag: t.keyword, color: 'var(--syntax-keyword)' },
  { tag: t.variableName, color: 'var(--syntax-variable)' },
  { tag: [t.bool, t.null, t.atom], color: 'var(--syntax-number)' },
  { tag: t.number, color: 'var(--syntax-number)' },
  { tag: [t.string, t.character], color: 'var(--syntax-string)' },
  {
    tag: [t.lineComment, t.comment],
    color: 'var(--syntax-comment)',
    fontStyle: 'italic',
  },
  {
    tag: [t.paren, t.squareBracket, t.brace, t.punctuation],
    color: 'var(--syntax-punctuation)',
  },
])

const lightThemeExtension: Extension = [
  EditorView.theme(editorChrome, { dark: false }),
  syntaxHighlighting(scamperHighlightStyle),
]

const darkThemeExtension: Extension = [
  EditorView.theme(editorChrome, { dark: true }),
  syntaxHighlighting(scamperHighlightStyle),
]

/** The editor theme+highlight extension for a given app theme. */
export function editorThemeExtension(theme: Theme): Extension {
  return theme === 'dark' ? darkThemeExtension : lightThemeExtension
}

/** Compartment holding the active editor theme, for live reconfiguration. */
export const editorThemeCompartment = new Compartment()

/**
 * Compartments for the display preferences the View menu changes. They are
 * reconfigured live while a file is open; a file opened afterwards gets the
 * same values through {@link mkExtensions}, which reads editor-prefs directly.
 */
export const fontSizeCompartment = new Compartment()
export const wordWrapCompartment = new Compartment()

/** The editor's font-size extension for a given size in pixels. */
export function fontSizeExtension(px: number): Extension {
  return EditorView.theme({
    '&': { fontSize: `${String(px)}px` },
  })
}

/** The line-wrapping extension, or nothing when lines should scroll instead. */
export function wordWrapExtension(on: boolean): Extension {
  return on ? EditorView.lineWrapping : []
}

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
    fontSizeCompartment.of(fontSizeExtension(editorFontSize.value)),
    wordWrapCompartment.of(wordWrapExtension(editorWordWrap.value)),
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
    // Ctrl-I re-indents the whole document.
    IndentationExtension,
    // TODO: probably extend this out into a separate extension file
    keymap.of([
      // Tab re-indents the selected lines rather than adding an indent unit,
      // matching DrRacket. Shift-Tab still outdents, for the rare line the
      // indenter cannot place.
      { key: 'Tab', run: indentSelection, shift: indentLess },
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

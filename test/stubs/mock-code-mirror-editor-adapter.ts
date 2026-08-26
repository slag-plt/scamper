import { Loc } from '../../src/lpm'
import type { CodeMirrorEditorAdapter } from '../../src/app/web/composables/codemirror-editor-adapter'

/** Records which editing commands a test drove, in order. */
export interface CommandLog {
  calls: string[]
}

export function makeMockCodeMirrorEditorAdapter(
  overrides: Partial<CodeMirrorEditorAdapter> = {},
  log: CommandLog = { calls: [] },
): CodeMirrorEditorAdapter {
  // The editing commands do nothing but record that they ran: what a menu test
  // wants to know is that picking "Format File" reached `format`, not what
  // CodeMirror then did with the document.
  const record =
    (name: string) =>
    () => {
      log.calls.push(name)
    }
  return {
    getDoc: () => '1',
    isLoaded: () => true,
    initializeDoc: () => {
      /* noop */
    },
    initializeDummyDoc: () => {
      /* noop */
    },
    replaceDoc: () => {
      /* noop */
    },
    replaceRange: () => {
      /* noop; a test that cares overrides this */
    },
    getCursorLoc: () => new Loc(0, 0, 0),
    setCursor: () => {
      /* noop; a test that cares overrides this */
    },
    coordsAtIdx: () => null,
    focus: record('focus'),
    status: () => ({
      readOnly: false,
      hasSelection: false,
      canUndo: false,
      canRedo: false,
      onIdentifier: false,
      isScamper: true,
    }),
    undo: record('undo'),
    redo: record('redo'),
    copy: record('copy'),
    cut: () => {
      log.calls.push('cut')
      return Promise.resolve()
    },
    paste: record('paste'),
    selectAll: record('selectAll'),
    find: record('find'),
    replace: record('replace'),
    goToLine: record('goToLine'),
    toggleComment: record('toggleComment'),
    format: record('format'),
    foldAll: record('foldAll'),
    unfoldAll: record('unfoldAll'),
    setExampleMarks: () => {
      /* noop; a test that cares overrides this */
    },
    goToDefinition: record('goToDefinition'),
    findReferences: record('findReferences'),
    destroy: () => {
      /* noop */
    },
    ...overrides,
  }
}

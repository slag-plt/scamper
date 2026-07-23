import { Loc } from '../../src/lpm'
import type { CodeMirrorEditorAdapter } from '../../src/app/web/composables/codemirror-editor-adapter'

export function makeMockCodeMirrorEditorAdapter(
  overrides: Partial<CodeMirrorEditorAdapter> = {},
): CodeMirrorEditorAdapter {
  return {
    getDoc: () => '1',
    isLoaded: () => true,
    initializeDoc: () => {
      /* noop */
    },
    initializeDummyDoc: () => {
      /* noop */
    },
    getCursorLoc: () => new Loc(0, 0, 0),
    coordsAtIdx: () => null,
    destroy: () => {
      /* noop */
    },
    ...overrides,
  }
}

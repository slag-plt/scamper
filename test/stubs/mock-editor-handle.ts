import type { CodeMirrorEditorAdapter } from '../../src/app/web/composables/codemirror-editor-adapter'

export const mockEditorHandle: {
  adapter: CodeMirrorEditorAdapter | null
  /**
   * Names of the editing commands driven through the adapter since the editor
   * mounted, in order -- how a test checks that a menu item reached the editor
   * without asserting on what CodeMirror would have done.
   */
  commands: string[]
} = {
  adapter: null,
  commands: [],
}

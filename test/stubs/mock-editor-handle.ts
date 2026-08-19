import type { CodeMirrorEditorAdapter } from '../../src/app/web/composables/codemirror-editor-adapter'

export const mockEditorHandle: {
  adapter: CodeMirrorEditorAdapter | null
  /**
   * Names of the editing commands driven through the adapter since the editor
   * mounted, in order -- how a test checks that a menu item reached the editor
   * without asserting on what CodeMirror would have done.
   */
  commands: string[]
  /**
   * The enclosing-form path the stub editor reports as its cursor status.
   *
   * Empty by default, which is what the IDE reads as "the cursor is not inside
   * a statement" -- so stepping is unavailable unless a test says otherwise.
   * Set it before mounting, and put it back afterwards.
   */
  cursorPath: string[]
} = {
  adapter: null,
  commands: [],
  cursorPath: [],
}

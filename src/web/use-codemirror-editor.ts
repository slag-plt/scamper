/**
 * Public interface of CodeMirrorEditor.vue.
 * Declared here so IdeApp.vue can type its template ref without a hardcoded
 * interface. The actual EditorView lifecycle lives in the component.
 */
export interface CodeMirrorEditorType {
  getDoc(): string
  initializeDoc(src: string): void
  initializeDummyDoc(): void
}

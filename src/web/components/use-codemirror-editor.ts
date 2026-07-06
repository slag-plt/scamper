// TODO: move codemirror functions here as single source of truth
export interface CodeMirrorEditorType {
  getDoc(): string
  initializeDoc(src: string): void
  initializeDummyDoc(): void
}

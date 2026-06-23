// TODO: move codemirror functions here as single source of truth
import { Loc } from "../../lpm"

export interface CodeMirrorEditorType {
  getDoc(): string
  initializeDoc(src: string): void
  initializeDummyDoc(): void
  getCursorLoc(): Loc | null
}

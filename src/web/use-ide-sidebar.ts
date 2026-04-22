import { ref } from "vue"
import type { FileEntry } from "./fs.js"

export function useIdeSidebar() {
  const files = ref<FileEntry[]>([])
  const currentFile = ref<string | null>(null)

  function setFiles(f: FileEntry[]): void {
    files.value = f
  }

  function setCurrentFile(f: string | null): void {
    currentFile.value = f
  }

  return { files, currentFile, setFiles, setCurrentFile }
}

export type IdeSidebarType = ReturnType<typeof useIdeSidebar>

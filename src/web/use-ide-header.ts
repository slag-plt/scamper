import { ref } from "vue"

export function useIdeHeader() {
  const currentFile = ref<string | null>(null)

  function setCurrentFile(f: string | null): void {
    currentFile.value = f
  }

  return { currentFile, setCurrentFile }
}

export type IdeHeaderType = ReturnType<typeof useIdeHeader>

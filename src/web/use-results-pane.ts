import { ref } from "vue"
import type { OutputPaneType } from "./output/use-output-pane"

export function useResultsPane() {
  const isTracing = ref(false)
  const isDirty = ref(false)

  function setTracing(v: boolean): void {
    isTracing.value = v
  }

  function makeDirty(): void {
    isDirty.value = true
  }

  function makeClean(): void {
    isDirty.value = false
  }

  return { isTracing, isDirty, setTracing, makeDirty, makeClean }
}

export type ResultsPaneType = ReturnType<typeof useResultsPane> &
  Pick<OutputPaneType, "reset" | "scrollToBottom" | "display">

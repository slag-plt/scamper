<script lang="ts">
  import type { DisplayCallbacks } from "../lpm/output/svelte"
  import { SvelteDisplay } from "../lpm/output/svelte"
  import { renderToOutput } from "../lpm/output/html"
  import { throwNull } from "../util"

  const levels: HTMLElement[] = $state([])

  const displayInput: DisplayCallbacks = {
    PushChildCallback: (element) => {
      ;(
        levels.at(-1) ??
        throwNull("tried to push child when levels array empty!")
      ).appendChild(element)
      levels.push(element)
    },
    PopLevelCallback: () => {
      levels.pop()
    },
    SendToCurrentLevelCallback: (value) => {
      renderToOutput(
        levels.at(-1) ??
          throwNull("tried to send to current level when levels array empty!"),
        value,
      )
    },
  }

  export const display = new SvelteDisplay(displayInput)

  export function reset(): void {
    levels.length = 1 // keep root
    levels[0].replaceChildren()
  }

  function mountRoot(el: HTMLElement) {
    levels.push(el)
    return {
      destroy() {
        levels.length = 0
      },
    }
  }
</script>

<div use:mountRoot></div>

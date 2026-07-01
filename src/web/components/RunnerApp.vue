<script setup lang="ts">
import * as FS from "../../fs"
import { onMounted, shallowRef } from "vue"
import { ScamperError } from "../../lpm/error"
import OutputPane from "./OutputPane.vue"
import type { OutputPaneType } from "./use-output-pane"
import { ScamperInstance } from "../../scamper"

const outputPaneRef = shallowRef<OutputPaneType | null>(null)
const version = shallowRef("")
const currentFile = shallowRef("")

onMounted(async () => {
  const display = outputPaneRef.value?.display
  if (!display) return

  await FS.initialize()
  const fs = FS.getFS()
  const params = new URLSearchParams(window.location.search)

  if (!params.has("filename")) {
    display.report(new ScamperError("Runtime", "No filename specified"))
    return
  }

  const filename = params.get("filename")
  if (!filename) return

  currentFile.value = filename

  if (!(await fs.fileExists(filename))) {
    display.report(
      new ScamperError("Runtime", `File ${filename} does not exist`),
    )
    return
  }

  const src = await fs.loadFile(filename)
  ScamperInstance.getInstance().execute({ src, out: display, err: display })

  version.value = `(${APP_VERSION})`
})
</script>

<template>
  <div class="runner">
    <div class="header">
      Scamper <span>{{ version }}</span> ⋅ <span>{{ currentFile }}</span>
    </div>
    <div id="output" class="output-wrapper">
      <OutputPane ref="outputPaneRef" />
    </div>
  </div>
</template>

<style>
html,
body {
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
  font-family:
    -apple-system,
    BlinkMacSystemFont,
    avenir next,
    avenir,
    segoe ui,
    helvetica neue,
    helvetica,
    Cantarell,
    Ubuntu,
    roboto,
    noto,
    arial,
    sans-serif;
  font-size: 1em;
}
</style>

<style scoped>
.runner {
  height: 100%;
  display: flex;
  flex-direction: column;
}

.header {
  background: #eee;
  color: #333;
  padding: 0.5em;
  flex: 0 0 auto;
}

.output-wrapper {
  flex: 1;
  min-height: 0;
  background: #fff;
  color: #333;
}
</style>

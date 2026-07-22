import { createApp } from "vue"
import DocsApp from "./DocsApp.vue"
import { initializeLibs } from "../lib"

function throwNull(message: string): never {
  throw new Error(message)
}

await initializeLibs()
createApp(DocsApp).mount(
  document.getElementById("app") ?? throwNull("no app element"),
)

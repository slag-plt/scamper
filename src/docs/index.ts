import { createApp } from "vue"
import DocsApp from "./DocsApp.vue"

function throwNull(message: string): never {
  throw new Error(message)
}

createApp(DocsApp).mount(
  document.getElementById("app") ?? throwNull("no app element"),
)

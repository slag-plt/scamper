import { createApp } from "vue"
import DocsApp from "./DocsApp.vue"
import { throwNull } from "../utils"

createApp(DocsApp).mount(
  document.getElementById("app") ?? throwNull("no app element"),
)

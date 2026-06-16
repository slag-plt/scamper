import { createApp } from "vue"
import { throwNull } from "../util.js"
import DocsApp from "./DocsApp.vue"

createApp(DocsApp).mount(
  document.getElementById("app") ?? throwNull("no app element"),
)

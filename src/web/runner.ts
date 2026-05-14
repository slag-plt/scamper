import { createApp } from "vue"
import { throwNull } from "../util.js"
import RunnerApp from "./components/RunnerApp.vue"

createApp(RunnerApp).mount(
  document.getElementById("app") ?? throwNull("no app element"),
)

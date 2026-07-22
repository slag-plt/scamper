import { createApp } from "vue"
import RunnerApp from "./components/RunnerApp.vue"
import { initialize } from "../scamper"

await initialize()
createApp(RunnerApp).mount("#app")

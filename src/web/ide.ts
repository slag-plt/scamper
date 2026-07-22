import { createApp } from "vue"
import IdeApp from "./components/IdeApp.vue"
import { initialize } from "../scamper"

await initialize()
createApp(IdeApp).mount("#app")

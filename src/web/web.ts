import { createApp } from "vue"
import { initializeLibs } from "../lib/index.js"
import WebEmbedWidget from "./WebEmbedWidget.vue"

await initializeLibs()

const widgets = Array.from(document.getElementsByClassName("scamper"))
for (const widget of widgets) {
  const el = widget as HTMLElement
  const src = el.innerText
  const sourceOnly = el.classList.contains("source-only")
  el.innerHTML = ""
  createApp(WebEmbedWidget, { src, sourceOnly }).mount(el)
}

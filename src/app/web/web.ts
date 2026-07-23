import { createApp } from 'vue'
import WebEmbedWidget from './components/WebEmbedWidget.vue'
import { initialize } from '../../scamper'

await initialize()
const widgets = Array.from(document.getElementsByClassName('scamper'))
for (const widget of widgets) {
  const el = widget as HTMLElement
  const src = el.innerText
  const sourceOnly = el.classList.contains('source-only')
  el.innerHTML = ''
  createApp(WebEmbedWidget, { src, sourceOnly }).mount(el)
}

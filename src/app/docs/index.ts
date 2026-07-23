import { createApp } from 'vue'
import DocsApp from './DocsApp.vue'
import { throwNull } from '../../utils'
import { initializeLibs } from '../../lib'

await initializeLibs()
createApp(DocsApp).mount(
  document.getElementById('app') ?? throwNull('no app element'),
)

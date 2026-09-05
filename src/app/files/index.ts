import { createApp } from 'vue'
import FilesApp from './FilesApp.vue'
import { throwNull } from '../../utils'

// No `initializeLibs()`, unlike the docs page: this one has to come up on a
// Scamper the IDE can no longer open, so it loads nothing of the language.

createApp(FilesApp).mount(
  document.getElementById('app') ?? throwNull('no app element'),
)

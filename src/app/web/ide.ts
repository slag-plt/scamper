import { createApp } from 'vue'
import IdeApp from './components/IdeApp.vue'
import { initialize } from '../../scamper'
// Bundled via the entry (not a relative <link>) so it resolves no matter what
// URL the IDE is served at -- see devFlatHtmlPlugin / flattenHtmlPlugin.
import './index.css'

await initialize()
createApp(IdeApp).mount('#app')

import { createApp } from 'vue'
import IdeApp from './components/IdeApp.vue'
import { initialize } from '../../scamper'
import { initializeBackend } from './server-session'
// Bundled via the entry (not a relative <link>) so it resolves no matter what
// URL the IDE is served at -- see devFlatHtmlPlugin / flattenHtmlPlugin.
import './index.css'

// Before the app mounts, so IdeApp's FS.initialize() finds a backend already
// chosen rather than defaulting to local storage. A no-op where the deployment
// advertises no file server, which is every checkout without a /config.json.
await initializeBackend()
await initialize()
createApp(IdeApp).mount('#app')

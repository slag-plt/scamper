import { createApp } from 'vue'
import IdeApp from './components/IdeApp.vue'
import { initialize } from '../../scamper'
import { useServerBackendInDev } from './dev-backend'
// Bundled via the entry (not a relative <link>) so it resolves no matter what
// URL the IDE is served at -- see devFlatHtmlPlugin / flattenHtmlPlugin.
import './index.css'

// Before the app mounts, so IdeApp's FS.initialize() finds a backend already
// chosen rather than defaulting to OPFS. A no-op in a production build.
await useServerBackendInDev()
await initialize()
createApp(IdeApp).mount('#app')

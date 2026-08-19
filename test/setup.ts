import * as matchers from '@testing-library/jest-dom/matchers'
import { afterEach, expect } from 'vitest'
import { activeModal, dismissModal } from '../src/app/web/composables/use-modals'
import 'vitest-canvas-mock'
import { initializeLibs } from '../src/lib'
import * as SymbolDB from '../src/scheme/symbol-db'

expect.extend(matchers)
// N.B., only initializeLibs() (plus the symbol DB it feeds) here, not
// scamper.ts's initialize(): importing scamper.ts triggers its
// module-load-time renderer registration (a fire-and-forget
// `import("./app/web/renderers.js")`), and doing that from this shared global
// setup -- before any individual test file's own vi.mock(...) calls have been
// registered -- grabs real (unmocked) transitive dependencies (e.g.
// src/fs/opfs.ts) out from under tests that mock them. Test files that
// actually need Scamper.getInstance() (or anything else from scamper.ts) call
// its initialize() themselves, after their own mocks.
await initializeLibs()
// N.B., after initializeLibs(): the symbol DB snapshots the just-loaded
// builtin libraries.
SymbolDB.initialize()

// jsdom implements no layout, and so no ResizeObserver. Components that watch
// their own box for changes (the floating output window keeping itself inside
// the pane) only need it not to throw: nothing here ever resizes.
if (!('ResizeObserver' in globalThis)) {
  globalThis.ResizeObserver = class {
    observe() {
      /* no layout in jsdom, so nothing ever fires */
    }

    unobserve() {
      /* as above */
    }

    disconnect() {
      /* as above */
    }
  }
}

// use-modals keeps a single module-level queue of pending dialogs, so a test
// that opens one and never answers it leaves the request active -- and the next
// test's freshly mounted ModalHost renders that stale dialog instead of the one
// it is waiting for. Draining between tests keeps that from crossing files.
afterEach(() => {
  while (activeModal.value !== null) {
    dismissModal()
  }
})

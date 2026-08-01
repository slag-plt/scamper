import type { Extension } from '@codemirror/state'
import { keymap } from '@codemirror/view'
import {
  LSPClient,
  closeReferencePanel,
  findReferences,
  hoverTooltips,
  jumpToDefinition,
  serverCompletion,
  serverDiagnostics,
  signatureHelp,
} from '@codemirror/lsp-client'
import { ScamperLanguageServer } from './server'
import { createInProcessTransport } from './transport'

// Navigation keybindings. Deliberately not the library defaults (F12 /
// Shift-F12), which browsers hijack for devtools.
const navigationKeymap = keymap.of([
  { key: 'Alt-.', run: jumpToDefinition, preventDefault: true },
  { key: 'Shift-Alt-.', run: findReferences, preventDefault: true },
  { key: 'Escape', run: closeReferencePanel },
])

// The editor shows one Scamper file at a time, so a single stable URI is
// enough: switching files closes and reopens this URI with the new contents.
const SCAMPER_DOC_URI = 'inmemory://main.scm'
const SCAMPER_LANGUAGE_ID = 'scheme'

let client: LSPClient | undefined

/**
 * The shared LSP client, created and connected to the in-process Scamper
 * server on first use. Lazy so importing this module has no side effects
 * (e.g. under SSR or tests).
 */
function getClient(): LSPClient {
  if (client === undefined) {
    const server = new ScamperLanguageServer()
    client = new LSPClient({
      // Editor features; each stays dormant until the server advertises the
      // matching capability (see ScamperLanguageServer). client.plugin() below
      // pulls these into the editor.
      extensions: [
        hoverTooltips(),
        serverCompletion(),
        signatureHelp(),
        serverDiagnostics(),
        navigationKeymap,
      ],
    }).connect(createInProcessTransport(server))
  }
  return client
}

/**
 * CodeMirror extension connecting the editor to the in-process Scamper
 * language server: hover docs, completion, signature help, diagnostics,
 * goto-definition (Alt-.), and find-references (Shift-Alt-.).
 */
export function scamperLspExtensions(): Extension {
  return getClient().plugin(SCAMPER_DOC_URI, SCAMPER_LANGUAGE_ID)
}

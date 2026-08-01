import type { Extension } from '@codemirror/state'
import {
  LSPClient,
  hoverTooltips,
  serverCompletion,
  serverDiagnostics,
  signatureHelp,
} from '@codemirror/lsp-client'
import { ScamperLanguageServer } from './server'
import { createInProcessTransport } from './transport'

// The editor shows one Scamper file at a time, so a single stable URI is
// enough: switching files closes and reopens this URI with the new contents.
const SCAMPER_DOC_URI = 'inmemory://main.scm'
const SCAMPER_LANGUAGE_ID = 'scheme'

/**
 * Experimental A/B switch: with `?lsp-diagnostics` in the URL, diagnostics are
 * served over LSP (server push) instead of the native CodeMirror linter, so
 * the two can be compared on the same code. See extensions/linter.ts.
 */
export function lspDiagnosticsEnabled(): boolean {
  return (
    typeof location !== 'undefined' &&
    new URLSearchParams(location.search).has('lsp-diagnostics')
  )
}

let client: LSPClient | undefined

/**
 * The shared LSP client, created and connected to the in-process Scamper
 * server on first use. Lazy so importing this module has no side effects
 * (e.g. under SSR or tests).
 */
function getClient(): LSPClient {
  if (client === undefined) {
    const lspDiagnostics = lspDiagnosticsEnabled()
    const server = new ScamperLanguageServer({
      publishDiagnostics: lspDiagnostics,
    })
    client = new LSPClient({
      // Editor features; each stays dormant until the server advertises the
      // matching capability (see ScamperLanguageServer). client.plugin() below
      // pulls these into the editor.
      extensions: [
        hoverTooltips(),
        serverCompletion(),
        signatureHelp(),
        ...(lspDiagnostics ? [serverDiagnostics()] : []),
      ],
    }).connect(createInProcessTransport(server))
  }
  return client
}

/**
 * CodeMirror extension connecting the editor to the in-process Scamper
 * language server: hover docs, completion, and signature help.
 */
export function scamperLspExtensions(): Extension {
  return getClient().plugin(SCAMPER_DOC_URI, SCAMPER_LANGUAGE_ID)
}

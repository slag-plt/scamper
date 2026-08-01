import type { Extension } from '@codemirror/state'
import { LSPClient, hoverTooltips } from '@codemirror/lsp-client'
import { ScamperLanguageServer } from './server'
import { createInProcessTransport } from './transport'

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
    client = new LSPClient().connect(createInProcessTransport(server))
  }
  return client
}

/**
 * CodeMirror extensions that connect the editor to the in-process Scamper
 * language server. Phase 1 provides hover documentation; further features
 * (completion, goto-definition, ...) are added here as the server advertises
 * the matching capabilities.
 */
export function scamperLspExtensions(): Extension {
  const c = getClient()
  return [c.plugin(SCAMPER_DOC_URI, SCAMPER_LANGUAGE_ID), hoverTooltips()]
}

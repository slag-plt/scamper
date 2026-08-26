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
import { occurrenceHighlighter } from './highlight-extension'

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
let server: ScamperLanguageServer | undefined

/**
 * The shared LSP client, created and connected to the in-process Scamper
 * server on first use. Lazy so importing this module has no side effects
 * (e.g. under SSR or tests).
 */
function getClient(): LSPClient {
  if (client === undefined) {
    server = new ScamperLanguageServer()
    client = new LSPClient({
      // Editor features; each stays dormant until the server advertises the
      // matching capability (see ScamperLanguageServer). client.plugin() below
      // pulls these into the editor.
      extensions: [
        hoverTooltips(),
        serverCompletion(),
        signatureHelp(),
        serverDiagnostics(),
        occurrenceHighlighter(),
        navigationKeymap,
      ],
    }).connect(createInProcessTransport(server))
  }
  return client
}

/**
 * CodeMirror extension connecting an editor to the in-process Scamper language
 * server: hover docs, completion, signature help, diagnostics, occurrence
 * highlighting, goto-definition (Alt-.), and find-references (Shift-Alt-.).
 *
 * @param uri which document this editor holds. One per editor: the default
 *        workspace refuses two views on one URI, and two editors sharing a URI
 *        would be two editors overwriting one document's contents. Defaults to
 *        the file the IDE is editing.
 */
export function scamperLspExtensions(uri = SCAMPER_DOC_URI): Extension {
  return getClient().plugin(uri, SCAMPER_LANGUAGE_ID)
}

/**
 * Sets the source `uri` is analysed inside: the program it is a continuation
 * of. For a REPL cell that is the file the session was seeded from and the
 * entries before it, which is what makes a name defined out there resolve in
 * here (#399).
 *
 * A document with a context is not linted -- see the server's
 * publishDiagnostics.
 */
export function setLspContext(uri: string, context: string): void {
  getClient()
  server?.setContext(uri, context)
}

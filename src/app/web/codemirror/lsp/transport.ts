import type { Transport } from '@codemirror/lsp-client'
import type { ScamperLanguageServer } from './server'

/**
 * A transport that wires the CodeMirror LSP client straight to an in-process
 * {@link ScamperLanguageServer}, with no worker or socket in between. A request
 * string is just a function call on the server; the server's replies are handed
 * back to the client's subscribers.
 */
export function createInProcessTransport(
  server: ScamperLanguageServer,
): Transport {
  const handlers = new Set<(value: string) => void>()
  server.setSend((message) => {
    // Deliver on a microtask so a response never re-enters the client
    // synchronously from within its own send() -- the client subscribes and
    // then sends `initialize` during connect(), and a synchronous reply would
    // land mid-connect.
    queueMicrotask(() => {
      for (const handler of handlers) {
        handler(message)
      }
    })
  })
  return {
    send(message) {
      server.handle(message)
    },
    subscribe(handler) {
      handlers.add(handler)
    },
    unsubscribe(handler) {
      handlers.delete(handler)
    },
  }
}

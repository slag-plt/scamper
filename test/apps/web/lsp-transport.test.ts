import { describe, expect, test } from 'vitest'
import { LSPClient } from '@codemirror/lsp-client'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'
import { createInProcessTransport } from '../../../src/app/web/codemirror/lsp/transport'

// Exercises the real client <-> transport <-> server path (async microtask
// delivery, JSON-RPC id correlation) that the direct-dispatch unit tests skip.

describe('in-process LSP transport', () => {
  test('completes the initialize handshake and a hover round-trip', async () => {
    const server = new ScamperLanguageServer()
    const client = new LSPClient().connect(createInProcessTransport(server))
    try {
      await client.initializing
      expect(client.serverCapabilities?.hoverProvider).toBe(true)

      client.notification('textDocument/didOpen', {
        textDocument: {
          uri: 'inmemory://main.scm',
          languageId: 'scheme',
          version: 1,
          text: '(car (list 1))',
        },
      })
      const hover = await client.request<
        unknown,
        { contents: { value: string } } | null
      >('textDocument/hover', {
        textDocument: { uri: 'inmemory://main.scm' },
        position: { line: 0, character: 1 },
      })
      expect(hover).not.toBeNull()
      expect(hover?.contents.value).toContain('car')
    } finally {
      client.disconnect()
    }
  })
})

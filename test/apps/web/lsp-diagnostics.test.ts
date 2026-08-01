import { describe, expect, test } from 'vitest'
import { computeDiagnostics } from '../../../src/app/web/codemirror/lsp/diagnostics'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'
import { computeLineStarts } from '../../../src/app/web/codemirror/lsp/positions'

// docRegistry + symbol DB are populated by the global test setup.

describe('computeDiagnostics', () => {
  test('reports an undefined variable', async () => {
    const src = '(+ zzz 1)'
    const diags = await computeDiagnostics(src, computeLineStarts(src))
    expect(diags.length).toBeGreaterThan(0)
    expect(diags[0].message.toLowerCase()).toContain('zzz')
  })

  test('reports nothing for a well-formed program', async () => {
    const src = '(+ 1 2)'
    expect(await computeDiagnostics(src, computeLineStarts(src))).toEqual([])
  })
})

describe('ScamperLanguageServer diagnostics push', () => {
  function open(text: string, options: { publishDiagnostics?: boolean }) {
    const server = new ScamperLanguageServer(options)
    const notes: { method?: string; params?: { diagnostics: unknown[] } }[] = []
    server.setSend((m) => {
      notes.push(JSON.parse(m) as { method?: string })
    })
    server.handle(
      JSON.stringify({
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
          textDocument: {
            uri: 'inmemory://main.scm',
            languageId: 'scheme',
            version: 1,
            text,
          },
        },
      }),
    )
    return notes
  }

  const flush = () => new Promise((resolve) => setTimeout(resolve, 20))

  test('pushes publishDiagnostics when enabled', async () => {
    const notes = open('(+ zzz 1)', { publishDiagnostics: true })
    await flush()
    const publish = notes.find(
      (n) => n.method === 'textDocument/publishDiagnostics',
    )
    expect(publish).toBeDefined()
    expect(publish?.params?.diagnostics.length).toBeGreaterThan(0)
  })

  test('does not push when disabled (default)', async () => {
    const notes = open('(+ zzz 1)', {})
    await flush()
    expect(
      notes.find((n) => n.method === 'textDocument/publishDiagnostics'),
    ).toBeUndefined()
  })
})

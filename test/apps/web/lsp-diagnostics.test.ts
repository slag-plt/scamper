import type { Diagnostic } from 'vscode-languageserver-protocol'
import { describe, expect, test } from 'vitest'
import { computeDiagnostics } from '../../../src/app/web/codemirror/lsp/diagnostics'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'
import { computeLineStarts } from '../../../src/app/web/codemirror/lsp/positions'

// docRegistry + symbol DB are populated by the global test setup.

describe('computeDiagnostics', () => {
  test('reports an undefined variable', async () => {
    const src = '(+ zzz 1)'
    const diags: Diagnostic[] = await computeDiagnostics(
      src,
      computeLineStarts(src),
    )
    expect(diags.length).toBeGreaterThan(0)
    expect(diags[0].message).toMatch(/zzz/i)
  })

  test('reports nothing for a well-formed program', async () => {
    const src = '(+ 1 2)'
    expect(await computeDiagnostics(src, computeLineStarts(src))).toEqual([])
  })
})

describe('ScamperLanguageServer diagnostics push', () => {
  function open(text: string) {
    const server = new ScamperLanguageServer()
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
  const publishOf = (notes: { method?: string; params?: { diagnostics: unknown[] } }[]) =>
    notes.find((n) => n.method === 'textDocument/publishDiagnostics')

  test('pushes publishDiagnostics with the errors on open', async () => {
    const notes = open('(+ zzz 1)')
    await flush()
    expect(publishOf(notes)?.params?.diagnostics.length).toBeGreaterThan(0)
  })

  test('pushes an empty diagnostic set for clean code', async () => {
    const notes = open('(+ 1 2)')
    await flush()
    expect(publishOf(notes)?.params?.diagnostics).toEqual([])
  })
})

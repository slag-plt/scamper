import { describe, expect, test } from 'vitest'
import { definitionAt } from '../../../src/app/web/codemirror/lsp/definition'
import { referencesAt } from '../../../src/app/web/codemirror/lsp/references'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'

// Symbol DB + docRegistry are populated by the global test setup.

describe('definitionAt', () => {
  test('jumps to a local parameter binding', async () => {
    // `(lambda (x) x)`: body x (index 12) -> the parameter x (index 9).
    expect(await definitionAt('(lambda (x) x)', 12)).toEqual({ from: 9, to: 10 })
  })

  test('jumps to a top-level definition', async () => {
    const src = '(define foo 1)\n(foo)'
    const useOffset = src.lastIndexOf('foo')
    const defOffset = src.indexOf('foo')
    expect(await definitionAt(src, useOffset)).toEqual({
      from: defOffset,
      to: defOffset + 3,
    })
  })

  test('returns null for a builtin (no in-source definition)', async () => {
    expect(await definitionAt('(car (list 1))', 1)).toBeNull()
  })

  test('returns null for an unbound identifier', async () => {
    expect(await definitionAt('(zzz)', 1)).toBeNull()
  })
})

describe('referencesAt', () => {
  test('finds the declaration and all uses of a binding', async () => {
    const src = '(define x 1)\n(+ x x)'
    const refs = await referencesAt(src, src.indexOf('x'))
    expect(refs.length).toBe(3)
  })

  test('respects shadowing', async () => {
    // The inner lambda binds its own x, so the outer x has only its own site.
    const src = '(define x 1)\n(lambda (x) x)'
    expect((await referencesAt(src, src.indexOf('x'))).length).toBe(1)
  })
})

describe('ScamperLanguageServer: navigation', () => {
  function serve(text: string) {
    const server = new ScamperLanguageServer()
    const sent: { id?: number; result?: any }[] = []
    server.setSend((m) => {
      sent.push(JSON.parse(m) as { id?: number; result?: unknown })
    })
    const request = (id: number, method: string, params: unknown) =>
      server.handle(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    request(1, 'initialize', { capabilities: {} })
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
    const reply = (id: number) => sent.find((m) => m.id === id) as { result: any }
    const flush = () => new Promise((resolve) => setTimeout(resolve, 20))
    return { request, reply, flush }
  }

  test('advertises definition and references capabilities', () => {
    const { reply } = serve('')
    const caps = reply(1).result.capabilities
    expect(caps.definitionProvider).toBe(true)
    expect(caps.referencesProvider).toBe(true)
  })

  test('answers a definition request with a Location', async () => {
    const { request, reply, flush } = serve('(lambda (x) x)')
    request(2, 'textDocument/definition', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 12 },
    })
    await flush()
    expect(reply(2).result.range.start.character).toBe(9)
  })

  test('answers a references request with all sites', async () => {
    const { request, reply, flush } = serve('(define x 1)\n(+ x x)')
    request(3, 'textDocument/references', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 8 },
      context: { includeDeclaration: true },
    })
    await flush()
    expect(reply(3).result.length).toBe(3)
  })
})

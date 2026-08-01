import { describe, expect, test } from 'vitest'
import { completionsFor } from '../../../src/app/web/codemirror/lsp/completion'
import { signatureHelpAt } from '../../../src/app/web/codemirror/lsp/signature'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'

// docRegistry is populated by the global test setup (initializeLibs()).

describe('completionsFor', () => {
  test('offers prelude bindings', () => {
    const labels = completionsFor('(display )').map((i) => i.label)
    expect(labels).toContain('car')
    expect(labels).toContain('length')
  })

  test('includes the program\'s own top-level definitions', () => {
    const labels = completionsFor('(define my-thing 42)\n').map((i) => i.label)
    expect(labels).toContain('my-thing')
  })

  test('adds bindings from an imported module', () => {
    const base = completionsFor('').length
    expect(completionsFor('(import image)').length).toBeGreaterThan(base)
  })

  test('carries a signature detail on documented items', () => {
    const car = completionsFor('').find((i) => i.label === 'car')
    expect(car?.detail).toContain('car')
  })
})

describe('signatureHelpAt', () => {
  test('describes a one-argument call with the active parameter', () => {
    const sig = signatureHelpAt('(car )', 5)
    expect(sig).not.toBeNull()
    expect(sig?.signatures[0].label).toContain('car')
    expect(sig?.signatures[0].parameters?.length).toBe(1)
    expect(sig?.activeParameter).toBe(0)
  })

  test('tracks the active parameter across arguments', () => {
    const src = '(equal? 1 )'
    const sig = signatureHelpAt(src, src.lastIndexOf(')'))
    expect(sig?.signatures[0].parameters?.length).toBe(2)
    expect(sig?.activeParameter).toBe(1)
  })

  test('returns null when not inside a documented call', () => {
    expect(signatureHelpAt('(+ 1 2)', 0)).toBeNull()
  })
})

describe('ScamperLanguageServer: completion & signature help', () => {
  function serve(text: string) {
    const server = new ScamperLanguageServer()
    const sent: { id?: number; result: unknown }[] = []
    server.setSend((m) => {
      sent.push(JSON.parse(m) as { id?: number; result: unknown })
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
    const reply = (id: number) =>
      sent.find((m) => m.id === id) as { result: any }
    return { request, reply }
  }

  test('advertises completion and signature-help capabilities', () => {
    const { reply } = serve('')
    const caps = reply(1).result.capabilities
    expect(caps.completionProvider).toBeDefined()
    expect(caps.signatureHelpProvider).toBeDefined()
  })

  test('answers a completion request', () => {
    const { request, reply } = serve('(display )')
    request(2, 'textDocument/completion', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 9 },
    })
    const labels = (reply(2).result as { label: string }[]).map((i) => i.label)
    expect(labels).toContain('car')
  })

  test('answers a signature-help request', () => {
    const { request, reply } = serve('(car )')
    request(3, 'textDocument/signatureHelp', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 5 },
    })
    expect(reply(3).result.signatures[0].label).toContain('car')
  })
})

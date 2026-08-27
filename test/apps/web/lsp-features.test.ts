import type {
  CompletionItem,
  InitializeResult,
  SignatureHelp,
} from 'vscode-languageserver-protocol'
import { describe, expect, test } from 'vitest'
import { completionsFor } from '../../../src/app/web/codemirror/lsp/completion'
import { signatureHelpAt } from '../../../src/app/web/codemirror/lsp/signature'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'

// docRegistry is populated by the global test setup (initializeLibs()).

describe('completionsFor', () => {
  const labelsOf = async (src: string, offset: number) =>
    (await completionsFor(src, offset)).map((i) => i.label)

  test('offers prelude bindings', async () => {
    const labels = await labelsOf('', 0)
    expect(labels).toContain('car')
    expect(labels).toContain('length')
  })

  test('includes the program\'s own top-level definitions', async () => {
    expect(await labelsOf('(define my-thing 42)\n', 0)).toContain('my-thing')
  })

  test('includes locals visible at the cursor', async () => {
    // Cursor on the lambda body `x`, where the parameter is in scope.
    const src = '(lambda (x) x)'
    expect(await labelsOf(src, src.length - 2)).toContain('x')
  })

  test('excludes internal ## machinery names', async () => {
    expect((await labelsOf('', 0)).some((l) => l.includes('##'))).toBe(false)
  })

  test('adds bindings from an imported module', async () => {
    const base = (await completionsFor('', 0)).length
    expect((await completionsFor('(import image)', 14)).length).toBeGreaterThan(
      base,
    )
  })

  test('carries a signature detail on documented items', async () => {
    const car = (await completionsFor('', 0)).find((i) => i.label === 'car')
    expect(car?.detail).toContain('car')
  })
})

describe('completionsFor: qualified imports', () => {
  const prelude = '(import image img)\n'
  const labelsOf = async (src: string, offset: number) =>
    (await completionsFor(src, offset)).map((i) => i.label)

  test('typing `alias.` offers the module members as alias.member', async () => {
    const src = `${prelude}img.`
    const labels = await labelsOf(src, src.length)
    expect(labels).toContain('img.rgb')
    expect(labels).toContain('img.rgb-red')
  })

  test('member completions are qualified, never bare', async () => {
    const src = `${prelude}img.`
    const labels = await labelsOf(src, src.length)
    expect(labels).not.toContain('rgb')
  })

  test('member completions carry a textEdit and filterText for the dotted token', async () => {
    const src = `${prelude}img.rg`
    const item = (await completionsFor(src, src.length)).find(
      (i) => i.label === 'img.rgb',
    )
    expect(item?.filterText).toBe('img.rgb')
    expect(item?.textEdit).toBeDefined()
  })

  test('a qualified import does not inject member names into the flat scope', async () => {
    // At a non-qualified position, `rgb` is only reachable as `img.rgb`.
    const labels = await labelsOf(prelude, prelude.length)
    expect(labels).not.toContain('rgb')
    expect(labels).not.toContain('rgb-red')
  })

  test('the alias itself is surfaced as a completion', async () => {
    expect(await labelsOf(prelude, prelude.length)).toContain('img')
  })

  test('an unqualified import does not create a qualified alias', async () => {
    // `image.` is not a known alias, so no members are offered under it.
    const src = '(import image)\nimage.'
    expect(await labelsOf(src, src.length)).not.toContain('image.rgb')
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
    const sent: { id?: number; result?: unknown }[] = []
    server.setSend((m) => {
      sent.push(JSON.parse(m) as { id?: number; result: unknown })
    })
    const request = (id: number, method: string, params: unknown) => {
      server.handle(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    }
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
    /**
     * The `result` of the reply to request `id`, as `unknown`: a JSON-RPC
     * response really is untyped until someone says what it should be, which
     * each assertion below does.
     */
    const reply = (id: number): unknown => {
      const found = sent.find((m) => m.id === id)
      if (found === undefined) throw new Error(`no reply to request ${id}`)
      return found.result
    }
    const flush = () => new Promise((resolve) => setTimeout(resolve, 20))
    return { request, reply, flush }
  }

  test('advertises completion and signature-help capabilities', () => {
    const { reply } = serve('')
    const caps = (reply(1) as InitializeResult).capabilities
    expect(caps.completionProvider).toBeDefined()
    expect(caps.signatureHelpProvider).toBeDefined()
  })

  test('answers a completion request', async () => {
    const { request, reply, flush } = serve('(+ 1 2)')
    request(2, 'textDocument/completion', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 3 },
    })
    await flush()
    const labels = (reply(2) as CompletionItem[]).map((i) => i.label)
    expect(labels).toContain('car')
  })

  test('answers a signature-help request', () => {
    const { request, reply } = serve('(car )')
    request(3, 'textDocument/signatureHelp', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 5 },
    })
    expect((reply(3) as SignatureHelp).signatures[0].label).toContain('car')
  })
})

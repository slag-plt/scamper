import { describe, expect, test } from 'vitest'
import { LSPClient } from '@codemirror/lsp-client'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'
import { createInProcessTransport } from '../../../src/app/web/codemirror/lsp/transport'
import { initialize } from '../../../src/scamper'

await initialize()

const URI = 'inmemory://repl-prompt.scm'

/**
 * A client on a fresh server, holding one document set inside `context` -- a
 * REPL cell, as far as the server is concerned (#399).
 */
async function openCell(text: string, context: string) {
  const server = new ScamperLanguageServer()
  const client = new LSPClient().connect(createInProcessTransport(server))
  await client.initializing
  server.setContext(URI, context)
  client.notification('textDocument/didOpen', {
    textDocument: { uri: URI, languageId: 'scheme', version: 1, text },
  })
  return {
    server,
    hover: (character: number) =>
      client.request<unknown, { contents: { value: string }; range?: unknown } | null>(
        'textDocument/hover',
        { textDocument: { uri: URI }, position: { line: 0, character } },
      ),
    completions: (character: number) =>
      client.request<unknown, { label: string }[]>('textDocument/completion', {
        textDocument: { uri: URI },
        position: { line: 0, character },
      }),
    definition: (character: number) =>
      client.request<unknown, { range: { start: { line: number } } } | null>(
        'textDocument/definition',
        { textDocument: { uri: URI }, position: { line: 0, character } },
      ),
    highlights: (character: number) =>
      client.request<unknown, { range: { start: { line: number; character: number } } }[]>(
        'textDocument/documentHighlight',
        { textDocument: { uri: URI }, position: { line: 0, character } },
      ),
  }
}

// A REPL cell is one form typed against a program it cannot see: the file the
// session was seeded from and the entries before it. The server analyses it
// with that context in front, and everything it hands back has to be in the
// cell's own coordinates.
describe('a document analysed inside a context', () => {
  test('completes a name the context defined', async () => {
    // Balanced, which is what the editor actually holds while you type: a `(`
    // arrives with its `)` already in place (closeBrackets).
    const cell = await openCell('(sq)', '(define sq (lambda (n) (* n n)))')
    const labels = (await cell.completions(3)).map((c) => c.label)
    expect(labels).toContain('sq')
  })

  test('a cell that does not parse falls back to the standard library', async () => {
    // The same fallback the file editor has, and for the same reason: a scope
    // tree needs a program. Recorded rather than desired -- what is lost is the
    // context's own names, until the brackets balance again.
    const cell = await openCell('(sq', '(define sq (lambda (n) (* n n)))')
    const labels = (await cell.completions(3)).map((c) => c.label)
    expect(labels).toContain('map')
    expect(labels).not.toContain('sq')
  })

  test('hovers a name the context defined, in the cell\'s own coordinates', async () => {
    const cell = await openCell(
      '(twice 3)',
      ';;; (twice n) -> number?\n;;;   n: number?\n;;; Doubles n.\n(define twice (lambda (n) (* 2 n)))',
    )
    const hover = await cell.hover(2)
    expect(hover?.contents.value).toContain('twice')
    // The range is the token in *this* document, not somewhere off in the
    // context: line 0, not line 3.
    expect(hover?.range).toEqual({
      start: { line: 0, character: 1 },
      end: { line: 0, character: 6 },
    })
  })

  test('still completes the standard library', async () => {
    const cell = await openCell('(ma', '(define sq (lambda (n) (* n n)))')
    const labels = (await cell.completions(3)).map((c) => c.label)
    expect(labels).toContain('map')
  })

  test('a definition out in the context is not offered as a jump', async () => {
    // It is real, but it lives in a document the editor does not have open, so
    // there is nowhere to go.
    const cell = await openCell('(sq 4)', '(define sq (lambda (n) (* n n)))')
    expect(await cell.definition(2)).toBeNull()
  })

  test('a definition inside the cell still is', async () => {
    const cell = await openCell('(define y 1) y', '(define x 1)')
    const found = await cell.definition(13)
    expect(found?.range.start).toEqual({ line: 0, character: 8 })
  })

  test('occurrences in the context are not highlighted here', async () => {
    const cell = await openCell('(sq 4)', '(define sq (lambda (n) (* n n)))')
    const spans = await cell.highlights(2)
    // Only the use in the cell, which is at character 1 of line 0.
    expect(spans.map((s) => s.range.start)).toEqual([{ line: 0, character: 1 }])
  })

  test('a document with no context is unaffected', async () => {
    const cell = await openCell('(car (list 1))', '')
    const hover = await cell.hover(1)
    expect(hover?.contents.value).toContain('car')
    expect(hover?.range).toEqual({
      start: { line: 0, character: 1 },
      end: { line: 0, character: 4 },
    })
  })
})

describe('diagnostics', () => {
  /** Collects whatever the server pushes for `uri`. */
  function watch(server: ScamperLanguageServer): unknown[] {
    const published: unknown[] = []
    server.setSend((message) => {
      const parsed = JSON.parse(message) as { method?: string; params?: unknown }
      if (parsed.method === 'textDocument/publishDiagnostics') {
        published.push(parsed.params)
      }
    })
    return published
  }

  test('a document with a context is not linted', async () => {
    // A cell is unclosed for most of the time it is being typed; squiggles
    // under every keystroke would say nothing except "you are not finished".
    const server = new ScamperLanguageServer()
    const published = watch(server)
    server.setContext(URI, '(define sq (lambda (n) (* n n)))')
    server.handle(
      JSON.stringify({
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
          textDocument: { uri: URI, languageId: 'scheme', version: 1, text: '(sq' },
        },
      }),
    )
    await Promise.resolve()
    expect(published).toEqual([])
  })

  test('a cell whose context is empty is not linted either', async () => {
    // Regression: the guard read the context's *length*, so a REPL opened on an
    // empty file -- nothing in front of the prompt -- was linted like a file,
    // squiggling every half-typed form.
    const server = new ScamperLanguageServer()
    const published = watch(server)
    server.setContext(URI, '')
    server.handle(
      JSON.stringify({
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
          textDocument: { uri: URI, languageId: 'scheme', version: 1, text: '(+ 1' },
        },
      }),
    )
    await new Promise((resolve) => setTimeout(resolve, 20))
    expect(published).toEqual([])
  })

  test('becoming a cell clears whatever was already published', async () => {
    // Otherwise the last squiggle sticks to the document with nothing left to
    // clear it, since a cell is never linted again.
    const server = new ScamperLanguageServer()
    const published = watch(server) as { diagnostics: unknown[] }[]
    server.handle(
      JSON.stringify({
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
          textDocument: { uri: URI, languageId: 'scheme', version: 1, text: '(+ 1' },
        },
      }),
    )
    await new Promise((resolve) => setTimeout(resolve, 20))
    expect(published.at(-1)?.diagnostics.length).toBeGreaterThan(0)

    server.setContext(URI, '(define x 1)')
    expect(published.at(-1)?.diagnostics).toEqual([])
  })

  test('a document without one still is', async () => {
    const server = new ScamperLanguageServer()
    const published = watch(server)
    server.handle(
      JSON.stringify({
        jsonrpc: '2.0',
        method: 'textDocument/didOpen',
        params: {
          textDocument: {
            uri: 'inmemory://main.scm',
            languageId: 'scheme',
            version: 1,
            text: '(sq',
          },
        },
      }),
    )
    await new Promise((resolve) => setTimeout(resolve, 20))
    expect(published.length).toBe(1)
  })
})

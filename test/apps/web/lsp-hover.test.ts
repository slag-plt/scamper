import { afterEach, describe, expect, test } from 'vitest'
import { hoverAt } from '../../../src/app/web/codemirror/lsp/hover'
import { ScamperLanguageServer } from '../../../src/app/web/codemirror/lsp/server'
import { moduleDocRegistry } from '../../../src/lib'

// docRegistry is populated by the global test setup (initializeLibs()).

describe('hoverAt', () => {
  test('renders builtin documentation for the identifier under the cursor', () => {
    const result = hoverAt('(car (list 1 2))', 1)
    expect(result).not.toBeNull()
    expect(result?.contents.kind).toBe('markdown')
    expect(result?.contents.value).toContain('car')
    expect(result?.contents.value).toContain('```scheme')
    expect(result?.from).toBe(1)
    expect(result?.to).toBe(4)
  })

  test('returns null when not on a documented identifier', () => {
    expect(hoverAt('(car 42)', 6)).toBeNull() // on the number 42
  })

  test('documents a user-defined function from its docstring', () => {
    const src = [
      ';;; (double x) -> number?',
      ';;;  x : number?',
      ';;; Doubles `x`.',
      '(define double (lambda (x) (* x 2)))',
      '(double 21)',
    ].join('\n')
    const offset = src.lastIndexOf('double') // the call site
    expect(hoverAt(src, offset)?.contents.value).toContain('Doubles')
  })

  test('documents a qualified reference through its module', () => {
    const src = '(import image img)\n(img.rgb 1 2 3)'
    const offset = src.indexOf('img.rgb') + 5 // on the `rgb` half
    const result = hoverAt(src, offset)
    expect(result).not.toBeNull()
    expect(result?.contents.value).toContain('rgb')
    expect(result?.contents.value).toContain('image') // source-module footer
    // The hover span covers the whole dotted identifier.
    expect(src.slice(result?.from, result?.to)).toBe('img.rgb')
  })

  test('a qualified reference through an unknown alias has no hover', () => {
    const src = '(nope.rgb 1 2 3)'
    expect(hoverAt(src, src.indexOf('nope.rgb') + 1)).toBeNull()
  })
})

describe('ScamperLanguageServer', () => {
  interface Position {
    line: number
    character: number
  }
  interface Reply {
    id?: number
    result: {
      capabilities?: { hoverProvider?: boolean }
      contents?: { value: string; kind?: string }
      range?: { start: Position; end: Position }
    } | null
    error?: { code: number }
  }

  function drive() {
    const server = new ScamperLanguageServer()
    const sent: string[] = []
    server.setSend((m) => {
      sent.push(m)
    })
    const request = (id: number, method: string, params: unknown) => {
      server.handle(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    }
    const notify = (method: string, params: unknown) => {
      server.handle(JSON.stringify({ jsonrpc: '2.0', method, params }))
    }
    const reply = (id: number): Reply => {
      const found = sent
        .map((m) => JSON.parse(m) as Reply)
        .find((m) => m.id === id)
      if (found === undefined) {
        throw new Error(`no reply for request ${String(id)}`)
      }
      return found
    }
    return { request, notify, reply }
  }

  test('answers initialize with hover capability and serves a hover request', () => {
    const { request, notify, reply } = drive()

    request(1, 'initialize', { capabilities: {} })
    expect(reply(1).result?.capabilities?.hoverProvider).toBe(true)

    notify('textDocument/didOpen', {
      textDocument: {
        uri: 'inmemory://main.scm',
        languageId: 'scheme',
        version: 1,
        text: '(car (list 1))',
      },
    })
    request(2, 'textDocument/hover', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 1 },
    })

    const hover = reply(2).result
    expect(hover).not.toBeNull()
    expect(hover?.contents?.value).toContain('car')
    expect(hover?.range).toEqual({
      start: { line: 0, character: 1 },
      end: { line: 0, character: 4 },
    })
  })

  test('tracks edits via didChange before answering a hover', () => {
    const { request, notify, reply } = drive()
    notify('textDocument/didOpen', {
      textDocument: {
        uri: 'inmemory://main.scm',
        languageId: 'scheme',
        version: 1,
        text: '(list 1)',
      },
    })
    notify('textDocument/didChange', {
      textDocument: { uri: 'inmemory://main.scm', version: 2 },
      contentChanges: [{ text: '(car (list 1))' }],
    })
    request(1, 'textDocument/hover', {
      textDocument: { uri: 'inmemory://main.scm' },
      position: { line: 0, character: 2 },
    })
    expect(reply(1).result?.contents?.value).toContain('car')
  })

  test('reports method-not-found for unsupported requests', () => {
    const { request, reply } = drive()
    request(9, 'textDocument/rename', {})
    expect(reply(9).error?.code).toBe(-32601)
  })
})

// Hovering the name in an import says what the module is for (#411). No builtin
// library carries a module comment yet, so these give one to `rex` for the
// duration of a test and take it away again.
describe('hovering an import', () => {
  afterEach(() => {
    moduleDocRegistry.delete('rex')
  })

  function giveRexAComment(description = 'Patterns for pulling text apart.') {
    moduleDocRegistry.set('rex', { description })
  }

  test("shows the module's comment", () => {
    giveRexAComment()
    const src = '(import rex)'
    const result = hoverAt(src, src.indexOf('rex') + 1)
    expect(result).not.toBeNull()
    expect(result?.contents.value).toContain('Patterns for pulling text apart.')
    expect(result?.contents.value).toContain('(import rex)')
    expect(src.slice(result?.from, result?.to)).toBe('rex')
  })

  test('shows it for the aliased form too', () => {
    giveRexAComment()
    const src = '(import rex r)'
    expect(hoverAt(src, src.indexOf('rex') + 1)?.contents.value).toContain(
      'Patterns for pulling text apart.',
    )
  })

  test('shows it when hovering the alias', () => {
    giveRexAComment()
    const src = '(import rex r)'
    expect(hoverAt(src, src.indexOf(' r)') + 1)?.contents.value).toContain(
      'Patterns for pulling text apart.',
    )
  })

  test('a module with no comment has no hover', () => {
    // The shipped state: nothing to say, so nothing is said.
    expect(hoverAt('(import rex)', 9)).toBeNull()
  })

  test('the module reading is confined to the import', () => {
    // `rex` used elsewhere is an ordinary name, not the module -- otherwise a
    // module whose name is also a function's would shadow it file-wide.
    giveRexAComment()
    const src = '(import rex)\n(rex-empty)'
    const result = hoverAt(src, src.indexOf('(rex-empty') + 1)
    expect(result?.contents.value).not.toContain('Patterns for pulling')
    expect(result?.contents.value).toContain('rex-empty')
  })

  test('hovering a function is unaffected', () => {
    giveRexAComment()
    expect(hoverAt('(car (list 1 2))', 1)?.contents.value).toContain('car')
  })
})

import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { EditorView } from '@codemirror/view'
import { completionStatus, startCompletion } from '@codemirror/autocomplete'
import type { SignatureHelp } from 'vscode-languageserver-protocol'
import { EditorState } from '@codemirror/state'
import { mkFreshEditorState } from '../../src/app/web/codemirror/codemirror'
import { mkCellEditorState } from '../../src/app/web/codemirror/cell-editor'
import { scamperMode } from '../../src/app/web/codemirror/modes'
import { setAutoSuggest } from '../../src/app/web/editor-prefs'
import { ScamperLanguageServer } from '../../src/app/web/codemirror/lsp/server'
import { initialize } from '../../src/scamper'

// Regression test for #449: the completion popup and the parameter-help
// tooltip used to appear on their own -- the popup a moment after typing, the
// tooltip on every `(` and every space -- which is a lot of movement over a
// beginner's code. Both are now offered on their keys (Ctrl+Space and
// Ctrl+Shift+Space) unless the student turns the automatic ones back on.

await initialize()

const AUTO_SUGGEST_KEY = 'scamper.editor.autoSuggest'

/** A mounted editor, since a completion only starts inside a view. */
function mount(state: EditorState): EditorView {
  const parent = document.createElement('div')
  document.body.appendChild(parent)
  return new EditorView({ state, parent })
}

/** The file editor's state, with the parts a test has no use for stubbed. */
function fileState(doc: string): EditorState {
  return mkFreshEditorState(doc, {
    dirtyAction: () => {
      /* nothing here tracks unsaved changes */
    },
    isReadOnly: false,
    mode: scamperMode,
  })
}

/** Types `text` at the end of the document, as a person would. */
function type(view: EditorView, text: string): void {
  const at = view.state.doc.length
  view.dispatch({
    changes: { from: at, insert: text },
    selection: { anchor: at + text.length },
    userEvent: 'input.type',
  })
  // Past activateOnTypingDelay, so a popup that was going to open has.
  vi.advanceTimersByTime(500)
}

describe('#449: suggestions while typing are opt-in', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    setAutoSuggest(false)
  })

  afterEach(() => {
    vi.useRealTimers()
    setAutoSuggest(false)
    document.body.innerHTML = ''
  })

  test('the preference starts out off', async () => {
    // Fresh modules, or this reads the same module-level ref the tests here
    // have been setting and proves nothing about the initializer.
    localStorage.removeItem(AUTO_SUGGEST_KEY)
    vi.resetModules()
    const fresh = await import('../../src/app/web/editor-prefs')
    expect(fresh.autoSuggest.value).toBe(false)
  })

  test('the choice is written down and read back', async () => {
    setAutoSuggest(true)
    expect(localStorage.getItem(AUTO_SUGGEST_KEY)).toBe('true')
    vi.resetModules()
    const fresh = await import('../../src/app/web/editor-prefs')
    expect(fresh.autoSuggest.value).toBe(true)
  })

  test('typing in the editor opens nothing', () => {
    const view = mount(fileState(''))
    try {
      type(view, 'ca')
      expect(completionStatus(view.state)).toBeNull()
    } finally {
      view.destroy()
    }
  })

  test('Ctrl+Space still asks for completions', () => {
    const view = mount(fileState('ca'))
    try {
      view.dispatch({ selection: { anchor: 2 } })
      startCompletion(view)
      vi.advanceTimersByTime(500)
      expect(completionStatus(view.state)).not.toBeNull()
    } finally {
      view.destroy()
    }
  })

  test('turning the preference on brings the popup back', () => {
    setAutoSuggest(true)
    const view = mount(fileState(''))
    try {
      type(view, 'ca')
      expect(completionStatus(view.state)).not.toBeNull()
    } finally {
      view.destroy()
    }
  })

  test('a REPL or notebook cell follows the same preference', () => {
    const quiet = mount(
      mkCellEditorState('', { lspUri: 'inmemory://cell-449-a.scm' }),
    )
    try {
      type(quiet, 'ca')
      expect(completionStatus(quiet.state)).toBeNull()
    } finally {
      quiet.destroy()
    }

    setAutoSuggest(true)
    const loud = mount(
      mkCellEditorState('', { lspUri: 'inmemory://cell-449-b.scm' }),
    )
    try {
      type(loud, 'ca')
      expect(completionStatus(loud.state)).not.toBeNull()
    } finally {
      loud.destroy()
    }
  })
})

describe('#449: parameter help while typing is opt-in', () => {
  /** Drives the language server directly, as test/apps/web/lsp-features does. */
  function serve(text: string, automatic: boolean) {
    const server = new ScamperLanguageServer({
      automaticSignatureHelp: () => automatic,
    })
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
    const reply = (id: number): unknown => {
      const found = sent.find((m) => m.id === id)
      if (found === undefined) throw new Error(`no reply to request ${id}`)
      return found.result
    }
    return { request, reply }
  }

  const at = {
    textDocument: { uri: 'inmemory://main.scm' },
    position: { line: 0, character: 5 },
  }
  // What the client sends after a typed `(` or space, per the trigger
  // characters the server advertises.
  const typed = { triggerKind: 2, triggerCharacter: ' ', isRetrigger: false }
  // What Ctrl+Shift+Space sends.
  const invoked = { triggerKind: 1, isRetrigger: false }

  test('a typed trigger character is ignored while the preference is off', () => {
    const { request, reply } = serve('(car )', false)
    request(2, 'textDocument/signatureHelp', { ...at, context: typed })
    expect(reply(2)).toBeNull()
  })

  test('an explicit request is answered regardless', () => {
    const { request, reply } = serve('(car )', false)
    request(2, 'textDocument/signatureHelp', { ...at, context: invoked })
    expect((reply(2) as SignatureHelp).signatures[0].label).toContain('car')
  })

  test('a typed trigger character is answered once the preference is on', () => {
    const { request, reply } = serve('(car )', true)
    request(2, 'textDocument/signatureHelp', { ...at, context: typed })
    expect((reply(2) as SignatureHelp).signatures[0].label).toContain('car')
  })

  test('a tooltip already showing keeps following the cursor', () => {
    // triggerKind 3 (ContentChange) is only sent while a tooltip is open, so
    // it is the manually opened one being kept up to date, never a new one.
    const { request, reply } = serve('(car )', false)
    request(2, 'textDocument/signatureHelp', {
      ...at,
      context: { triggerKind: 3, isRetrigger: true },
    })
    expect((reply(2) as SignatureHelp).signatures[0].label).toContain('car')
  })
})

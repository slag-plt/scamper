import type {
  CompletionItem,
  CompletionParams,
  DefinitionParams,
  DidChangeTextDocumentParams,
  DidCloseTextDocumentParams,
  DidOpenTextDocumentParams,
  Hover,
  HoverParams,
  InitializeResult,
  Location,
  ReferenceParams,
  ServerCapabilities,
  SignatureHelp,
  SignatureHelpParams,
  TextDocumentContentChangeEvent,
} from 'vscode-languageserver-protocol'
import { hoverAt } from './hover'
import { completionsFor } from './completion'
import { signatureHelpAt } from './signature'
import { definitionAt } from './definition'
import { referencesAt } from './references'
import { computeDiagnostics } from './diagnostics'
import {
  computeLineStarts,
  positionToOffset,
  rangeFromOffsets,
} from './positions'

/** A document tracked by the server, with line starts cached for position math. */
interface TrackedDoc {
  version: number
  text: string
  lineStarts: number[]
}

/** JSON-RPC error codes. */
const METHOD_NOT_FOUND = -32601
const INTERNAL_ERROR = -32603

/**
 * An in-process LSP server backed directly by Scamper's language services.
 * It answers the subset of the protocol the CodeMirror LSP client needs,
 * synchronously on the main thread -- no worker, wasm, or socket. Messages
 * are bare JSON-RPC strings (no LSP headers); responses are pushed back out
 * through the {@link setSend} callback.
 */
export class ScamperLanguageServer {
  private readonly docs = new Map<string, TrackedDoc>()
  private send: (message: string) => void = () => {
    /* replaced by the transport via setSend */
  }

  /** Registers the callback used to deliver responses/notifications to the client. */
  setSend(send: (message: string) => void): void {
    this.send = send
  }

  /** Dispatches a single incoming JSON-RPC message. */
  handle(message: string): void {
    let parsed: unknown
    try {
      parsed = JSON.parse(message)
    } catch {
      return
    }
    if (typeof parsed !== 'object' || parsed === null) {
      return
    }
    const msg = parsed as {
      id?: number | string | null
      method?: string
      params?: unknown
    }
    // We only ever receive requests and notifications (we never send requests
    // to the client), so anything without a method is a stray we can ignore.
    if (typeof msg.method !== 'string') {
      return
    }
    if (msg.id !== undefined && msg.id !== null) {
      this.handleRequest(msg.id, msg.method, msg.params)
    } else {
      this.handleNotification(msg.method, msg.params)
    }
  }

  private handleRequest(
    id: number | string,
    method: string,
    params: unknown,
  ): void {
    switch (method) {
      case 'initialize':
        this.respond(id, this.initializeResult())
        break
      case 'shutdown':
        this.respond(id, null)
        break
      case 'textDocument/hover':
        this.respond(id, this.hover(params as HoverParams))
        break
      case 'textDocument/completion':
        this.respondAsync(id, this.completion(params as CompletionParams))
        break
      case 'textDocument/signatureHelp':
        this.respond(id, this.signatureHelp(params as SignatureHelpParams))
        break
      case 'textDocument/definition':
        this.respondAsync(id, this.definition(params as DefinitionParams))
        break
      case 'textDocument/references':
        this.respondAsync(id, this.references(params as ReferenceParams))
        break
      default:
        this.respondError(id, METHOD_NOT_FOUND, `Method not found: ${method}`)
    }
  }

  private handleNotification(method: string, params: unknown): void {
    switch (method) {
      case 'textDocument/didOpen':
        this.didOpen(params as DidOpenTextDocumentParams)
        break
      case 'textDocument/didChange':
        this.didChange(params as DidChangeTextDocumentParams)
        break
      case 'textDocument/didClose':
        this.didClose(params as DidCloseTextDocumentParams)
        break
      // `initialized`, `exit`, `$/setTrace`, `$/cancelRequest`, etc. need no action.
    }
  }

  private initializeResult(): InitializeResult {
    const capabilities: ServerCapabilities = {
      // Full sync: each change carries the whole document (see didChange).
      textDocumentSync: 1,
      hoverProvider: true,
      completionProvider: { resolveProvider: false },
      signatureHelpProvider: { triggerCharacters: ['(', ' '] },
      definitionProvider: true,
      referencesProvider: true,
    }
    return { capabilities, serverInfo: { name: 'scamper-lsp', version: '0.1.0' } }
  }

  private hover(params: HoverParams): Hover | null {
    const doc = this.docs.get(params.textDocument.uri)
    if (doc === undefined) {
      return null
    }
    const offset = positionToOffset(params.position, doc.lineStarts, doc.text.length)
    const result = hoverAt(doc.text, offset)
    if (result === null) {
      return null
    }
    return {
      contents: result.contents,
      range: rangeFromOffsets(result.from, result.to, doc.lineStarts),
    }
  }

  private async completion(params: CompletionParams): Promise<CompletionItem[]> {
    const doc = this.docs.get(params.textDocument.uri)
    if (doc === undefined) {
      return []
    }
    const offset = positionToOffset(params.position, doc.lineStarts, doc.text.length)
    return completionsFor(doc.text, offset)
  }

  private signatureHelp(params: SignatureHelpParams): SignatureHelp | null {
    const doc = this.docs.get(params.textDocument.uri)
    if (doc === undefined) {
      return null
    }
    const offset = positionToOffset(params.position, doc.lineStarts, doc.text.length)
    return signatureHelpAt(doc.text, offset)
  }

  private async definition(params: DefinitionParams): Promise<Location | null> {
    const doc = this.docs.get(params.textDocument.uri)
    if (doc === undefined) {
      return null
    }
    const offset = positionToOffset(params.position, doc.lineStarts, doc.text.length)
    const span = await definitionAt(doc.text, offset)
    if (span === null) {
      return null
    }
    return {
      uri: params.textDocument.uri,
      range: rangeFromOffsets(span.from, span.to, doc.lineStarts),
    }
  }

  private async references(params: ReferenceParams): Promise<Location[]> {
    const doc = this.docs.get(params.textDocument.uri)
    if (doc === undefined) {
      return []
    }
    const offset = positionToOffset(params.position, doc.lineStarts, doc.text.length)
    const spans = await referencesAt(doc.text, offset)
    return spans.map((span) => ({
      uri: params.textDocument.uri,
      range: rangeFromOffsets(span.from, span.to, doc.lineStarts),
    }))
  }

  private didOpen(params: DidOpenTextDocumentParams): void {
    const { uri, text, version } = params.textDocument
    this.docs.set(uri, track(text, version))
    this.publishDiagnostics(uri)
  }

  private didChange(params: DidChangeTextDocumentParams): void {
    const existing = this.docs.get(params.textDocument.uri)
    if (existing === undefined) {
      return
    }
    let text = existing.text
    for (const change of params.contentChanges) {
      text = applyChange(text, change)
    }
    this.docs.set(
      params.textDocument.uri,
      track(text, params.textDocument.version),
    )
    this.publishDiagnostics(params.textDocument.uri)
  }

  /** Computes diagnostics for a document and pushes them to the client. */
  private publishDiagnostics(uri: string): void {
    const doc = this.docs.get(uri)
    if (doc === undefined) {
      return
    }
    const { text, lineStarts, version } = doc
    void computeDiagnostics(text, lineStarts).then((diagnostics) => {
      this.notify('textDocument/publishDiagnostics', { uri, version, diagnostics })
    })
  }

  private notify(method: string, params: unknown): void {
    this.send(JSON.stringify({ jsonrpc: '2.0', method, params }))
  }

  private didClose(params: DidCloseTextDocumentParams): void {
    this.docs.delete(params.textDocument.uri)
  }

  private respond(id: number | string, result: unknown): void {
    this.send(JSON.stringify({ jsonrpc: '2.0', id, result }))
  }

  /** Responds once [result] settles; a rejection becomes a JSON-RPC error. */
  private respondAsync(id: number | string, result: Promise<unknown>): void {
    result.then(
      (value) => {
        this.respond(id, value)
      },
      (err: unknown) => {
        this.respondError(id, INTERNAL_ERROR, err instanceof Error ? err.message : String(err))
      },
    )
  }

  private respondError(
    id: number | string,
    code: number,
    message: string,
  ): void {
    this.send(JSON.stringify({ jsonrpc: '2.0', id, error: { code, message } }))
  }
}

function track(text: string, version: number): TrackedDoc {
  return { version, text, lineStarts: computeLineStarts(text) }
}

/**
 * Applies one content change. We advertise Full sync, so changes normally
 * carry the whole document; the incremental (ranged) branch is a defensive
 * fallback in case a client sends one anyway.
 */
function applyChange(text: string, change: TextDocumentContentChangeEvent): string {
  if ('range' in change) {
    const lineStarts = computeLineStarts(text)
    const from = positionToOffset(change.range.start, lineStarts, text.length)
    const to = positionToOffset(change.range.end, lineStarts, text.length)
    return text.slice(0, from) + change.text + text.slice(to)
  }
  return change.text
}

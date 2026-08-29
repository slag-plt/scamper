import type { Bytes } from './fs'
import type { WriteReply, WriteRequest } from './opfs-writer'

/**
 * The worker half of `opfs-writer.ts`: it writes OPFS files for browsers whose
 * file handles have no `createWritable` (#429).
 *
 * A sync access handle is the write those browsers do have, and the spec puts
 * it in a dedicated worker -- its methods block, which is the point of them and
 * the reason a window may not call them. Nothing else lives here: the worker
 * takes a name and bytes, and answers with success or a message.
 */

/** `self`, as the worker global it is rather than the window `lib.dom` assumes. */
const worker = self as unknown as DedicatedWorkerGlobalScope

/**
 * A sync access handle, as the oldest browsers in range implement it.
 *
 * An earlier draft of the spec had these methods return promises, and Safari
 * did so until 16.4 -- inside the range this fallback exists for, so the types
 * `lib.webworker` gives (values, per the current spec) are not the whole story.
 * Saying "either" is what lets one `await` cover both: awaiting a plain value
 * simply resolves to it.
 */
interface SyncAccessHandle {
  truncate: (to: number) => Promise<void> | void
  write: (bytes: Bytes, options: { at: number }) => Promise<number> | number
  flush: () => Promise<void> | void
  close: () => Promise<void> | void
}

/**
 * Writes run one at a time.
 *
 * A sync access handle is an exclusive lock on its file, so a second write that
 * opens one while the first still holds it fails outright. Autosave and a
 * history snapshot land together often enough that this is a real race, and a
 * queue is cheaper than teaching every caller to wait.
 */
let queue: Promise<void> = Promise.resolve()

/**
 * Replaces `filename`'s contents with `bytes`, creating it if need be.
 *
 * Not atomic, unlike the writable stream this stands in for: that one fills a
 * swap file and swaps it in at the end, whereas a sync access handle edits the
 * file itself. A write interrupted between the truncate and the write leaves
 * the file empty rather than as it was. There is no better option -- OPFS has
 * no rename to build one from (see `renameFile` in opfs.ts) -- so a failure
 * here is reported to the student rather than hidden.
 */
async function write({ filename, bytes }: WriteRequest): Promise<void> {
  const root = await navigator.storage.getDirectory()
  const handle = await root.getFileHandle(filename, { create: true })
  const access: SyncAccessHandle = await handle.createSyncAccessHandle()
  try {
    // Truncate first: without it a shorter save leaves the tail of the longer
    // version it replaced, since a write only overwrites what it covers.
    await access.truncate(0)
    const written = await access.write(bytes, { at: 0 })
    if (written !== bytes.length) {
      throw new Error(`only ${written} of ${bytes.length} bytes could be written`)
    }
    await access.flush()
  } finally {
    await access.close()
  }
}

/** Performs `request` and answers it, whether it succeeded or failed. */
async function serve(request: WriteRequest): Promise<void> {
  let reply: WriteReply = { id: request.id }
  try {
    await write(request)
  } catch (e) {
    reply = { id: request.id, error: e instanceof Error ? e.message : String(e) }
  }
  worker.postMessage(reply)
}

worker.onmessage = (event: MessageEvent<WriteRequest>) => {
  const next = () => serve(event.data)
  // Both hands are the same one on purpose: a queue that had rejected would
  // skip every callback chained after it, and no write would ever be answered
  // again.
  queue = queue.then(next, next)
}

import type { WriteReply, WriteRequest } from './opfs-writer'

/**
 * The worker half of {@link ../opfs-writer}: it writes OPFS files for browsers
 * whose file handles have no `createWritable` (#429).
 *
 * A sync access handle is the write those browsers do have, and the spec puts
 * it in a dedicated worker -- its methods block, which is the whole point of
 * them and the reason a window may not call them. Nothing else lives here: the
 * worker takes a name and bytes, and answers with success or a message.
 */

/** `self`, as the worker global it is rather than the window `lib.dom` assumes. */
const worker = self as unknown as DedicatedWorkerGlobalScope

/**
 * Writes run one at a time.
 *
 * A sync access handle is an exclusive lock on its file, so a second write
 * that opens one while the first still holds it fails outright. Autosave and a
 * history snapshot land together often enough that this is a real race, and a
 * queue is cheaper than teaching every caller to wait.
 */
let queue: Promise<void> = Promise.resolve()

/** Replaces `filename`'s contents with `bytes`, creating it if need be. */
async function write({ filename, bytes }: WriteRequest): Promise<void> {
  const root = await navigator.storage.getDirectory()
  const handle = await root.getFileHandle(filename, { create: true })
  const access = await handle.createSyncAccessHandle()
  try {
    // Truncate first: without it a shorter save leaves the tail of the longer
    // version it replaced, since a write only overwrites what it covers.
    access.truncate(0)
    access.write(bytes, { at: 0 })
    access.flush()
  } finally {
    access.close()
  }
}

worker.onmessage = (event: MessageEvent<WriteRequest>) => {
  const request = event.data
  const reply = (r: WriteReply) => { worker.postMessage(r) }
  queue = queue.then(() =>
    write(request).then(
      () => { reply({ id: request.id }) },
      (e: unknown) => {
        reply({ id: request.id, error: e instanceof Error ? e.message : String(e) })
      },
    ),
  )
}

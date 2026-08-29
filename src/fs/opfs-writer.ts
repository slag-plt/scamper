import type { Bytes } from './fs'

/** A write asked of the worker: `filename`'s contents become `bytes`. */
export interface WriteRequest {
  id: number
  filename: string
  bytes: Bytes
}

/** The worker's answer to a {@link WriteRequest}; `error` iff it failed. */
export interface WriteReply {
  id: number
  error?: string
}

/** A write waiting on the worker, and what to tell its caller. */
interface Pending {
  filename: string
  resolve: () => void
  reject: (reason: Error) => void
}

/**
 * Writing to OPFS in a browser that cannot write to it from the main thread.
 *
 * Safari got `createWritable` only in version 26 (#429), and before that a
 * sync access handle was its one way to write a file -- which the spec allows
 * only inside a dedicated worker, its methods being blocking ones. So this
 * hands the write to a worker and waits for the answer.
 *
 * One worker for the whole origin, started by the first write that needs one.
 * A browser with `createWritable` never starts one, and neither does the CLI,
 * which shares this module's import graph but runs under Node.
 */
class OPFSWriter {
  private worker?: Promise<Worker>
  private nextId = 0
  private readonly pending = new Map<number, Pending>()

  /**
   * Writes `bytes` to `filename`, creating the file if it does not exist.
   * @returns a promise that resolves once the file has been written
   */
  write(filename: string, bytes: Bytes): Promise<void> {
    const id = this.nextId++
    return new Promise<void>((resolve, reject) => {
      this.pending.set(id, { filename, resolve, reject })
      const request: WriteRequest = { id, filename, bytes }
      this.channel().then(
        (worker) => { worker.postMessage(request) },
        // A worker that will not start is the end of every write, so say so
        // here rather than leaving the caller waiting on it.
        (e: unknown) => { this.settle(id, `the file writer could not start (${String(e)})`) },
      )
    })
  }

  /**
   * @returns the worker, starting it on the first call. Requests posted to it
   *          arrive in the order they were made, since they all wait on this
   *          one promise.
   */
  private channel(): Promise<Worker> {
    this.worker ??= this.start()
    return this.worker
  }

  /** @returns a started worker, listening for its replies. */
  private async start(): Promise<Worker> {
    // Imported here rather than at the top of the file so that a host with no
    // OPFS never loads it. `?worker&inline` is Vite's, and means the worker is
    // built and inlined as a blob rather than emitted beside the bundle:
    // `dist/scamper-embed.js` is one self-contained file a reading on another
    // site includes by URL (see vite.config.embed.ts), and a worker it had to
    // fetch relative to itself would not survive the trip.
    const { default: WriterWorker } = await import('./opfs-writer.worker?worker&inline')
    const worker = new WriterWorker()
    worker.onmessage = (event: MessageEvent<WriteReply>) => {
      const { id, error } = event.data
      this.settle(id, error)
    }
    // A worker that dies takes every write in flight with it, so say so rather
    // than leaving those callers waiting forever. The next write starts a
    // fresh one.
    worker.onerror = () => { this.fail('the file writer stopped unexpectedly') }
    worker.onmessageerror = () => { this.fail('the file writer sent a reply that could not be read') }
    return worker
  }

  /** Completes the write `id` belongs to, failed with `error` if given. */
  private settle(id: number, error?: string): void {
    const waiter = this.pending.get(id)
    if (waiter === undefined) return
    this.pending.delete(id)
    if (error === undefined) {
      waiter.resolve()
    } else {
      waiter.reject(new Error(`could not save ${waiter.filename}: ${error}`))
    }
  }

  /** Fails every write in flight and discards the worker they were sent to. */
  private fail(error: string): void {
    void this.worker?.then((worker) => { worker.terminate() })
    this.worker = undefined
    for (const id of [...this.pending.keys()]) {
      this.settle(id, error)
    }
  }
}

/** The origin's writer; see {@link OPFSWriter}. */
export const opfsWriter = new OPFSWriter()

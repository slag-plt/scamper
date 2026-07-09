declare global {
  interface Scheduler {
    yield(): Promise<void>
  }
  // eslint-disable-next-line no-var
  var scheduler: Scheduler | undefined
}

/**
 * Yields control back to the event loop between scheduler execution rounds.
 * Prefers the browser's native Prioritized Task Scheduling API when present;
 * falls back to a MessageChannel-based macrotask otherwise (Node, jsdom,
 * browsers without native support). Unlike setTimeout, MessageChannel isn't
 * subject to the 4ms-nesting/background-tab throttling clamps, which is why
 * schedulers like React's use the same fallback.
 */
export function schedulerYield(): Promise<void> {
  if (typeof scheduler !== "undefined" && typeof scheduler.yield === "function") {
    return scheduler.yield()
  }
  return new Promise((resolve) => {
    const channel = new MessageChannel()
    channel.port1.onmessage = () => {
      // N.B., in Node, an open MessagePort counts as an active handle and
      // keeps the event loop (and thus the process) alive indefinitely, so
      // close both ends once the round trip completes.
      channel.port1.close()
      channel.port2.close()
      resolve()
    }
    channel.port2.postMessage(undefined)
  })
}

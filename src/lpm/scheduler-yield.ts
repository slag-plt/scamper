interface YieldingScheduler {
  yield(): Promise<void>
}

// Feature support doesn't change at runtime, so detect once at module load
// instead of on every call. Absent in Node, so this is `undefined` for the
// CLI, which always takes the MessageChannel fallback below.
const nativeScheduler = (
  globalThis as { scheduler?: YieldingScheduler }
).scheduler

/**
 * Yields control back to the event loop between scheduler execution rounds.
 * Prefers the browser's native Prioritized Task Scheduling API when present;
 * falls back to a MessageChannel-based macrotask otherwise (Node, jsdom,
 * browsers without native support). Unlike setTimeout, MessageChannel isn't
 * subject to the 4ms-nesting/background-tab throttling clamps, which is why
 * schedulers like React's use the same fallback.
 */
export function schedulerYield(): Promise<void> {
  if (nativeScheduler && typeof nativeScheduler.yield === "function") {
    return nativeScheduler.yield()
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

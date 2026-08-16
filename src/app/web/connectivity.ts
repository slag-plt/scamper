// Whether the file server is reachable right now.
//
// A student's wifi drops, a laptop lid closes, the server restarts during a
// deploy. Before this, the first file operation after any of those put an error
// on top of the editor that could not be dismissed. So the IDE keeps a
// heartbeat instead: it knows it is offline *before* the student clicks
// anything, says so, and stops autosaving into a void.
//
// What this does *not* do is keep working offline. Every file operation is
// refused while the server is away, and edits live in the editor buffer and
// nowhere else, so a closed tab loses them -- hence the warning in the modal.
// Mirroring a user's files client-side so those operations keep working is
// issue #364; persisting just the open buffer, the subset of that worth
// shipping on its own, is #363.
//
// This tracks reachability, not the browser's own idea of connectivity.
// `navigator.onLine` answers "is there a network interface", which is true on a
// hotel wifi that has not been paid for and true when the server itself is
// down. Its events are still worth listening to -- they are a free hint that
// something changed -- but the answer always comes from asking the server.

import { ref, type Ref } from 'vue'

/** Whether the server answered the last time it was asked. */
export type Connection = 'online' | 'offline'

/**
 * How often the server is asked while it is answering.
 *
 * Long, because nothing depends on noticing a drop quickly: the operations that
 * need the server report their own failure the moment they hit one, and that
 * failure marks us offline. This exists to notice the *quiet* cases -- an idle
 * tab whose session outlived the server -- and to notice recovery.
 */
const ONLINE_INTERVAL_MS = 20_000

/**
 * How often it is asked while it is not answering.
 *
 * Shorter, because now someone is waiting: a student who reconnects wants the
 * IDE to start saving again without having to reload the page.
 */
const OFFLINE_INTERVAL_MS = 5_000

/**
 * How long a heartbeat waits before giving up.
 *
 * Without this a captive portal that accepts the connection and answers nothing
 * would leave the probe pending forever, and the IDE would sit on a stale
 * "online" indefinitely.
 */
const PROBE_TIMEOUT_MS = 5_000

const state = ref<Connection>('online')

/** The current connection state. Always `online` where there is no server. */
export const connection: Ref<Connection> = state

// Null when no heartbeat is running, which is every deployment without a file
// server -- the default `npm run dev` checkout included.
let healthUrl: string | null = null
let timer: ReturnType<typeof setTimeout> | null = null

/** Notified whenever the state changes, so the app can react to a transition. */
const listeners = new Set<(state: Connection) => void>()

/**
 * Records the connection state, notifying listeners only when it changes.
 *
 * The guard matters: the heartbeat sets this every few seconds, and a watcher
 * that restarted autosave on every beat would defeat the point of pausing it.
 */
function setState(next: Connection): void {
  if (state.value === next) return
  state.value = next
  for (const listener of listeners) listener(next)
}

/**
 * Subscribes to connection changes.
 * @returns a function that unsubscribes
 */
export function onConnectionChange(
  listener: (state: Connection) => void,
): () => void {
  listeners.add(listener)
  return () => listeners.delete(listener)
}

/**
 * Marks the server unreachable without waiting for the next heartbeat.
 *
 * Called when a real request fails: that request is a better probe than any
 * heartbeat, since it just tried the exact thing the student wanted.
 */
export function reportUnreachable(): void {
  setState('offline')
  // Reschedule so the recovery poll starts from this failure rather than
  // whenever the next slow beat happened to be due.
  schedule()
}

/**
 * Asks the server whether it is there.
 *
 * `/health` is the one route that answers without a session (see
 * `server/src/api.ts`), which is what makes it usable as a heartbeat both
 * before anyone has signed in and after a session has lapsed.
 *
 * @returns the state this answer implies
 */
export async function checkNow(): Promise<Connection> {
  const url = healthUrl
  if (url === null) return 'online'

  let answered: boolean
  try {
    const response = await fetch(url, {
      cache: 'no-store',
      signal: AbortSignal.timeout(PROBE_TIMEOUT_MS),
    })
    // A 502 from the proxy in front of the server is as offline as a refused
    // connection: something answered, but it was not the server. `fetchServer`
    // draws the same line for real requests, deliberately.
    answered = response.ok
  } catch {
    answered = false
  }

  // Checked again rather than trusting the `url` captured above: `stop()` may
  // have run while this probe was in flight, and recording its answer now would
  // put a torn-down module back into a state nothing is watching or clearing.
  if (healthUrl === null) return 'online'

  setState(answered ? 'online' : 'offline')
  return state.value
}

/** Queues the next heartbeat at the interval the current state calls for. */
function schedule(): void {
  if (timer !== null) clearTimeout(timer)
  if (healthUrl === null) return

  timer = setTimeout(
    () => {
      void checkNow().finally(schedule)
    },
    state.value === 'online' ? ONLINE_INTERVAL_MS : OFFLINE_INTERVAL_MS,
  )
}

// The browser's own signals, used as hints. `offline` is trustworthy in one
// direction only -- no interface really does mean no server -- so it is taken
// at its word, while `online` only prompts a fresh ask.
function handleBrowserOffline(): void {
  reportUnreachable()
}

function handleBrowserOnline(): void {
  void checkNow().finally(schedule)
}

/**
 * Starts watching the server named by `serverUrl` (the API root, e.g.
 * `/api/v1`). Idempotent, and a no-op for a deployment with no server: the
 * state then stays `online`, since nothing can be out of reach.
 */
export function start(serverUrl: string): void {
  healthUrl = `${serverUrl.replace(/\/+$/, '')}/health`
  window.addEventListener('online', handleBrowserOnline)
  window.addEventListener('offline', handleBrowserOffline)
  // Asked immediately rather than one interval from now: a tab opened while the
  // server is down should say so before the student tries to save.
  void checkNow().finally(schedule)
}

/** Stops watching, e.g. when the IDE unmounts. */
export function stop(): void {
  if (timer !== null) clearTimeout(timer)
  timer = null
  healthUrl = null
  window.removeEventListener('online', handleBrowserOnline)
  window.removeEventListener('offline', handleBrowserOffline)
  setState('online')
}

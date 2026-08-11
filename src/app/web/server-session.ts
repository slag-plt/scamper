import * as FS from '../../fs'
import { loadServerConfig } from '../../fs/config'
import {
  createClient,
  currentUser,
  signInMethods,
  type AuthClient,
  type SessionUser,
  type SignInMethods,
} from './auth-client'

/**
 * What this deployment offers, once startup has looked.
 *
 * Absent means there is no file server at all -- no `/config.json` -- which is
 * an ordinary `npm run dev` checkout and every deployment until one is
 * configured. The IDE then behaves exactly as it always has.
 */
export interface ServerSession {
  client: AuthClient
  methods: SignInMethods
  /** Null when a server exists but nobody is signed in to it. */
  user: SessionUser | null
}

let session: ServerSession | null = null

/** @returns the server this deployment uses, or null if it has none */
export function serverSession(): ServerSession | null {
  return session
}

/**
 * Picks the file system before the app mounts.
 *
 * Signing in is what moves a user's files onto the server; a configured server
 * on its own only means there is one to sign in to. So the rule is:
 *
 * - no `/config.json`      -> local storage, as ever
 * - server, not signed in  -> local storage, and offer to sign in
 * - server, signed in      -> that user's files on the server
 *
 * The exception is a server reporting *no* sign-in methods, which is the
 * in-memory development stub (`SCAMPER_STUB=1`): it has no accounts, so there
 * is nothing to sign in to and its one namespace is used directly. A real
 * server cannot be in that state -- it refuses to start without a database.
 */
export async function initializeBackend(): Promise<void> {
  const config = await loadServerConfig()
  if (config === null) return

  const methods = await signInMethods(config.serverUrl)
  const client = createClient(config.serverUrl)

  if (!methods.password) {
    // The development stub: open, and the only thing it can be.
    FS.setBackend(FS.serverBackend(config.serverUrl))
    session = { client, methods, user: null }
    return
  }

  const user = await currentUser(client)
  if (user !== null) {
    FS.setBackend(FS.serverBackend(config.serverUrl))
  }
  session = { client, methods, user }
}

/**
 * Restarts the app after the signed-in user changes.
 *
 * A reload rather than swapping the backend in place. Switching mid-session
 * would have to reconcile an open file, an autosave timer, and a file drawer
 * that all belong to the storage being replaced -- and it happens twice in a
 * session at most, on a deliberate click. Starting over is both simpler and
 * harder to get wrong.
 */
export function restart(): void {
  window.location.reload()
}

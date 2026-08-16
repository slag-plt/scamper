import * as FS from '../../fs'
import { loadServerConfig } from '../../fs/config'
import * as Connectivity from './connectivity'
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
let filesOnServer = false

/** @returns the server this deployment uses, or null if it has none */
export function serverSession(): ServerSession | null {
  return session
}

/**
 * @returns true iff this tab's files live on the server rather than in the
 *          browser
 *
 * Not the same question as "is there a server": a signed-out user on a
 * server-backed deployment still keeps their files locally, so losing the
 * server costs them nothing and nothing should be blocked on their behalf.
 */
export function usesServerFiles(): boolean {
  return filesOnServer
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
  if (methods === null) {
    // The server did not answer, or answered with something that is not a list
    // of sign-in methods. Staying on local storage is the safe reading: moving
    // onto a server we cannot even ask about strands the user on a file system
    // that answers 401 to everything, and the sign-in dialog would have no form
    // to offer because we never learned there was a password method.
    return
  }

  const client = createClient(config.serverUrl)

  // The server answered once, so it is worth watching from here on: a heartbeat
  // is how the IDE knows it has gone away without waiting for a save to fail.
  // Started for a signed-out user too, since signing in needs it as much as
  // saving does.
  Connectivity.start(config.serverUrl)

  if (!methods.password) {
    // A server offering no way in at all is the development stub, which has no
    // accounts. Reached only on a *successful* reply saying so.
    FS.setBackend(FS.serverBackend(config.serverUrl))
    filesOnServer = true
    session = { client, methods, user: null }
    return
  }

  const user = await currentUser(client)
  if (user !== null) {
    FS.setBackend(FS.serverBackend(config.serverUrl))
    filesOnServer = true
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

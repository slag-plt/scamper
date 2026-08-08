import * as FS from '../../fs'
import { loadServerConfig } from '../../fs/config'

/**
 * Switches the IDE onto the server backend in a development build.
 *
 * This is scaffolding, and deliberately dev-only. What will move a user off
 * local storage is logging in (#357); until BetterAuth lands there is nothing
 * to log in to, and the stub server keeps everyone's files in one unauthorised
 * namespace. Making a *production* build switch on the mere presence of
 * `/config.json` would therefore turn one `set-server-url` into "every student
 * now shares one pile of files", so it doesn't.
 *
 * `SCAMPER_DEV_SERVER` is a build-time constant (vite.config.ts), true only
 * under `vite --mode server`, so the branch below -- and the config fetch it
 * guards -- is eliminated from every other bundle entirely.
 *
 * Replace this with the login flow once there is one.
 *
 * @returns true iff the server backend was installed
 */
export async function useServerBackendInDev(): Promise<boolean> {
  if (!SCAMPER_DEV_SERVER) {
    return false
  }
  const config = await loadServerConfig()
  if (config === null) {
    // No `/config.json`, so this checkout is running the front end alone --
    // `npm run dev` rather than `npm run dev:full`. Stay on OPFS.
    return false
  }
  FS.setBackend(FS.serverBackend(config.serverUrl))
  console.info(`Scamper: using the file server at ${config.serverUrl}`)
  return true
}

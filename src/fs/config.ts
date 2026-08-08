/**
 * Where the deployment says its file server lives, if it has one.
 *
 * This is read at runtime rather than compiled in, and the reason is
 * `scripts/deploy`: it rsyncs each release into its own directory
 * (`scamper.cs.grinnell.edu/3.5.0/`) and `scripts/update-latest` only moves a
 * redirect, so every past release stays reachable at its URL indefinitely. A
 * build-time define -- the way `APP_VERSION` works -- would pin each of those
 * releases to whatever server URL was true on the day it shipped, and none of
 * them will ever be rebuilt.
 */

/** What a deployment publishes about its file server. */
export interface ServerConfig {
  /** Origin + path the API is rooted at, e.g. `https://host/api/v1`. */
  serverUrl: string
}

/**
 * The site-root path the config is fetched from.
 *
 * Deliberately absolute, and deliberately not `public/config.json`: a file in
 * `public/` is copied into each versioned build, so re-pointing the server
 * would mean editing one copy per release. One file at the site root re-points
 * every deployed version at once (see `scripts/set-server-url`).
 *
 * The tradeoff is that a Scamper served from a subdirectory of some other site
 * would look for the config above itself. Nothing deploys that way today.
 */
export const CONFIG_PATH = '/config.json'

/** @returns true iff `value` has the shape of a `ServerConfig`. */
function isServerConfig(value: unknown): value is ServerConfig {
  return (
    typeof value === 'object' &&
    value !== null &&
    'serverUrl' in value &&
    typeof (value as { serverUrl: unknown }).serverUrl === 'string' &&
    (value as { serverUrl: string }).serverUrl.length > 0
  )
}

/**
 * Reads the deployment's server configuration.
 *
 * Every failure -- no such file, a network error, malformed JSON, a web server
 * that answers 404s with an HTML error page -- means the same thing: this
 * deployment has no file server, so the caller should stay on OPFS. That is
 * the common case (a `npm run dev` checkout has no config at all) and it must
 * stay silent rather than surface as an error to a logged-out student.
 *
 * @returns the configuration, or null if this deployment has no file server
 */
export async function loadServerConfig(): Promise<ServerConfig | null> {
  try {
    const response = await fetch(CONFIG_PATH, { cache: 'no-store' })
    if (!response.ok) return null

    const parsed: unknown = await response.json()
    return isServerConfig(parsed) ? parsed : null
  } catch {
    return null
  }
}

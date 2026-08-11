// The server's configuration, read from the environment in one place so a
// missing value fails at startup with a message naming it, rather than at the
// first request that needed it.

/** @returns `name`'s value, or throws explaining what to set. */
function required(name: string, why: string): string {
  const value = process.env[name]
  if (value === undefined || value === '') {
    throw new Error(`${name} is not set. ${why}`)
  }
  return value
}

/** Where the database lives, e.g. `mysql://scamper:pw@localhost:3306/scamper`. */
export function databaseUrl(): string {
  return required(
    'DATABASE_URL',
    'It names the MariaDB the server stores files in.',
  )
}

/**
 * The key sessions are signed with.
 *
 * No default: a fallback would be a published secret, and anyone holding it
 * could mint a session for any user. Generate one with `openssl rand -base64 32`.
 */
export function authSecret(): string {
  return required(
    'BETTER_AUTH_SECRET',
    'Generate one with `openssl rand -base64 32`.',
  )
}

/**
 * The origin the app is served from, which is also this server's own origin --
 * the two share a host (see server/README.md), so this is the one URL both
 * halves answer on.
 */
export function authBaseUrl(): string {
  return required(
    'BETTER_AUTH_URL',
    'It is the origin Scamper is served from, e.g. http://localhost:5173.',
  )
}

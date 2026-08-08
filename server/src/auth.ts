import { betterAuth } from 'better-auth'
import { MysqlDialect } from 'kysely'
import type { Pool } from 'mysql2'

import { microsoftCredentials } from './env'

/**
 * Authentication, and the `user` table everything else is keyed to.
 *
 * Two ways in, deliberately:
 *
 * - **Microsoft (Entra ID)**, which is how students actually sign in: the
 *   institution runs Office 365, so they already have an account and Scamper
 *   never holds a password. Enabled only when its credentials are configured,
 *   so a checkout without them still runs.
 * - **Email and password**, which needs no third-party registration and so
 *   works offline. That keeps development, tests, and anyone without a campus
 *   account from depending on Entra being reachable.
 *
 * Nothing downstream knows which was used -- it all keys off `session.user.id`.
 *
 * The client and this server share an origin, so the session cookie needs no
 * cross-site handling: `SameSite=Lax` and same-origin are the defaults, and
 * they are correct here.
 */
export function createAuth(pool: Pool, secret: string, baseURL: string) {
  const microsoft = microsoftCredentials()

  return betterAuth({
    // Spelled out as a dialect rather than handed the pool directly. Passing a
    // bare mysql2 pool is accepted but broken in better-auth 1.6: its adapter
    // builds `new MysqlDialect(pool)` where Kysely wants
    // `new MysqlDialect({ pool })`, so the driver ends up with no pool at all
    // (the Postgres branch beside it does pass `{ pool: db }`). Naming the
    // type here also skips its `instanceof` dialect detection, which is
    // unreliable when a tool brings its own copy of Kysely -- as the migration
    // CLI does.
    database: { dialect: new MysqlDialect({ pool }), type: 'mysql' },
    secret,
    baseURL,
    emailAndPassword: {
      enabled: true,
      // Nothing sends mail yet, so requiring verification would lock out every
      // account at creation. Turn this on with the mail transport -- and see
      // the sign-up gate noted in server/README.md before deploying.
      requireEmailVerification: false,
    },
    socialProviders:
      microsoft === null
        ? {}
        : {
            microsoft: {
              clientId: microsoft.clientId,
              clientSecret: microsoft.clientSecret,
              // A single tenant means only that directory's accounts can sign
              // in, which is the sign-up gate for this route: nobody outside
              // the institution can get an account this way. `common` would
              // accept any Microsoft account anywhere, so it is not a default
              // worth having.
              tenantId: microsoft.tenantId,
            },
          },
  })
}

/** @returns true iff sign-in through Microsoft is configured. */
export function hasMicrosoft(): boolean {
  return microsoftCredentials() !== null
}

export type Auth = ReturnType<typeof createAuth>

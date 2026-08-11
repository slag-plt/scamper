import { betterAuth } from 'better-auth'
import { MysqlDialect } from 'kysely'
import type { Pool } from 'mysql2'

/**
 * Authentication, and the `user` table everything else is keyed to.
 *
 * Email and password, and nothing else. Two constraints shape this:
 *
 * - **No identity provider.** Institutional SSO is out on compliance grounds,
 *   so Scamper holds the credential itself.
 * - **No mail server.** Every flow that would normally send mail is therefore
 *   unavailable, which rules out self-service sign-up, address verification,
 *   and reset links.
 *
 * So sign-up is off (`disableSignUp`) and there is no reset route: an
 * administrator creates each account and passes the password on out of band,
 * and a forgotten password is another visit to the administrator. See
 * `accounts.ts` and `admin.ts`. This is not a stopgap -- a reset link is only
 * as trustworthy as the mailbox it lands in, and there is no mailbox here.
 *
 * The client and this server share an origin, so the session cookie needs no
 * cross-site handling: `SameSite=Lax` and same-origin are the defaults, and
 * they are correct here.
 */
export function createAuth(pool: Pool, secret: string, baseURL: string) {
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
      // Nobody may create their own account. This is the sign-up gate: with it
      // on, the only way in is an account an administrator made.
      disableSignUp: true,
      // Would need mail to satisfy, and an administrator vouched for the
      // address when creating the account -- `accounts.ts` marks it verified.
      requireEmailVerification: false,
    },
  })
}

export type Auth = ReturnType<typeof createAuth>

import { betterAuth } from 'better-auth'
import { MysqlDialect } from 'kysely'
import type { Pool } from 'mysql2'

/**
 * Authentication, and the `user` table everything else is keyed to.
 *
 * Email and password is what is enabled here, because it is the method that
 * needs no third-party registration to run: a contributor can create an account
 * against a local database and exercise the whole flow offline. Adding an
 * identity provider later (campus Google, say) is configuration on this object
 * plus a button in the login form -- nothing downstream cares, because
 * everything downstream keys off `session.user.id`.
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
      // Nothing sends mail yet, so requiring verification would lock out every
      // account at creation. Turn this on with the mail transport.
      requireEmailVerification: false,
    },
  })
}

export type Auth = ReturnType<typeof createAuth>

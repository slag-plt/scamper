import { readFile } from 'node:fs/promises'
import { fileURLToPath } from 'node:url'
import { createConnection, createPool, type Pool } from 'mysql2'
import type { Pool as SqlPool } from 'mysql2/promise'

/**
 * The database, in the two shapes its users want.
 *
 * BetterAuth takes the callback-style pool (it hands it to Kysely, which
 * detects MySQL by the presence of `getConnection`); everything we write uses
 * the promise wrapper over that same pool. One pool, so connection limits mean
 * what they say.
 */
export interface Database {
  /** For BetterAuth. */
  pool: Pool
  /** For our own queries. */
  sql: SqlPool
}

/** Opens the connection pool named by a `mysql://user:pass@host/db` URL. */
export function connect(url: string): Database {
  const pool = createPool(url)
  return { pool, sql: pool.promise() }
}

/**
 * Creates the two tables in `schema.sql` if they are absent.
 *
 * Run on every start rather than as a separate deploy step: the statements are
 * `IF NOT EXISTS`, so this is a no-op once applied, and a server that cannot
 * serve a request until its tables exist may as well make them.
 *
 * BetterAuth's tables (`user`, `session`, `account`, `verification`) are *not*
 * created here -- it owns them and its CLI migrates them, and ours reference
 * `user`, so this fails with a clear message if that has not been run.
 *
 * Uses its own short-lived connection because applying a whole file needs
 * `multipleStatements`, which the pool that serves requests deliberately does
 * not enable.
 */
export async function applySchema(url: string): Promise<void> {
  const path = fileURLToPath(new URL('../schema.sql', import.meta.url))
  const schema = await readFile(path, 'utf-8')

  const connection = createConnection({ uri: url, multipleStatements: true })
  try {
    await connection.promise().query(schema)
  } catch (error) {
    throw new Error(
      `Could not apply ${path}: ${error instanceof Error ? error.message : String(error)}\n` +
        "If it cannot find the `user` table, BetterAuth's tables have not been " +
        'created yet -- run `npm run db:migrate --workspace @scamper/server`.',
      { cause: error },
    )
  } finally {
    connection.destroy()
  }
}

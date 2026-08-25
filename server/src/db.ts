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

/**
 * Opens the connection pool named by a `mysql://user:pass@host/db` URL.
 *
 * `timezone: 'Z'` is not optional. DATETIME carries no zone, and every time
 * written here is UTC; without this mysql2 reads them back as *local* times, so
 * on a server whose clock is not UTC every snapshot comes back shifted by the
 * offset. That is worse than a display bug: a head timestamp in the future
 * makes `addsNothing` see a negative age, judge every save to be inside the
 * merge window, and record nothing at all.
 */
export function connect(url: string): Database {
  const pool = createPool({ uri: url, timezone: 'Z' })
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
    await widenFileContents(connection.promise())
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

/**
 * Widens `files.contents` from LONGTEXT to LONGBLOB, once (#385).
 *
 * `schema.sql` is `CREATE TABLE IF NOT EXISTS` throughout, which creates a
 * schema but cannot migrate one, and MariaDB has no guarded form of `MODIFY`.
 * Running a bare `ALTER` on every start would rebuild the table every time, so
 * the column is inspected first and the statement runs only while there is
 * something to change. Converting utf8mb4 text to bytes is lossless, and the
 * table is one row per file, so the one-time rewrite is quick.
 */
async function widenFileContents(sql: {
  query: (text: string) => Promise<unknown>
}): Promise<void> {
  const [rows] = (await sql.query(
    `SELECT DATA_TYPE FROM information_schema.COLUMNS
      WHERE TABLE_SCHEMA = DATABASE()
        AND TABLE_NAME = 'files' AND COLUMN_NAME = 'contents'`,
  )) as [{ DATA_TYPE: string }[], unknown]

  if (rows[0]?.DATA_TYPE.toLowerCase() !== 'longtext') return

  console.log('Migrating files.contents from LONGTEXT to LONGBLOB (#385)...')
  await sql.query('ALTER TABLE files MODIFY contents LONGBLOB NOT NULL')
}

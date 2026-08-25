// The MariaDB stores, against a real MariaDB.
//
// Skipped unless SCAMPER_TEST_DATABASE_URL is set, so an ordinary `npm test`
// needs no database. CI sets it (see .github/workflows/node.js.yml); locally:
//
//   docker run -d --name scamper-test-db -e MARIADB_ROOT_PASSWORD=test \
//     -e MARIADB_DATABASE=scamper_test -p 3307:3306 mariadb:11
//   SCAMPER_TEST_DATABASE_URL='mysql://root:test@127.0.0.1:3307/scamper_test' \
//     npx vitest run test/server/mariadb-stores.test.ts
//
// What these are here for is the scoping. Every store method takes a user id,
// and a query that forgets it returns somebody else's work -- which the
// in-memory stores cannot catch, because they are a different implementation of
// the same interface rather than the SQL that actually ships.

import { afterAll, afterEach, beforeAll, describe, expect, test } from 'vitest'
import type { Pool as SqlPool } from 'mysql2/promise'

import { connect, widenFileContents } from '../../server/src/db'
import {
  MariaDbFileStore,
  MariaDbHistoryStore,
} from '../../server/src/mariadb-stores'

const URL = process.env.SCAMPER_TEST_DATABASE_URL

/**
 * A file's contents are bytes (#385). These keep the specs below about the SQL
 * -- scoping, collation, transactions -- rather than about encoding.
 */
const bytes = (contents: string) => new TextEncoder().encode(contents)
const decode = (stored: Uint8Array | undefined) =>
  stored === undefined ? undefined : new TextDecoder().decode(stored)

/** Two users, so every test can check one cannot see the other's work. */
const ADA = 'user-ada'
const GRACE = 'user-grace'

let sql: SqlPool
let files: MariaDbFileStore
let history: MariaDbHistoryStore

describe.skipIf(URL === undefined)('the MariaDB stores', () => {
  beforeAll(async () => {
    // Through connect(), not a bare pool: its `timezone: 'Z'` is part of what
    // makes stored times mean what they say, so a test that configured its own
    // pool would pass while the server shipped a five-hour skew. The CI job
    // runs these under a non-UTC TZ for the same reason.
    sql = connect(URL ?? '').sql

    // The real schema minus its foreign keys to `user`: BetterAuth owns that
    // table and its CLI makes it, which is more than these tests need. What
    // they exercise is our queries, and those key on user_id either way.
    await sql.query(`CREATE TABLE IF NOT EXISTS files (
      id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      user_id VARCHAR(36) NOT NULL,
      name VARCHAR(255) COLLATE utf8mb4_bin NOT NULL,
      contents LONGBLOB NOT NULL, updated_at DATETIME(3) NOT NULL,
      UNIQUE KEY uniq_user_name (user_id, name))`)
    await sql.query(`CREATE TABLE IF NOT EXISTS histories (
      id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      user_id VARCHAR(36) NOT NULL,
      filename VARCHAR(255) COLLATE utf8mb4_bin NOT NULL,
      deleted_at DATETIME(3) NULL,
      UNIQUE KEY uniq_user_file (user_id, filename))`)
    await sql.query(`CREATE TABLE IF NOT EXISTS snapshots (
      id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      history_id BIGINT UNSIGNED NOT NULL, taken_at DATETIME(3) NOT NULL,
      contents LONGTEXT NOT NULL,
      CONSTRAINT fk_snap_hist FOREIGN KEY (history_id) REFERENCES histories (id)
        ON DELETE CASCADE,
      KEY idx_hist_time (history_id, taken_at DESC, id DESC))`)

    files = new MariaDbFileStore(sql)
    history = new MariaDbHistoryStore(sql)
  })

  afterEach(async () => {
    await sql.query('DELETE FROM snapshots')
    await sql.query('DELETE FROM histories')
    await sql.query('DELETE FROM files')
  })

  afterAll(async () => {
    await sql.end()
  })

  describe('files', () => {
    test('reads back what it wrote', async () => {
      await files.write(ADA, 'hello.scm', bytes('(+ 1 2)'))
      expect(decode(await files.read(ADA, 'hello.scm'))).toBe('(+ 1 2)')
    })

    test('replaces a file in place rather than adding a second row', async () => {
      await files.write(ADA, 'hello.scm', bytes('first'))
      await files.write(ADA, 'hello.scm', bytes('second'))
      expect(decode(await files.read(ADA, 'hello.scm'))).toBe('second')
      expect(await files.list(ADA)).toHaveLength(1)
    })

    test('keeps one user out of another\'s files', async () => {
      await files.write(ADA, 'hello.scm', bytes('ada'))
      expect(await files.list(GRACE)).toEqual([])
      expect(await files.read(GRACE, 'hello.scm')).toBeUndefined()
    })

    test('will not delete across users', async () => {
      await files.write(ADA, 'hello.scm', bytes('ada'))
      expect(await files.remove(GRACE, 'hello.scm')).toBe(false)
      expect(decode(await files.read(ADA, 'hello.scm'))).toBe('ada')
    })

    test('will not rename across users', async () => {
      await files.write(ADA, 'hello.scm', bytes('ada'))
      expect(await files.rename(GRACE, 'hello.scm', 'stolen.scm')).toBe(false)
      expect(decode(await files.read(ADA, 'hello.scm'))).toBe('ada')
    })

    test('renaming onto an existing name replaces it, leaving one file', async () => {
      await files.write(ADA, 'from.scm', bytes('moving'))
      await files.write(ADA, 'to.scm', bytes('replaced'))
      expect(await files.rename(ADA, 'from.scm', 'to.scm')).toBe(true)

      const names = (await files.list(ADA)).map((entry) => entry.name)
      expect(names).toEqual(['to.scm'])
      expect(decode(await files.read(ADA, 'to.scm'))).toBe('moving')
    })

    test('renaming a file that is gone leaves the destination alone', async () => {
      // The rename is delete-then-move in a transaction. If the move finds
      // nothing, the delete must not stand: a stale name cache on one machine
      // would otherwise destroy a file another machine had just written.
      await files.write(ADA, 'homework.scm', bytes('weeks of work'))

      expect(await files.rename(ADA, 'ghost.scm', 'homework.scm')).toBe(false)
      expect(decode(await files.read(ADA, 'homework.scm'))).toBe('weeks of work')
    })

    test('a refused cross-user rename leaves the destination alone', async () => {
      await files.write(ADA, 'mine.scm', bytes('ada'))
      await files.write(GRACE, 'target.scm', bytes('grace'))

      expect(await files.rename(GRACE, 'mine.scm', 'target.scm')).toBe(false)
      expect(decode(await files.read(GRACE, 'target.scm'))).toBe('grace')
      expect(decode(await files.read(ADA, 'mine.scm'))).toBe('ada')
    })

    test('names differing only by case are different files', async () => {
      // MariaDB's default collation is case-insensitive, which would fold these
      // into one row -- keeping one file's contents under the other's name.
      // Neither OPFS nor the Node backend behaves that way.
      await files.write(ADA, 'Homework.scm', bytes('capital'))
      await files.write(ADA, 'homework.scm', bytes('lowercase'))

      expect(decode(await files.read(ADA, 'Homework.scm'))).toBe('capital')
      expect(decode(await files.read(ADA, 'homework.scm'))).toBe('lowercase')
      expect(await files.list(ADA)).toHaveLength(2)
    })

    test('names differing only by accent are different files', async () => {
      await files.write(ADA, 'cafe.scm', bytes('plain'))
      await files.write(ADA, 'café.scm', bytes('accented'))

      expect(decode(await files.read(ADA, 'cafe.scm'))).toBe('plain')
      expect(decode(await files.read(ADA, 'café.scm'))).toBe('accented')
    })

    test('lists by name, and previews nothing hidden', async () => {
      await files.write(ADA, 'b.scm', bytes('second'))
      await files.write(ADA, 'a.scm', bytes('first'))
      await files.write(ADA, '.secret', bytes('internal'))

      const listed = await files.list(ADA)
      expect(listed.map((entry) => entry.name)).toEqual([
        '.secret',
        'a.scm',
        'b.scm',
      ])
      expect(listed.find((entry) => entry.name === '.secret')?.preview).toBeNull()
      expect(listed.find((entry) => entry.name === 'a.scm')?.preview).toBe('first')
    })

    test('bytes that are not text survive the column unchanged', async () => {
      // The whole point of LONGBLOB: a PNG signature is not valid UTF-8, and a
      // text column would have rejected or mangled it (#385). The in-memory
      // store cannot catch this -- it is a Map, not a column.
      const png = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0xff])
      await files.write(ADA, 'cat.png', png)

      expect(await files.read(ADA, 'cat.png')).toEqual(png)
      // And it is listed without a preview rather than read to build one.
      expect((await files.list(ADA))[0].preview).toBeNull()
    })

    test('a long file is previewed without reading all of it', async () => {
      // list() reads only the opening bytes, since it runs on every file
      // switch. The preview is still the first five lines of them.
      await files.write(ADA, 'long.scm', bytes('one\ntwo\nthree\nfour\nfive\nsix\n' + 'x'.repeat(50_000)))

      expect((await files.list(ADA))[0].preview).toBe('one\ntwo\nthree\nfour\nfive')
    })
  })

  // The guarded migration in db.ts. schema.sql is CREATE ... IF NOT EXISTS
  // throughout, so it creates a schema but cannot migrate one, and MariaDB has
  // no guarded MODIFY -- a bare ALTER on every start would rebuild the table
  // every time (#385).
  describe('widenFileContents', () => {
    /** @returns the declared type of `files.contents`, lower-cased. */
    async function contentsType(): Promise<string> {
      const [rows] = await sql.query(
        `SELECT DATA_TYPE FROM information_schema.COLUMNS
          WHERE TABLE_SCHEMA = DATABASE()
            AND TABLE_NAME = 'files' AND COLUMN_NAME = 'contents'`,
      )
      return ((rows as { DATA_TYPE: string }[])[0].DATA_TYPE).toLowerCase()
    }

    /** Puts the column back the way a database made before #385 had it. */
    async function makeLegacyTable(): Promise<void> {
      await sql.query('DROP TABLE IF EXISTS files')
      await sql.query(`CREATE TABLE files (
        id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        user_id VARCHAR(36) NOT NULL,
        name VARCHAR(255) COLLATE utf8mb4_bin NOT NULL,
        contents LONGTEXT NOT NULL, updated_at DATETIME(3) NOT NULL,
        UNIQUE KEY uniq_user_name (user_id, name))`)
    }

    test('widens a LONGTEXT column and keeps what was in it', async () => {
      await makeLegacyTable()
      await sql.query(
        `INSERT INTO files (user_id, name, contents, updated_at)
         VALUES (?, ?, ?, NOW(3))`,
        [ADA, 'homework.scm', '(display "λ")'],
      )
      expect(await contentsType()).toBe('longtext')

      await widenFileContents(sql)

      expect(await contentsType()).toBe('longblob')
      // utf8mb4 text to bytes is lossless, multi-byte characters included.
      expect(decode(await files.read(ADA, 'homework.scm'))).toBe('(display "λ")')
    })

    test('is a no-op once the column is already bytes', async () => {
      await makeLegacyTable()
      await widenFileContents(sql)
      await files.write(ADA, 'after.scm', bytes('kept'))

      // A second start must not rewrite the table again.
      await widenFileContents(sql)

      expect(await contentsType()).toBe('longblob')
      expect(decode(await files.read(ADA, 'after.scm'))).toBe('kept')
    })
  })

  describe('history', () => {
    const T1 = new Date('2026-08-01T10:00:00.000Z')
    const T2 = new Date('2026-08-01T11:00:00.000Z')

    test('records a snapshot and indexes it without contents', async () => {
      await history.record(ADA, 'hello.scm', 'first', T1, true)
      const { snapshots } = await history.index(ADA, 'hello.scm')

      expect(snapshots).toHaveLength(1)
      expect(snapshots[0].time).toBe(T1.toISOString())
      // The index carries times and ids only -- the reason snapshots are rows.
      expect(JSON.stringify(snapshots)).not.toContain('first')
    })

    test('gives back the time it was given, not the server\'s local reading', async () => {
      // DATETIME carries no zone. Without `timezone: 'Z'` on the pool this
      // comes back shifted by the server's offset -- and a head timestamp in
      // the future makes addsNothing suppress every later snapshot.
      await history.record(ADA, 'hello.scm', 'first', T1, true)
      const { snapshots } = await history.index(ADA, 'hello.scm')
      expect(snapshots[0].time).toBe('2026-08-01T10:00:00.000Z')
    })

    test('orders newest first', async () => {
      await history.record(ADA, 'hello.scm', 'first', T1, true)
      await history.record(ADA, 'hello.scm', 'second', T2, true)

      const { snapshots } = await history.index(ADA, 'hello.scm')
      expect(snapshots.map((s) => s.time)).toEqual([
        T2.toISOString(),
        T1.toISOString(),
      ])
    })

    test('skips a save that adds nothing', async () => {
      await history.record(ADA, 'hello.scm', 'same', T1, true)
      const again = await history.record(ADA, 'hello.scm', 'same', T2, true)

      expect(again.recorded).toBe(false)
      expect((await history.index(ADA, 'hello.scm')).snapshots).toHaveLength(1)
    })

    test('keeps one user out of another\'s history', async () => {
      await history.record(ADA, 'hello.scm', 'ada', T1, true)

      expect(await history.list(GRACE)).toEqual([])
      expect((await history.index(GRACE, 'hello.scm')).snapshots).toEqual([])
    })

    test('refuses a snapshot id guessed from another user\'s history', async () => {
      await history.record(ADA, 'hello.scm', 'ada', T1, true)
      const { snapshots } = await history.index(ADA, 'hello.scm')

      // Ids are sequential, so they are guessable by design; what stops a guess
      // is that reading one is scoped to the asking user's history.
      expect(await history.read(GRACE, 'hello.scm', snapshots[0].id)).toBeNull()
      expect(await history.read(ADA, 'hello.scm', snapshots[0].id)).toBe('ada')
    })

    test('renaming a file with no history keeps the destination\'s', async () => {
      // A file saved a minute ago has no snapshot yet (the merge window), so
      // renaming it onto a name carrying weeks of history must not take that
      // history with it -- which is the recovery path #42 exists for.
      await history.record(ADA, 'keeper.scm', 'weeks of versions', T1, true)

      expect(await history.rename(ADA, 'brand-new.scm', 'keeper.scm')).toBe(false)
      expect((await history.index(ADA, 'keeper.scm')).snapshots).toHaveLength(1)
    })

    test('marks a deletion and clears it on the next record', async () => {
      await history.record(ADA, 'hello.scm', 'ada', T1, true)
      await history.markDeleted(ADA, 'hello.scm', T2)
      expect((await history.list(ADA))[0].deletedAt).toBe(T2.toISOString())

      await history.record(ADA, 'hello.scm', 'back', T2, true)
      expect((await history.list(ADA))[0].deletedAt).toBeUndefined()
    })

    test('leaves no tombstone for a file that had no history', async () => {
      await history.markDeleted(ADA, 'never-saved.scm', T1)
      expect(await history.list(ADA)).toEqual([])
    })

    test('keeps no history of the IDE\'s own files', async () => {
      const outcome = await history.record(ADA, '.scamper.config', '{}', T1, true)
      expect(outcome.recorded).toBe(false)
      expect(await history.list(ADA)).toEqual([])
    })
  })
})

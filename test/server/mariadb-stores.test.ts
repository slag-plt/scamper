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
import { createPool, type Pool } from 'mysql2'
import type { Pool as SqlPool } from 'mysql2/promise'

import {
  MariaDbFileStore,
  MariaDbHistoryStore,
} from '../../server/src/mariadb-stores'

const URL = process.env.SCAMPER_TEST_DATABASE_URL

/** Two users, so every test can check one cannot see the other's work. */
const ADA = 'user-ada'
const GRACE = 'user-grace'

let pool: Pool
let sql: SqlPool
let files: MariaDbFileStore
let history: MariaDbHistoryStore

describe.skipIf(URL === undefined)('the MariaDB stores', () => {
  beforeAll(async () => {
    pool = createPool(URL ?? '')
    sql = pool.promise()

    // The real schema minus its foreign keys to `user`: BetterAuth owns that
    // table and its CLI makes it, which is more than these tests need. What
    // they exercise is our queries, and those key on user_id either way.
    await sql.query(`CREATE TABLE IF NOT EXISTS files (
      id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      user_id VARCHAR(36) NOT NULL, name VARCHAR(255) NOT NULL,
      contents LONGTEXT NOT NULL, updated_at DATETIME(3) NOT NULL,
      UNIQUE KEY uniq_user_name (user_id, name))`)
    await sql.query(`CREATE TABLE IF NOT EXISTS histories (
      id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      user_id VARCHAR(36) NOT NULL, filename VARCHAR(255) NOT NULL,
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
      await files.write(ADA, 'hello.scm', '(+ 1 2)')
      expect(await files.read(ADA, 'hello.scm')).toBe('(+ 1 2)')
    })

    test('replaces a file in place rather than adding a second row', async () => {
      await files.write(ADA, 'hello.scm', 'first')
      await files.write(ADA, 'hello.scm', 'second')
      expect(await files.read(ADA, 'hello.scm')).toBe('second')
      expect(await files.list(ADA)).toHaveLength(1)
    })

    test('keeps one user out of another\'s files', async () => {
      await files.write(ADA, 'hello.scm', 'ada')
      expect(await files.list(GRACE)).toEqual([])
      expect(await files.read(GRACE, 'hello.scm')).toBeUndefined()
    })

    test('will not delete across users', async () => {
      await files.write(ADA, 'hello.scm', 'ada')
      expect(await files.remove(GRACE, 'hello.scm')).toBe(false)
      expect(await files.read(ADA, 'hello.scm')).toBe('ada')
    })

    test('will not rename across users', async () => {
      await files.write(ADA, 'hello.scm', 'ada')
      expect(await files.rename(GRACE, 'hello.scm', 'stolen.scm')).toBe(false)
      expect(await files.read(ADA, 'hello.scm')).toBe('ada')
    })

    test('renaming onto an existing name replaces it, leaving one file', async () => {
      await files.write(ADA, 'from.scm', 'moving')
      await files.write(ADA, 'to.scm', 'replaced')
      expect(await files.rename(ADA, 'from.scm', 'to.scm')).toBe(true)

      const names = (await files.list(ADA)).map((entry) => entry.name)
      expect(names).toEqual(['to.scm'])
      expect(await files.read(ADA, 'to.scm')).toBe('moving')
    })

    test('lists by name, and previews nothing hidden', async () => {
      await files.write(ADA, 'b.scm', 'second')
      await files.write(ADA, 'a.scm', 'first')
      await files.write(ADA, '.secret', 'internal')

      const listed = await files.list(ADA)
      expect(listed.map((entry) => entry.name)).toEqual([
        '.secret',
        'a.scm',
        'b.scm',
      ])
      expect(listed.find((entry) => entry.name === '.secret')?.preview).toBeNull()
      expect(listed.find((entry) => entry.name === 'a.scm')?.preview).toBe('first')
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

// The durable stores, over the schema in server/schema.sql.
//
// The shape that matters is history: one row per snapshot, so listing which
// files have a history and indexing one file's versions never read a byte of
// file contents. Only `read` selects `contents`, and only for the single
// version being shown. That is the whole reason this replaced a flat file.

import type { RowDataPacket, ResultSetHeader } from 'mysql2'
import type { Pool as SqlPool } from 'mysql2/promise'

import { isHiddenName, type FileEntry } from '../../src/fs/fs'
import { addsNothing, MAX_SNAPSHOTS } from '../../src/history/policy'
import type {
  HistoryFile,
  HistoryIndex,
  Snapshot,
  SnapshotRef,
} from '../../src/history/history'
import { previewOf, type FileStore, type HistoryStore, type RecordOutcome } from './stores'

/**
 * MariaDB stores DATETIME without a zone. Every time written is UTC and every
 * time read is stamped back as UTC, so a server whose clock is local does not
 * silently shift a student's history.
 */
function toSql(time: Date): string {
  return time.toISOString().slice(0, 23).replace('T', ' ')
}

/** @returns an ISO-8601 UTC timestamp for a DATETIME(3) read back. */
function fromSql(value: Date | string): string {
  return typeof value === 'string'
    ? `${value.replace(' ', 'T')}Z`
    : value.toISOString()
}

export class MariaDbFileStore implements FileStore {
  constructor(private readonly sql: SqlPool) {}

  async list(userId: string): Promise<FileEntry[]> {
    const [rows] = await this.sql.query<RowDataPacket[]>(
      'SELECT name, contents FROM files WHERE user_id = ? ORDER BY name',
      [userId],
    )
    return rows.map((row) => ({
      name: row.name as string,
      preview: previewOf(row.name as string, row.contents as string),
      isDirectory: false,
    }))
  }

  async read(userId: string, name: string): Promise<string | undefined> {
    const [rows] = await this.sql.query<RowDataPacket[]>(
      'SELECT contents FROM files WHERE user_id = ? AND name = ?',
      [userId, name],
    )
    return rows.length === 0 ? undefined : (rows[0].contents as string)
  }

  async write(userId: string, name: string, contents: string): Promise<void> {
    // One statement, so a save cannot half-happen: the unique key on
    // (user_id, name) is what turns this into "create or replace".
    await this.sql.query(
      `INSERT INTO files (user_id, name, contents, updated_at) VALUES (?, ?, ?, ?)
       ON DUPLICATE KEY UPDATE contents = VALUES(contents), updated_at = VALUES(updated_at)`,
      [userId, name, contents, toSql(new Date())],
    )
  }

  async remove(userId: string, name: string): Promise<boolean> {
    const [result] = await this.sql.query<ResultSetHeader>(
      'DELETE FROM files WHERE user_id = ? AND name = ?',
      [userId, name],
    )
    return result.affectedRows > 0
  }

  async rename(userId: string, from: string, to: string): Promise<boolean> {
    const connection = await this.sql.getConnection()
    try {
      // Delete-then-rename in one transaction: overwriting `to` and moving
      // `from` onto it must not be separately observable, or an interruption
      // leaves the user with two copies or none.
      await connection.beginTransaction()
      await connection.query('DELETE FROM files WHERE user_id = ? AND name = ?', [
        userId,
        to,
      ])
      const [result] = await connection.query<ResultSetHeader>(
        'UPDATE files SET name = ? WHERE user_id = ? AND name = ?',
        [to, userId, from],
      )
      await connection.commit()
      return result.affectedRows > 0
    } catch (error) {
      await connection.rollback()
      throw error
    } finally {
      connection.release()
    }
  }
}

export class MariaDbHistoryStore implements HistoryStore {
  constructor(private readonly sql: SqlPool) {}

  async list(userId: string): Promise<HistoryFile[]> {
    // Joined against snapshots so a history emptied by retention does not show
    // up as a file with no versions.
    const [rows] = await this.sql.query<RowDataPacket[]>(
      `SELECT h.filename, h.deleted_at FROM histories h
        WHERE h.user_id = ?
          AND EXISTS (SELECT 1 FROM snapshots s WHERE s.history_id = h.id)
        ORDER BY h.filename`,
      [userId],
    )
    return rows.map((row) =>
      row.deleted_at === null
        ? { filename: row.filename as string }
        : {
            filename: row.filename as string,
            deletedAt: fromSql(row.deleted_at as Date | string),
          },
    )
  }

  async index(userId: string, filename: string): Promise<HistoryIndex> {
    const history = await this.find(userId, filename)
    if (history === null) return { snapshots: [] }

    const [rows] = await this.sql.query<RowDataPacket[]>(
      `SELECT id, taken_at FROM snapshots WHERE history_id = ?
        ORDER BY taken_at DESC, id DESC`,
      [history.id],
    )
    const snapshots: SnapshotRef[] = rows.map((row) => ({
      id: String(row.id),
      time: fromSql(row.taken_at as Date | string),
    }))

    return history.deletedAt === null
      ? { snapshots }
      : { snapshots, deletedAt: fromSql(history.deletedAt) }
  }

  async read(
    userId: string,
    filename: string,
    id: string,
  ): Promise<string | null> {
    // Scoped by history_id as well as id, so a guessed snapshot id from another
    // user's history reads as absent rather than as their file.
    const history = await this.find(userId, filename)
    if (history === null) return null

    const [rows] = await this.sql.query<RowDataPacket[]>(
      'SELECT contents FROM snapshots WHERE id = ? AND history_id = ?',
      [id, history.id],
    )
    return rows.length === 0 ? null : (rows[0].contents as string)
  }

  async record(
    userId: string,
    filename: string,
    contents: string,
    now: Date,
    force: boolean,
  ): Promise<RecordOutcome> {
    // No history of the IDE's own state, matching the flat-file backend.
    if (isHiddenName(filename)) return { recorded: false, head: null }

    const historyId = await this.findOrCreate(userId, filename)
    const head = await this.head(historyId)

    if (addsNothing(head, contents, now, force)) {
      // A skipped save still clears a deletion mark: the file is plainly back,
      // and nothing else would clear it.
      await this.sql.query(
        'UPDATE histories SET deleted_at = NULL WHERE id = ?',
        [historyId],
      )
      return { recorded: false, head }
    }

    await this.sql.query(
      'INSERT INTO snapshots (history_id, taken_at, contents) VALUES (?, ?, ?)',
      [historyId, toSql(now), contents],
    )
    await this.sql.query('UPDATE histories SET deleted_at = NULL WHERE id = ?', [
      historyId,
    ])
    // One insert plus a trim, rather than rewriting every version -- the reason
    // a row-per-snapshot store exists. MAX_SNAPSHOTS comes from the module the
    // client applies too, so the two cannot drift.
    await this.sql.query(
      `DELETE FROM snapshots WHERE history_id = ? AND id NOT IN (
         SELECT id FROM (
           SELECT id FROM snapshots WHERE history_id = ?
            ORDER BY taken_at DESC, id DESC LIMIT ?
         ) AS keep
       )`,
      [historyId, historyId, MAX_SNAPSHOTS],
    )

    return { recorded: true, head: await this.head(historyId) }
  }

  async rename(userId: string, from: string, to: string): Promise<boolean> {
    if (isHiddenName(from) || isHiddenName(to)) return false

    const connection = await this.sql.getConnection()
    try {
      await connection.beginTransaction()
      await connection.query(
        'DELETE FROM histories WHERE user_id = ? AND filename = ?',
        [userId, to],
      )
      const [result] = await connection.query<ResultSetHeader>(
        'UPDATE histories SET filename = ? WHERE user_id = ? AND filename = ?',
        [to, userId, from],
      )
      await connection.commit()
      return result.affectedRows > 0
    } catch (error) {
      await connection.rollback()
      throw error
    } finally {
      connection.release()
    }
  }

  async markDeleted(userId: string, filename: string, now: Date): Promise<void> {
    const history = await this.find(userId, filename)
    if (history === null) return

    // Only marks a history that has something in it: deleting a file that was
    // never recorded shouldn't leave a tombstone behind. Asked as its own
    // query rather than a correlated subquery inside the UPDATE, because
    // MariaDB cannot see the updated table's alias from inside a derived one.
    const [rows] = await this.sql.query<RowDataPacket[]>(
      'SELECT 1 FROM snapshots WHERE history_id = ? LIMIT 1',
      [history.id],
    )
    if (rows.length === 0) return

    await this.sql.query('UPDATE histories SET deleted_at = ? WHERE id = ?', [
      toSql(now),
      history.id,
    ])
  }

  /** @returns the history row for `filename`, or null if there is none yet. */
  private async find(
    userId: string,
    filename: string,
  ): Promise<{ id: number; deletedAt: Date | string | null } | null> {
    const [rows] = await this.sql.query<RowDataPacket[]>(
      'SELECT id, deleted_at FROM histories WHERE user_id = ? AND filename = ?',
      [userId, filename],
    )
    return rows.length === 0
      ? null
      : {
          id: rows[0].id as number,
          deletedAt: rows[0].deleted_at as Date | string | null,
        }
  }

  /** @returns the history row's id for `filename`, creating the row if absent. */
  private async findOrCreate(userId: string, filename: string): Promise<number> {
    const existing = await this.find(userId, filename)
    if (existing !== null) return existing.id

    const [result] = await this.sql.query<ResultSetHeader>(
      `INSERT INTO histories (user_id, filename, deleted_at) VALUES (?, ?, NULL)
       ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)`,
      [userId, filename],
    )
    return result.insertId
  }

  /** @returns the newest snapshot, with contents, or null if there are none. */
  private async head(historyId: number): Promise<Snapshot | null> {
    const [rows] = await this.sql.query<RowDataPacket[]>(
      `SELECT id, taken_at, contents FROM snapshots WHERE history_id = ?
        ORDER BY taken_at DESC, id DESC LIMIT 1`,
      [historyId],
    )
    if (rows.length === 0) return null
    return {
      id: String(rows[0].id),
      time: fromSql(rows[0].taken_at as Date | string),
      contents: rows[0].contents as string,
    }
  }
}

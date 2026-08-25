-- The MariaDB schema behind the server's storage.
--
-- Applied by server/src/db.ts on every start. Every statement is
-- `IF NOT EXISTS`, so applying it twice does nothing the second time.
--
-- Note that these are `IF NOT EXISTS`: they create a schema, they do not
-- migrate one. A change here needs an explicit ALTER against any database that
-- already exists.
--
-- BetterAuth owns the authentication tables (user, session, account,
-- verification) and its CLI creates them -- `npm run db:migrate --workspace
-- @scamper/server`. The tables below reference `user`, so that has to have run
-- first; db.ts says so if it hasn't.

-- A user's files. One row per file, replaced in place on save.
CREATE TABLE IF NOT EXISTS files (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  user_id    VARCHAR(36)  NOT NULL,
  -- Binary collation, because these are filenames. MariaDB's default is
  -- case- and accent-insensitive, which would make Homework.scm and
  -- homework.scm one row -- silently keeping one file's contents under the
  -- other's name -- and neither OPFS nor the Node backend behaves that way.
  name       VARCHAR(255) COLLATE utf8mb4_bin NOT NULL,
  -- Bytes, not text: a user's files include images and other things that are
  -- not UTF-8, and a text column would reject or mangle them (#385). Text
  -- files are simply their UTF-8 bytes. A database made before this change is
  -- widened by the guarded ALTER in db.ts, since a CREATE ... IF NOT EXISTS
  -- cannot migrate an existing column.
  contents   LONGBLOB     NOT NULL,
  updated_at DATETIME(3)  NOT NULL,

  -- Every file operation is scoped to one user and one name, which is exactly
  -- the FS interface: load, save, delete, rename, and exists all key on it.
  UNIQUE KEY uniq_user_name (user_id, name),
  CONSTRAINT fk_file_user FOREIGN KEY (user_id) REFERENCES user (id)
    ON DELETE CASCADE
);

-- A file's save history, kept separate from `files` on purpose: a history
-- outlives the file it belongs to, so a student can recover an accidental
-- delete (#42). `deleted_at` is that tombstone, not a row-deletion marker.
CREATE TABLE IF NOT EXISTS histories (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  user_id    VARCHAR(36)  NOT NULL,
  filename   VARCHAR(255) COLLATE utf8mb4_bin NOT NULL,
  deleted_at DATETIME(3)  NULL,

  UNIQUE KEY uniq_user_file (user_id, filename),
  CONSTRAINT fk_hist_user FOREIGN KEY (user_id) REFERENCES user (id)
    ON DELETE CASCADE
);

-- One row per recorded version, rather than one blob per file.
--
-- This is the point of the whole arrangement. As a flat file, listing which
-- files have a history meant reading every snapshot of every file, and adding
-- one meant rewriting the other forty-nine. As rows:
--
--   list    SELECT filename, deleted_at FROM histories WHERE user_id = ?
--   index   SELECT id, taken_at FROM snapshots WHERE history_id = ?
--             ORDER BY taken_at DESC, id DESC
--   read    SELECT contents FROM snapshots WHERE id = ? AND history_id = ?
--   record  INSERT, then trim past MAX_SNAPSHOTS
--
-- Only `read` touches `contents`, and only for the one version being shown.
CREATE TABLE IF NOT EXISTS snapshots (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  history_id BIGINT UNSIGNED NOT NULL,
  taken_at   DATETIME(3)     NOT NULL,
  -- Text, unlike `files.contents` above, and deliberately so: history is only
  -- ever recorded from the editor buffer, and a binary file never loads into
  -- the editor, so a blob cannot reach this table. Keeping it TEXT documents
  -- that, has MariaDB validate UTF-8 on insert, and leaves FULLTEXT indexing
  -- available if searching a history is ever wanted.
  contents   LONGTEXT        NOT NULL,

  CONSTRAINT fk_snap_hist FOREIGN KEY (history_id) REFERENCES histories (id)
    ON DELETE CASCADE,
  -- Newest-first is the only order anything asks for. `id` breaks ties, since
  -- two forced saves can land in the same millisecond.
  KEY idx_hist_time (history_id, taken_at DESC, id DESC)
);

-- Retention (src/history/policy.ts MAX_SNAPSHOTS) is enforced after each
-- insert rather than by a trigger, so the limit lives in one place that both
-- halves of the codebase already share:
--
--   DELETE FROM snapshots
--    WHERE history_id = ?
--      AND id NOT IN (
--        SELECT id FROM (
--          SELECT id FROM snapshots WHERE history_id = ?
--           ORDER BY taken_at DESC, id DESC LIMIT ?
--        ) AS keep
--      );

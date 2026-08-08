-- The MariaDB schema the in-memory stores in server/src/ stand in for.
--
-- Not applied by anything yet: BetterAuth is not wired up, so the `user` table
-- these reference does not exist. This is here so the shape is settled before
-- the stores are replaced, and so the route layer can be reviewed against the
-- queries it is meant to become.
--
-- BetterAuth owns the authentication tables (user, session, account,
-- verification) and creates them itself. Only the two below are ours.

-- A user's files. One row per file, replaced in place on save.
CREATE TABLE files (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  user_id    VARCHAR(36)  NOT NULL,
  name       VARCHAR(255) NOT NULL,
  contents   LONGTEXT     NOT NULL,
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
CREATE TABLE histories (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  user_id    VARCHAR(36)  NOT NULL,
  filename   VARCHAR(255) NOT NULL,
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
CREATE TABLE snapshots (
  id         BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  history_id BIGINT UNSIGNED NOT NULL,
  taken_at   DATETIME(3)     NOT NULL,
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

import type * as FS from '../../fs'
import type { History, Snapshot } from '../../history'

/**
 * The FileSession owns the IDE's file lifecycle: which file is currently open,
 * the autosave timer, and the save/delete/rename operations that read from the
 * editor and write to the file system.
 *
 * It is also where a file's save history is recorded (see src/history/), since
 * this is the one place the editor's contents reach storage.
 *
 * The key invariant it enforces is that a delete (or a delete-before-overwrite
 * during rename/upload) always waits for any in-flight save to settle before
 * removing the file. On OPFS, an in-flight save holds a writable lock on the
 * file, so removing it mid-save throws `NoModificationAllowedError` and the
 * delete silently fails. Serializing here fixes both file-system backends
 * uniformly (see issue #184).
 *
 * The session depends only on an injected `FS.t`, its paired `History`, and
 * small editor callbacks, so it is framework-free and unit-testable without
 * mounting a Vue component.
 */

/** Editor callbacks the session reads from when saving. */
export interface EditorHooks {
  /** @returns the current document text in the editor */
  getDoc: () => string
  /** @returns true iff the editor is loaded and safe to read from */
  isEditorLoaded: () => boolean
}

export interface FileSessionOptions {
  /** Autosave interval in milliseconds. */
  autosaveIntervalMs?: number
  /** Called when a save fails so the host can surface the error. */
  onSaveError?: (error: Error) => void
  /**
   * Whether a save can reach its file system at all. Defaults to always.
   *
   * This is how the offline state pauses autosave (issue #357): the timer keeps
   * running and every tick simply declines, so reconnecting resumes saving on
   * the next tick with no timer to restart. Left as a predicate rather than a
   * `pause()` call because the reason to decline is not the session's to know.
   */
  canSave?: () => boolean
}

const DEFAULT_AUTOSAVE_INTERVAL_MS = 3000

export class FileSession {
  private fs: FS.t
  private history: History
  private editor: EditorHooks
  private autosaveIntervalMs: number
  private onSaveError?: (error: Error) => void
  private canSave: () => boolean

  private currentFile: string | null = null
  private autosaveId: ReturnType<typeof setInterval> | null = null
  // The promise of the save currently writing to disk, if any. Delete/rename
  // await this so the writable is closed before the file is removed.
  private inFlightSave: Promise<void> | null = null
  // The newest snapshot of the open file, so a save that adds nothing to its
  // history costs no reads -- and, on a server-backed history, no request at
  // all. Dropped whenever the file it describes changes identity (switch,
  // delete, rename), as src/history/ requires.
  private historyHead: { filename: string; head: Snapshot | null } | null = null

  constructor(
    fs: FS.t,
    history: History,
    editor: EditorHooks,
    options: FileSessionOptions = {},
  ) {
    this.fs = fs
    this.history = history
    this.editor = editor
    this.autosaveIntervalMs =
      options.autosaveIntervalMs ?? DEFAULT_AUTOSAVE_INTERVAL_MS
    this.onSaveError = options.onSaveError
    this.canSave = options.canSave ?? (() => true)
  }

  /** @returns the name of the currently open file, or null if none. */
  getCurrentFile(): string | null {
    return this.currentFile
  }

  /** Sets the currently open file without touching the file system. */
  setCurrentFile(filename: string | null): void {
    this.currentFile = filename
  }

  // ---------- autosave ----------

  /** Starts the autosave timer if it isn't already running. */
  startAutosave(): void {
    this.autosaveId ??= setInterval(() => {
      void this.save()
    }, this.autosaveIntervalMs)
  }

  /**
   * Stops the autosave timer. Note this does NOT wait for an in-flight save;
   * callers that need the save to finish should `await settle()`.
   */
  stopAutosave(): void {
    if (this.autosaveId !== null) {
      clearInterval(this.autosaveId)
      this.autosaveId = null
    }
  }

  /** @returns true iff the autosave timer is currently running. */
  isAutosaving(): boolean {
    return this.autosaveId !== null
  }

  // ---------- saving ----------

  /**
   * Saves the current file, coalescing with any in-flight save so at most one
   * write is outstanding at a time. Resolves once the write has settled, or at
   * once if `canSave` says there is nowhere to write to.
   * @param options.force takes a history snapshot even inside the merge window
   *        (see src/history/). A save that coalesces with one already in
   *        flight cannot force, since that save is already past this point.
   */
  async save(options: { force?: boolean } = {}): Promise<void> {
    if (this.inFlightSave) {
      await this.inFlightSave
      return
    }
    const filename = this.currentFile
    if (filename === null || !this.editor.isEditorLoaded()) return
    // Nowhere to write to, so declining is the whole behaviour: an autosave
    // that tried anyway would produce one failed request every few seconds and
    // save nothing for its trouble.
    if (!this.canSave()) return

    const doc = this.editor.getDoc()
    this.inFlightSave = (async () => {
      try {
        await this.fs.saveFile(filename, doc)
      } catch (e) {
        if (e instanceof Error) this.onSaveError?.(e)
        // A save that failed is not history.
        return
      }
      // Inside the in-flight promise on purpose: delete and rename await
      // settle(), so this puts the history write inside the same critical
      // section as the file's own, rather than racing it.
      await this.recordHistory(filename, doc, options.force ?? false)
    })()
    try {
      await this.inFlightSave
    } finally {
      this.inFlightSave = null
    }
  }

  /**
   * Records the just-saved contents as a snapshot of `filename`. A history is
   * a convenience, so a failure here is swallowed: the file itself saved, and
   * taking over the screen with an error would cost more than a missing entry.
   */
  private async recordHistory(
    filename: string,
    contents: string,
    force: boolean,
  ): Promise<void> {
    try {
      const cached = this.historyHead
      const { head } = await this.history.record(filename, contents, new Date(), {
        force,
        ...(cached?.filename === filename ? { knownHead: cached.head } : {}),
      })
      this.historyHead = { filename, head }
    } catch {
      // Deliberately ignored; see above.
    }
  }

  /** Awaits any in-flight save so its writable is closed. */
  async settle(): Promise<void> {
    if (this.inFlightSave) await this.inFlightSave
  }

  // ---------- switching ----------

  /**
   * Switches the open file to `filename`, forcing a save of the outgoing file
   * BEFORE the new one is loaded so a quick edit is never lost on switch (issue
   * #238). The save is forced (not skipped like an incidental lifecycle save)
   * and coalesces with any in-flight save. The editor still holds the outgoing
   * file's doc at this point, so `save()` writes the correct contents; the
   * caller loads the returned source into the editor afterwards.
   * @returns the contents of the newly-opened file.
   */
  async switchTo(filename: string): Promise<string> {
    // Forced, so leaving a file always closes out its history with what is
    // actually on disk rather than leaving the last minute of edits unrecorded.
    if (this.currentFile !== null) await this.save({ force: true })
    const src = await this.fs.loadFile(filename)
    this.currentFile = filename
    this.historyHead = null
    return src
  }

  // ---------- deleting ----------

  /**
   * Deletes `filename`, first stopping autosave and awaiting any in-flight save
   * so its writable is closed (otherwise OPFS's `removeEntry` throws
   * `NoModificationAllowedError`). Nulls the current file before the removal so
   * a save that starts afterwards no-ops and cannot recreate the file. Errors
   * are surfaced (rethrown) rather than swallowed.
   */
  async deleteFile(
    filename: string,
    options: { replacing?: boolean } = {},
  ): Promise<void> {
    this.stopAutosave()
    await this.settle()
    const wasOpen = this.currentFile === filename
    if (wasOpen) this.currentFile = null
    this.historyHead = null

    // Pin what is about to disappear. A file deleted before its first autosave
    // would otherwise have no history at all, and recovering an accidental
    // delete is half of what the history is for (#42). The editor's contents
    // are used rather than the file's, since that is what the student sees.
    if (options.replacing !== true && wasOpen && this.editor.isEditorLoaded()) {
      try {
        await this.history.record(filename, this.editor.getDoc(), new Date(), {
          force: true,
        })
      } catch {
        // A history is a convenience; never fail the delete over one.
      }
    }

    await this.fs.deleteFile(filename)
    // The history outlives the file so it can be recovered (#42) -- but only
    // when the file is really going away. An overwrite deletes first purely to
    // close the writable, and the file is back a moment later.
    if (options.replacing !== true) {
      try {
        await this.history.markDeleted(filename, new Date())
      } catch {
        // A history is a convenience; never fail the delete over one.
      }
    }
  }

  // ---------- renaming ----------

  /**
   * Renames `from` to `to`, serializing against any in-flight save the same way
   * `deleteFile` does (renaming closes the fs handle to the source file).
   * Follows the open file to its new name, and leaves it alone when some other
   * file is the one being renamed.
   */
  async renameFile(from: string, to: string): Promise<void> {
    this.stopAutosave()
    await this.settle()
    const wasOpen = this.currentFile === from
    if (wasOpen) this.currentFile = null
    this.historyHead = null
    await this.fs.renameFile(from, to)
    if (wasOpen) this.currentFile = to
    try {
      await this.history.rename(from, to)
    } catch {
      // A history is a convenience; never fail the rename over one.
    }
  }
}

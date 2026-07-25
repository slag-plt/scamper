import type * as FS from '../../fs'

/**
 * The FileSession owns the IDE's file lifecycle: which file is currently open,
 * the autosave timer, and the save/delete/rename operations that read from the
 * editor and write to the file system.
 *
 * The key invariant it enforces is that a delete (or a delete-before-overwrite
 * during rename/upload) always waits for any in-flight save to settle before
 * removing the file. On OPFS, an in-flight save holds a writable lock on the
 * file, so removing it mid-save throws `NoModificationAllowedError` and the
 * delete silently fails. Serializing here fixes both file-system backends
 * uniformly (see issue #184).
 *
 * The session depends only on an injected `FS.t` and small editor callbacks, so
 * it is framework-free and unit-testable without mounting a Vue component.
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
  onSaveError?: (message: string) => void
}

const DEFAULT_AUTOSAVE_INTERVAL_MS = 3000

export class FileSession {
  private fs: FS.t
  private editor: EditorHooks
  private autosaveIntervalMs: number
  private onSaveError?: (message: string) => void

  private currentFile: string | null = null
  private autosaveId: ReturnType<typeof setInterval> | null = null
  // The promise of the save currently writing to disk, if any. Delete/rename
  // await this so the writable is closed before the file is removed.
  private inFlightSave: Promise<void> | null = null

  constructor(
    fs: FS.t,
    editor: EditorHooks,
    options: FileSessionOptions = {},
  ) {
    this.fs = fs
    this.editor = editor
    this.autosaveIntervalMs =
      options.autosaveIntervalMs ?? DEFAULT_AUTOSAVE_INTERVAL_MS
    this.onSaveError = options.onSaveError
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
   * write is outstanding at a time. Resolves once the write has settled.
   */
  async save(): Promise<void> {
    if (this.inFlightSave) {
      await this.inFlightSave
      return
    }
    const filename = this.currentFile
    if (filename === null || !this.editor.isEditorLoaded()) return

    const doc = this.editor.getDoc()
    this.inFlightSave = (async () => {
      try {
        await this.fs.saveFile(filename, doc)
      } catch (e) {
        if (e instanceof Error) this.onSaveError?.(e.message)
      }
    })()
    try {
      await this.inFlightSave
    } finally {
      this.inFlightSave = null
    }
  }

  /** Awaits any in-flight save so its writable is closed. */
  async settle(): Promise<void> {
    if (this.inFlightSave) await this.inFlightSave
  }

  // ---------- deleting ----------

  /**
   * Deletes `filename`, first stopping autosave and awaiting any in-flight save
   * so its writable is closed (otherwise OPFS's `removeEntry` throws
   * `NoModificationAllowedError`). Nulls the current file before the removal so
   * a save that starts afterwards no-ops and cannot recreate the file. Errors
   * are surfaced (rethrown) rather than swallowed.
   */
  async deleteFile(filename: string): Promise<void> {
    this.stopAutosave()
    await this.settle()
    if (this.currentFile === filename) this.currentFile = null
    await this.fs.deleteFile(filename)
  }

  // ---------- renaming ----------

  /**
   * Renames the current file to `newName`, serializing against any in-flight
   * save the same way `deleteFile` does (renaming closes the fs handle to the
   * source file). Updates the current file to the new name on success.
   */
  async renameFile(from: string, to: string): Promise<void> {
    this.stopAutosave()
    await this.settle()
    if (this.currentFile === from) this.currentFile = null
    await this.fs.renameFile(from, to)
    this.currentFile = to
  }
}

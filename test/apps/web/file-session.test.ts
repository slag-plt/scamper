import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import type { FileEntry, FS } from '../../../src/fs/fs'
import { FileSession } from '../../../src/app/web/file-session'

/** A deferred promise handle used to control when a save "settles". */
function defer(): { promise: Promise<void>; resolve: () => void; reject: (e: unknown) => void } {
  let resolve!: () => void
  let reject!: (e: unknown) => void
  const promise = new Promise<void>((res, rej) => {
    resolve = res
    reject = rej
  })
  return { promise, resolve, reject }
}

/**
 * An in-memory fake FS whose `saveFile` can be held pending to model an open
 * writable stream. When `lockOnSave` is true it behaves like OPFS: while a save
 * is in flight (writable open) the file is locked, and `deleteFile` throws
 * `NoModificationAllowedError`. When false it behaves like Node's lock-free
 * `fs.unlink`, so the module can be proven filesystem-agnostic.
 */
class FakeFS implements FS {
  files = new Map<string, string>()
  // The pending save currently holding the writable open, if any.
  private openSave: { filename: string; deferred: ReturnType<typeof defer> } | null = null
  saveCalls: string[] = []
  deleteCalls: string[] = []

  constructor(private lockOnSave: boolean) {}

  /** Resolves the currently-open save, closing its writable. */
  closeOpenSave(): void {
    this.openSave?.deferred.resolve()
  }

  hasOpenSave(): boolean {
    return this.openSave !== null
  }

  async getFileList(): Promise<FileEntry[]> {
    return [...this.files.keys()].map((name) => ({
      name,
      preview: null,
      isDirectory: false,
    }))
  }

  async fileExists(filename: string): Promise<boolean> {
    return this.files.has(filename)
  }

  async loadFile(filename: string): Promise<string> {
    const contents = this.files.get(filename)
    if (contents === undefined) throw new Error(`No such file: ${filename}`)
    return contents
  }

  async saveFile(filename: string, contents: string): Promise<void> {
    this.saveCalls.push(filename)
    const deferred = defer()
    this.openSave = { filename, deferred }
    // The write only commits once the "writable" is closed (deferred resolves).
    await deferred.promise
    this.files.set(filename, contents)
    this.openSave = null
  }

  async deleteFile(filename: string): Promise<void> {
    this.deleteCalls.push(filename)
    if (this.lockOnSave && this.openSave?.filename === filename) {
      const err = new Error('The file is locked.')
      err.name = 'NoModificationAllowedError'
      throw err
    }
    this.files.delete(filename)
  }

  async renameFile(from: string, to: string): Promise<void> {
    if (this.lockOnSave && this.openSave?.filename === from) {
      const err = new Error('The file is locked.')
      err.name = 'NoModificationAllowedError'
      throw err
    }
    const contents = this.files.get(from)
    if (contents === undefined) throw new Error(`No such file: ${from}`)
    this.files.set(to, contents)
    this.files.delete(from)
  }
}

const editor = {
  getDoc: () => 'some contents',
  isEditorLoaded: () => true,
}

describe.each([
  { label: 'OPFS-like (save holds a writable lock)', lockOnSave: true },
  { label: 'Node-like (lock-free)', lockOnSave: false },
])('FileSession delete race — $label', ({ lockOnSave }) => {
  let fs: FakeFS
  let s: FileSession

  beforeEach(() => {
    fs = new FakeFS(lockOnSave)
    s = new FileSession(fs, editor)
  })

  test('delete during an in-flight save waits for the save then removes the file', async () => {
    fs.files.set('a.scm', 'old')
    s.setCurrentFile('a.scm')

    // Kick off a save whose writable stays open (models autosave firing).
    const savePromise = s.save()
    // Let the save begin so the writable is registered as open.
    await Promise.resolve()
    expect(fs.hasOpenSave()).toBe(true)

    // Delete while the save is in flight. It must NOT reject/throw and must not
    // leave the file behind. Close the writable shortly after so the delete's
    // awaited settle() can proceed.
    const deletePromise = s.deleteFile('a.scm')
    fs.closeOpenSave()

    await expect(deletePromise).resolves.toBeUndefined()
    await savePromise

    // The file is actually gone (the bug was that it persisted).
    expect(fs.files.has('a.scm')).toBe(false)
    // Delete waited for the save, so it was never attempted on a locked file.
    expect(fs.deleteCalls).toEqual(['a.scm'])
    // The current file was cleared.
    expect(s.getCurrentFile()).toBeNull()
    // Autosave was stopped by the delete.
    expect(s.isAutosaving()).toBe(false)
  })

  test('normal delete with no in-flight save removes the file', async () => {
    fs.files.set('a.scm', 'contents')
    s.setCurrentFile('a.scm')

    await s.deleteFile('a.scm')

    expect(fs.files.has('a.scm')).toBe(false)
    expect(s.getCurrentFile()).toBeNull()
  })

  test('a save that starts after delete does not recreate the file', async () => {
    fs.files.set('a.scm', 'contents')
    s.setCurrentFile('a.scm')

    await s.deleteFile('a.scm')
    expect(s.getCurrentFile()).toBeNull()

    // Autosave fires again after the delete; with no current file it no-ops.
    await s.save()
    fs.closeOpenSave()

    expect(fs.files.has('a.scm')).toBe(false)
    expect(fs.saveCalls).toEqual([])
  })

  test('delete errors are surfaced, not swallowed', async () => {
    fs.files.set('a.scm', 'contents')
    s.setCurrentFile('a.scm')
    const boom = new Error('disk on fire')
    vi.spyOn(fs, 'deleteFile').mockRejectedValueOnce(boom)

    await expect(s.deleteFile('a.scm')).rejects.toBe(boom)
  })

  test('overwrite-delete of a non-current file serializes against a save of the current file', async () => {
    fs.files.set('current.scm', 'cur')
    fs.files.set('victim.scm', 'old')
    s.setCurrentFile('current.scm')

    const savePromise = s.save()
    await Promise.resolve()
    expect(fs.hasOpenSave()).toBe(true)

    // Deleting a different file (upload overwrite path) still awaits settle().
    const deletePromise = s.deleteFile('victim.scm')
    fs.closeOpenSave()
    await expect(deletePromise).resolves.toBeUndefined()
    await savePromise

    expect(fs.files.has('victim.scm')).toBe(false)
    // Deleting a non-current file leaves the current file intact.
    expect(s.getCurrentFile()).toBe('current.scm')
  })
})

describe('FileSession autosave lifecycle', () => {
  beforeEach(() => {
    vi.useFakeTimers()
  })
  afterEach(() => {
    vi.useRealTimers()
  })

  test('start/stop autosave toggles the timer and fires saves', async () => {
    const fs = new FakeFS(true)
    fs.files.set('a.scm', 'x')
    const s = new FileSession(fs, editor, { autosaveIntervalMs: 1000 })
    s.setCurrentFile('a.scm')

    expect(s.isAutosaving()).toBe(false)
    s.startAutosave()
    expect(s.isAutosaving()).toBe(true)
    // Starting twice does not stack timers.
    s.startAutosave()

    vi.advanceTimersByTime(1000)
    expect(fs.saveCalls).toEqual(['a.scm'])

    s.stopAutosave()
    expect(s.isAutosaving()).toBe(false)
    vi.advanceTimersByTime(5000)
    expect(fs.saveCalls).toEqual(['a.scm'])
  })
})

describe('FileSession switchTo (issue #238)', () => {
  test('forces a save of the outgoing file before loading the new one', async () => {
    const fs = new FakeFS(false)
    fs.files.set('a.scm', 'a on disk')
    fs.files.set('b.scm', 'b on disk')

    // The editor holds an unsaved edit to the OUTGOING file (a.scm).
    const mutableEditor = {
      getDoc: () => 'a edited',
      isEditorLoaded: () => true,
    }
    const s = new FileSession(fs, mutableEditor)
    s.setCurrentFile('a.scm')

    // switchTo awaits the forced outgoing save, whose writable stays open until
    // we close it; do so once the save is in flight so the switch can proceed.
    const switchPromise = s.switchTo('b.scm')
    await Promise.resolve()
    fs.closeOpenSave()
    const src = await switchPromise

    // The outgoing file's edit was persisted before the switch (the bug lost it).
    expect(fs.files.get('a.scm')).toBe('a edited')
    // The new file's contents are returned for the caller to load.
    expect(src).toBe('b on disk')
    // The session now tracks the new file.
    expect(s.getCurrentFile()).toBe('b.scm')
  })

  test('saves the outgoing file BEFORE reading the new one', async () => {
    const events: string[] = []
    const fs = new FakeFS(false)
    fs.files.set('a.scm', 'a on disk')
    fs.files.set('b.scm', 'b on disk')
    const saveSpy = vi.spyOn(fs, 'saveFile')
    saveSpy.mockImplementation(async (filename: string, contents: string) => {
      events.push(`save:${filename}`)
      fs.files.set(filename, contents)
    })
    const loadSpy = vi.spyOn(fs, 'loadFile')
    loadSpy.mockImplementation((filename: string) => {
      events.push(`load:${filename}`)
      return Promise.resolve(fs.files.get(filename) ?? '')
    })

    const s = new FileSession(fs, {
      getDoc: () => 'a edited',
      isEditorLoaded: () => true,
    })
    s.setCurrentFile('a.scm')

    await s.switchTo('b.scm')

    // The outgoing save must strictly precede the incoming load.
    expect(events).toEqual(['save:a.scm', 'load:b.scm'])
  })

  test('does not save when no file is currently open', async () => {
    const fs = new FakeFS(false)
    fs.files.set('b.scm', 'b on disk')
    const s = new FileSession(fs, editor)
    // No current file (e.g. after a delete): nothing to save on switch.

    const src = await s.switchTo('b.scm')

    expect(fs.saveCalls).toEqual([])
    expect(src).toBe('b on disk')
    expect(s.getCurrentFile()).toBe('b.scm')
  })
})

describe('FileSession rename', () => {
  test('rename during an in-flight save waits then renames and updates current file', async () => {
    const fs = new FakeFS(true)
    fs.files.set('a.scm', 'old')
    const s = new FileSession(fs, editor)
    s.setCurrentFile('a.scm')

    const savePromise = s.save()
    await Promise.resolve()
    expect(fs.hasOpenSave()).toBe(true)

    const renamePromise = s.renameFile('a.scm', 'b.scm')
    fs.closeOpenSave()
    await expect(renamePromise).resolves.toBeUndefined()
    await savePromise

    expect(fs.files.has('a.scm')).toBe(false)
    expect(fs.files.has('b.scm')).toBe(true)
    expect(s.getCurrentFile()).toBe('b.scm')
    expect(s.isAutosaving()).toBe(false)
  })
})

import { afterEach, describe, expect, test, vi } from 'vitest'
import * as Lock from '../../../src/app/web/lockfile'
import type { FS } from '../../../src/fs/fs'

/** An in-memory FS good enough for the lockfile's fileExists/load/save/delete. */
function mockFS(): FS {
  const files = new Map<string, string>()
  return {
    getFileList: () =>
      Promise.resolve(
        [...files.keys()].map((name) => ({ name, preview: null, isDirectory: false })),
      ),
    fileExists: (n) => Promise.resolve(files.has(n)),
    loadFile: (n) => Promise.resolve(files.get(n) ?? ''),
    saveFile: (n, c) => {
      files.set(n, c)
      return Promise.resolve()
    },
    deleteFile: (n) => {
      files.delete(n)
      return Promise.resolve()
    },
    renameFile: (a, b) => {
      const v = files.get(a)
      if (v !== undefined) {
        files.set(b, v)
        files.delete(a)
      }
      return Promise.resolve()
    },
  }
}

describe('lockfile', () => {
  afterEach(() => {
    vi.useRealTimers()
  })

  test('acquires when no lock exists', async () => {
    const fs = mockFS()
    expect(await Lock.acquireLockFile(fs)).toBe(true)
    expect(await fs.fileExists(Lock.lockfileName)).toBe(true)
    await Lock.releaseLockFile(fs)
  })

  test('blocks when a fresh lock is present', async () => {
    const fs = mockFS()
    await fs.saveFile(Lock.lockfileName, new Date().toISOString())
    expect(await Lock.acquireLockFile(fs)).toBe(false)
  })

  test('takes over a stale lock', async () => {
    const fs = mockFS()
    const stale = new Date(Date.now() - 5 * 60_000).toISOString() // 5 min old
    await fs.saveFile(Lock.lockfileName, stale)
    expect(await Lock.acquireLockFile(fs)).toBe(true)
    // The stale timestamp was overwritten with a fresh one.
    expect(await fs.loadFile(Lock.lockfileName)).not.toBe(stale)
    await Lock.releaseLockFile(fs)
  })

  test('treats an unreadable lock as absent and acquires', async () => {
    const fs = mockFS()
    await fs.saveFile(Lock.lockfileName, 'not a date')
    expect(await Lock.acquireLockFile(fs)).toBe(true)
    await Lock.releaseLockFile(fs)
  })

  test('releaseLockFile removes the lock', async () => {
    const fs = mockFS()
    await Lock.acquireLockFile(fs)
    await Lock.releaseLockFile(fs)
    expect(await fs.fileExists(Lock.lockfileName)).toBe(false)
  })

  test('heartbeat keeps the timestamp fresh while held', async () => {
    vi.useFakeTimers({ now: new Date('2020-01-01T00:00:00Z') })
    const fs = mockFS()
    await Lock.acquireLockFile(fs)
    const first = await fs.loadFile(Lock.lockfileName)
    await vi.advanceTimersByTimeAsync(30_000)
    expect(await fs.loadFile(Lock.lockfileName)).not.toBe(first)
    await Lock.releaseLockFile(fs)
  })
})

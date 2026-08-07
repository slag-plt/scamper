import { beforeEach, describe, expect, test, vi } from 'vitest'
import {
  historyFilename,
  loadHistory,
  markHistoryDeleted,
  MAX_SNAPSHOTS,
  MERGE_WINDOW_MS,
  recordSnapshot,
  renameHistory,
} from '../../../src/app/web/file-history'
import { MockFileSystem } from '../../stubs/mock-file-system'

// The save history of a file (issue #42). These cover the recording policy,
// which is where the feature is correct or not: autosave writes every few
// seconds whether or not anything changed, so what does *not* become a
// snapshot matters as much as what does.

const START = new Date('2026-08-07T14:00:00.000Z')

/** @returns `START` advanced by `ms`. */
function at(ms: number): Date {
  return new Date(START.getTime() + ms)
}

let fs: MockFileSystem

beforeEach(() => {
  fs = new MockFileSystem()
})

/** @returns the contents of every snapshot of `filename`, newest first. */
async function contentsOf(filename: string): Promise<string[]> {
  return (await loadHistory(fs, filename)).snapshots.map((s) => s.contents)
}

describe('recordSnapshot', () => {
  test('records the first save', async () => {
    const result = await recordSnapshot(fs, 'hello.scm', '(display 1)', START)

    expect(result.recorded).toBe(true)
    expect(result.head).toEqual({ time: START.toISOString(), contents: '(display 1)' })
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('keeps snapshots newest first', async () => {
    await recordSnapshot(fs, 'hello.scm', 'one', START)
    await recordSnapshot(fs, 'hello.scm', 'two', at(MERGE_WINDOW_MS))
    await recordSnapshot(fs, 'hello.scm', 'three', at(2 * MERGE_WINDOW_MS))

    expect(await contentsOf('hello.scm')).toEqual(['three', 'two', 'one'])
  })

  test('ignores a save that changed nothing', async () => {
    // Autosave rewrites the file every 3s regardless of edits, so without this
    // a history would fill with identical entries.
    await recordSnapshot(fs, 'hello.scm', '(display 1)', START)
    const result = await recordSnapshot(
      fs,
      'hello.scm',
      '(display 1)',
      at(10 * MERGE_WINDOW_MS),
    )

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('folds edits made inside the merge window into the open snapshot', async () => {
    await recordSnapshot(fs, 'hello.scm', 'first', START)
    const result = await recordSnapshot(fs, 'hello.scm', 'second', at(MERGE_WINDOW_MS - 1))

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['first'])
  })

  test('opens a new snapshot once the window has passed', async () => {
    await recordSnapshot(fs, 'hello.scm', 'first', START)
    await recordSnapshot(fs, 'hello.scm', 'second', at(MERGE_WINDOW_MS))

    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('measures the window from the snapshot, not from the last edit', async () => {
    // The trap: if each edit slid the window forward, a student typing
    // steadily would hold it open forever and the history would stay at one
    // entry. Five minutes of edits every 3s is ~5 entries, not 1 and not 100.
    for (let ms = 0; ms < 5 * MERGE_WINDOW_MS; ms += 3_000) {
      await recordSnapshot(fs, 'hello.scm', `edit at ${ms.toString()}`, at(ms))
    }

    expect((await contentsOf('hello.scm')).length).toBe(5)
  })

  test('forces a snapshot inside the window when asked', async () => {
    // What closing or switching away from a file does, so a session always
    // ends on a complete entry.
    await recordSnapshot(fs, 'hello.scm', 'first', START)
    const result = await recordSnapshot(fs, 'hello.scm', 'second', at(1_000), {
      force: true,
    })

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('forcing still ignores a save that changed nothing', async () => {
    await recordSnapshot(fs, 'hello.scm', 'same', START)
    const result = await recordSnapshot(fs, 'hello.scm', 'same', at(1_000), {
      force: true,
    })

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['same'])
  })

  test('drops the oldest snapshots past the cap', async () => {
    for (let i = 0; i <= MAX_SNAPSHOTS; i++) {
      await recordSnapshot(fs, 'hello.scm', `edit ${i.toString()}`, at(i * MERGE_WINDOW_MS))
    }

    const contents = await contentsOf('hello.scm')
    expect(contents.length).toBe(MAX_SNAPSHOTS)
    expect(contents[0]).toBe(`edit ${MAX_SNAPSHOTS.toString()}`)
    // 'edit 0' fell off the end; the oldest kept is the one after it.
    expect(contents[contents.length - 1]).toBe('edit 1')
  })

  test('keeps no history of internal files', async () => {
    const result = await recordSnapshot(fs, '.scamper.config', '{}', START)

    expect(result.recorded).toBe(false)
    expect(await fs.fileExists(historyFilename('.scamper.config'))).toBe(false)
  })

  test('touches no storage when a cached head says the save adds nothing', async () => {
    const { head } = await recordSnapshot(fs, 'hello.scm', 'same', START)
    const loadFile = vi.spyOn(fs, 'loadFile')
    const saveFile = vi.spyOn(fs, 'saveFile')

    const result = await recordSnapshot(fs, 'hello.scm', 'same', at(3_000), {
      knownHead: head,
    })

    expect(result.recorded).toBe(false)
    expect(loadFile).not.toHaveBeenCalled()
    expect(saveFile).not.toHaveBeenCalled()
  })

  test('still records when a cached head says the save is new', async () => {
    const { head } = await recordSnapshot(fs, 'hello.scm', 'first', START)
    const result = await recordSnapshot(fs, 'hello.scm', 'second', at(MERGE_WINDOW_MS), {
      knownHead: head,
    })

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('starts over on a history it cannot read', async () => {
    await fs.saveFile(historyFilename('hello.scm'), 'this is not json{')

    const result = await recordSnapshot(fs, 'hello.scm', '(display 1)', START)

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('starts over on a history from an unknown version', async () => {
    // A newer build wrote this. Refusing to record would disable snapshots for
    // this file forever, which is worse than losing entries the running build
    // cannot read anyway.
    await fs.saveFile(
      historyFilename('hello.scm'),
      JSON.stringify({ version: 99, entries: ['???'] }),
    )

    const result = await recordSnapshot(fs, 'hello.scm', '(display 1)', START)

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('skips entries of the wrong shape', async () => {
    await fs.saveFile(
      historyFilename('hello.scm'),
      JSON.stringify({
        version: 1,
        snapshots: [{ time: 'yesterday', contents: 'good' }, { nonsense: true }],
      }),
    )

    expect(await contentsOf('hello.scm')).toEqual(['good'])
  })
})

describe('renameHistory', () => {
  test('carries the history across to the new name', async () => {
    await recordSnapshot(fs, 'hello.scm', '(display 1)', START)
    await renameHistory(fs, 'hello.scm', 'goodbye.scm')

    expect(await contentsOf('goodbye.scm')).toEqual(['(display 1)'])
    expect(await fs.fileExists(historyFilename('hello.scm'))).toBe(false)
  })

  test('does nothing for a file with no history', async () => {
    await renameHistory(fs, 'hello.scm', 'goodbye.scm')

    expect(await fs.fileExists(historyFilename('goodbye.scm'))).toBe(false)
  })
})

describe('markHistoryDeleted', () => {
  test('keeps the history, marked, so the file can be recovered', async () => {
    await recordSnapshot(fs, 'hello.scm', '(display 1)', START)
    await markHistoryDeleted(fs, 'hello.scm', at(1_000))

    const history = await loadHistory(fs, 'hello.scm')
    expect(history.deletedAt).toBe(at(1_000).toISOString())
    expect(history.snapshots.map((s) => s.contents)).toEqual(['(display 1)'])
  })

  test('leaves no history behind for a file that had none', async () => {
    await markHistoryDeleted(fs, 'hello.scm', START)

    expect(await fs.fileExists(historyFilename('hello.scm'))).toBe(false)
  })

  test('a later save clears the mark, even when it adds no snapshot', async () => {
    // Recreating a deleted file with its original contents: the save itself
    // adds nothing, but the history is no longer of a deleted file.
    await recordSnapshot(fs, 'hello.scm', '(display 1)', START)
    await markHistoryDeleted(fs, 'hello.scm', at(1_000))

    const result = await recordSnapshot(fs, 'hello.scm', '(display 1)', at(2_000))

    expect(result.recorded).toBe(false)
    expect((await loadHistory(fs, 'hello.scm')).deletedAt).toBeUndefined()
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })
})

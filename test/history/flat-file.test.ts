import { beforeEach, describe, expect, test, vi } from 'vitest'
import {
  fileOfHistory,
  FlatFileHistory,
  historyFilename,
} from '../../src/history/flat-file'
import { formatSnapshotTime } from '../../src/history/history'
import { MAX_SNAPSHOTS, MERGE_WINDOW_MS } from '../../src/history/policy'
import { MockFileSystem } from '../stubs/mock-file-system'

// The flat-file save history (issue #42): one `.{filename}.history` blob per
// file, which is what OPFS and the CLI use. These cover the recording policy,
// which is where the feature is correct or not: autosave writes every few
// seconds whether or not anything changed, so what does *not* become a
// snapshot matters as much as what does.

const START = new Date('2026-08-07T14:00:00.000Z')

/** @returns `START` advanced by `ms`. */
function at(ms: number): Date {
  return new Date(START.getTime() + ms)
}

let fs: MockFileSystem
let history: FlatFileHistory

beforeEach(() => {
  fs = new MockFileSystem()
  history = new FlatFileHistory(fs)
})

/**
 * @returns the contents of every snapshot of `filename`, newest first. Goes
 *          through the interface's index-then-read pair rather than reaching
 *          into storage, so it exercises the ids the browser navigates by.
 */
async function contentsOf(filename: string): Promise<string[]> {
  const { snapshots } = await history.index(filename)
  const contents = await Promise.all(
    snapshots.map((s) => history.read(filename, s.id)),
  )
  return contents.map((c) => c ?? '<missing>')
}

describe('record', () => {
  test('records the first save', async () => {
    const result = await history.record('hello.scm', '(display 1)', START)

    expect(result.recorded).toBe(true)
    expect(result.head).toEqual({
      id: START.toISOString(),
      time: START.toISOString(),
      contents: '(display 1)',
    })
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('keeps snapshots newest first', async () => {
    await history.record('hello.scm', 'one', START)
    await history.record('hello.scm', 'two', at(MERGE_WINDOW_MS))
    await history.record('hello.scm', 'three', at(2 * MERGE_WINDOW_MS))

    expect(await contentsOf('hello.scm')).toEqual(['three', 'two', 'one'])
  })

  test('ignores a save that changed nothing', async () => {
    // Autosave rewrites the file every 3s regardless of edits, so without this
    // a history would fill with identical entries.
    await history.record('hello.scm', '(display 1)', START)
    const result = await history.record('hello.scm',
      '(display 1)',
      at(10 * MERGE_WINDOW_MS),
    )

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('folds edits made inside the merge window into the open snapshot', async () => {
    await history.record('hello.scm', 'first', START)
    const result = await history.record('hello.scm', 'second', at(MERGE_WINDOW_MS - 1))

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['first'])
  })

  test('opens a new snapshot once the window has passed', async () => {
    await history.record('hello.scm', 'first', START)
    await history.record('hello.scm', 'second', at(MERGE_WINDOW_MS))

    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('measures the window from the snapshot, not from the last edit', async () => {
    // The trap: if each edit slid the window forward, a student typing
    // steadily would hold it open forever and the history would stay at one
    // entry. Five minutes of edits every 3s is ~5 entries, not 1 and not 100.
    for (let ms = 0; ms < 5 * MERGE_WINDOW_MS; ms += 3_000) {
      await history.record('hello.scm', `edit at ${ms.toString()}`, at(ms))
    }

    expect((await contentsOf('hello.scm')).length).toBe(5)
  })

  test('forces a snapshot inside the window when asked', async () => {
    // What closing or switching away from a file does, so a session always
    // ends on a complete entry.
    await history.record('hello.scm', 'first', START)
    const result = await history.record('hello.scm', 'second', at(1_000), {
      force: true,
    })

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('forcing still ignores a save that changed nothing', async () => {
    await history.record('hello.scm', 'same', START)
    const result = await history.record('hello.scm', 'same', at(1_000), {
      force: true,
    })

    expect(result.recorded).toBe(false)
    expect(await contentsOf('hello.scm')).toEqual(['same'])
  })

  test('drops the oldest snapshots past the cap', async () => {
    for (let i = 0; i <= MAX_SNAPSHOTS; i++) {
      await history.record('hello.scm', `edit ${i.toString()}`, at(i * MERGE_WINDOW_MS))
    }

    const contents = await contentsOf('hello.scm')
    expect(contents.length).toBe(MAX_SNAPSHOTS)
    expect(contents[0]).toBe(`edit ${MAX_SNAPSHOTS.toString()}`)
    // 'edit 0' fell off the end; the oldest kept is the one after it.
    expect(contents[contents.length - 1]).toBe('edit 1')
  })

  test('keeps no history of internal files', async () => {
    const result = await history.record('.scamper.config', '{}', START)

    expect(result.recorded).toBe(false)
    expect(await fs.fileExists(historyFilename('.scamper.config'))).toBe(false)
  })

  test('touches no storage when a cached head says the save adds nothing', async () => {
    const { head } = await history.record('hello.scm', 'same', START)
    const loadFile = vi.spyOn(fs, 'loadFile')
    const saveFile = vi.spyOn(fs, 'saveFile')

    const result = await history.record('hello.scm', 'same', at(3_000), {
      knownHead: head,
    })

    expect(result.recorded).toBe(false)
    expect(loadFile).not.toHaveBeenCalled()
    expect(saveFile).not.toHaveBeenCalled()
  })

  test('still records when a cached head says the save is new', async () => {
    const { head } = await history.record('hello.scm', 'first', START)
    const result = await history.record('hello.scm', 'second', at(MERGE_WINDOW_MS), {
      knownHead: head,
    })

    expect(result.recorded).toBe(true)
    expect(await contentsOf('hello.scm')).toEqual(['second', 'first'])
  })

  test('starts over on a history it cannot read', async () => {
    await fs.saveFile(historyFilename('hello.scm'), 'this is not json{')

    const result = await history.record('hello.scm', '(display 1)', START)

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

    const result = await history.record('hello.scm', '(display 1)', START)

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

describe('formatSnapshotTime', () => {
  const now = new Date(2026, 7, 7, 15, 0)

  test('shows only the clock for today', () => {
    expect(formatSnapshotTime(new Date(2026, 7, 7, 14, 4).toISOString(), now)).toBe(
      '2:04pm',
    )
  })

  test('adds the date earlier in the same year', () => {
    expect(formatSnapshotTime(new Date(2026, 7, 1, 9, 5).toISOString(), now)).toBe(
      'Aug 1, 9:05am',
    )
  })

  test('adds the year for an older one', () => {
    // Deleted files keep their history indefinitely, so last year's version
    // must not read as this week's.
    expect(formatSnapshotTime(new Date(2025, 7, 7, 14, 4).toISOString(), now)).toBe(
      'Aug 7 2025, 2:04pm',
    )
  })

  test('says so for a time it cannot read', () => {
    expect(formatSnapshotTime('not a date', now)).toBe('unknown')
  })
})

describe('list', () => {
  test('lists files with a history, deleted ones included', async () => {
    await history.record('hello.scm', 'one', START)
    await history.record('gone.scm', 'two', START)
    await history.markDeleted('gone.scm', at(1_000))
    // A file with no history at all, and an unrelated internal file.
    await fs.saveFile('untouched.scm', 'three')
    await fs.saveFile('.scamper.config', '{}')

    expect(await history.list()).toEqual([
      { filename: 'gone.scm', deletedAt: at(1_000).toISOString() },
      { filename: 'hello.scm' },
    ])
  })

  test('is empty when nothing has been recorded', async () => {
    await fs.saveFile('hello.scm', 'one')

    expect(await history.list()).toEqual([])
  })
})

describe('fileOfHistory', () => {
  test('recovers the file a history belongs to', () => {
    expect(fileOfHistory(historyFilename('hello.scm'))).toBe('hello.scm')
    // Names with dots of their own survive the round trip.
    expect(fileOfHistory(historyFilename('a.b.scm'))).toBe('a.b.scm')
  })

  test('rejects names that are not histories', () => {
    expect(fileOfHistory('hello.scm')).toBeNull()
    expect(fileOfHistory('.scamper.config')).toBeNull()
    expect(fileOfHistory('.history')).toBeNull()
  })
})

describe('rename', () => {
  test('carries the history across to the new name', async () => {
    await history.record('hello.scm', '(display 1)', START)
    await history.rename('hello.scm', 'goodbye.scm')

    expect(await contentsOf('goodbye.scm')).toEqual(['(display 1)'])
    expect(await fs.fileExists(historyFilename('hello.scm'))).toBe(false)
  })

  test('does nothing for a file with no history', async () => {
    await history.rename('hello.scm', 'goodbye.scm')

    expect(await fs.fileExists(historyFilename('goodbye.scm'))).toBe(false)
  })
})

describe('markDeleted', () => {
  test('keeps the history, marked, so the file can be recovered', async () => {
    await history.record('hello.scm', '(display 1)', START)
    await history.markDeleted('hello.scm', at(1_000))

    const index = await history.index('hello.scm')
    expect(index.deletedAt).toBe(at(1_000).toISOString())
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })

  test('leaves no history behind for a file that had none', async () => {
    await history.markDeleted('hello.scm', START)

    expect(await fs.fileExists(historyFilename('hello.scm'))).toBe(false)
  })

  test('a later save clears the mark, even when it adds no snapshot', async () => {
    // Recreating a deleted file with its original contents: the save itself
    // adds nothing, but the history is no longer of a deleted file.
    await history.record('hello.scm', '(display 1)', START)
    await history.markDeleted('hello.scm', at(1_000))

    const result = await history.record('hello.scm', '(display 1)', at(2_000))

    expect(result.recorded).toBe(false)
    expect((await history.index('hello.scm')).deletedAt).toBeUndefined()
    expect(await contentsOf('hello.scm')).toEqual(['(display 1)'])
  })
})

import type {
  History,
  HistoryFile,
  HistoryIndex,
  RecordResult,
} from './history'

/**
 * A history that records nothing, for hosts that have no use for one.
 *
 * The CLI runs a program and exits; there is no editor, no autosave, and
 * nothing to recover. Handing it this rather than making a history optional
 * keeps every caller free of a null check for a case that only arises outside
 * the browser.
 */
export class NoHistory implements History {
  list(): Promise<HistoryFile[]> {
    return Promise.resolve([])
  }

  index(): Promise<HistoryIndex> {
    return Promise.resolve({ snapshots: [] })
  }

  read(): Promise<string | null> {
    return Promise.resolve(null)
  }

  record(): Promise<RecordResult> {
    return Promise.resolve({ head: null, recorded: false })
  }

  rename(): Promise<void> {
    return Promise.resolve()
  }

  markDeleted(): Promise<void> {
    return Promise.resolve()
  }
}

export default NoHistory

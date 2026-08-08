export type {
  History as t,
  History,
  HistoryFile,
  HistoryIndex,
  RecordOptions,
  RecordResult,
  Snapshot,
  SnapshotRef,
} from './history'

export { formatSnapshotTime } from './history'
export { MAX_SNAPSHOTS, MERGE_WINDOW_MS, addsNothing } from './policy'
export { FlatFileHistory } from './flat-file'
export { ServerHistory } from './server'
export { NoHistory } from './none'

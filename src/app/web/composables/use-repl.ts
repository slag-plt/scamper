import { ref, shallowRef, triggerRef, type Ref } from 'vue'
import Scamper, { type ReplSession } from '../../../scamper'
import type { ErrorChannel, OutputChannel, Value } from '../../../lpm'
import type { ScamperError } from '../../../lpm/error'

/**
 * The REPL window's transcript and the session behind it (#399).
 *
 * A REPL is scratch work beside the file: it is seeded from the file once, when
 * it opens, and from then on it is its own program. Nothing here reconciles
 * with the editor -- an entry that turns out to be worth keeping is copied into
 * the file by the person who wrote it.
 */

/** One entry: what was typed, and what it produced. */
export interface ReplEntry {
  id: number
  source: string
  /** What it printed, errors included, in the order it arrived. */
  values: Value[]
  isRunning: boolean
}

export interface Repl {
  /** Null until the REPL is opened, which is also what puts the panel up. */
  readonly session: Ref<ReplSession | null>
  readonly entries: Ref<ReplEntry[]>
  /** What the session was seeded from, shown above the first entry. */
  readonly banner: Ref<string>
  /** True while an entry is running, so the prompt can wait and offer Stop. */
  readonly isBusy: Ref<boolean>
  /**
   * Opens a session seeded from `src`, replacing one already open -- which is
   * also how the window restarts.
   */
  open: (fileName: string | null, src: string) => Promise<void>
  /** Runs `text` as the next entry. */
  submit: (text: string) => Promise<void>
  /** Abandons the entry in flight. */
  interrupt: () => void
  /** Closes the session and everything it left running. */
  close: () => void
}

export function useRepl(): Repl {
  // Shallow, both of them. A session is a handle with methods on it rather than
  // state, and an entry holds rendered values -- deep structures whose type
  // alone defeats Vue's reactive unwrapping. Updates are published by replacing
  // the array or by triggerRef, as the output pane does.
  const session = shallowRef<ReplSession | null>(null)
  const entries = shallowRef<ReplEntry[]>([])
  const banner = ref('')
  const isBusy = ref(false)
  let nextId = 0

  /**
   * Where output goes: the entry being run.
   *
   * One channel for the session rather than one per entry, because the session
   * takes one. Only one entry runs at a time, so the last entry is the one that
   * produced whatever arrives -- including, deliberately, output from a handler
   * an *earlier* entry registered, which lands in the transcript where it
   * happened rather than back under the entry that set it up.
   */
  const channel: OutputChannel & ErrorChannel = {
    send: (v: Value) => {
      record(v)
    },
    report: (e: ScamperError) => {
      record(e)
    },
    pushLevel: () => {
      /* a transcript is flat: an entry is one form, not a traced program */
    },
    popLevel: () => {
      /* as above */
    },
    get totalSends() {
      let n = 0
      for (const entry of entries.value) n += entry.values.length
      return n
    },
  }

  function record(v: Value): void {
    const entry = entries.value.at(-1)
    if (entry === undefined) return
    entry.values.push(v)
    triggerRef(entries)
  }

  async function open(fileName: string | null, src: string): Promise<void> {
    close()
    const repl = Scamper.getInstance().startRepl({ out: channel, err: channel })
    session.value = repl
    // An entry for whatever seeding reports -- a file that does not compile, or
    // one that fails half way -- so it has somewhere to land. Dropped again
    // below if it stays empty, which is the usual case.
    const seedEntry: ReplEntry = {
      id: nextId++,
      source: '',
      values: [],
      isRunning: true,
    }
    entries.value = [seedEntry]
    isBusy.value = true
    try {
      const seeded = await repl.seed(src)
      banner.value =
        fileName === null
          ? 'Starting from the standard library.'
          : seeded
            ? `Definitions from ${fileName} are available here. Nothing you type changes the file.`
            : `${fileName} did not run, so only the standard library is available here.`
    } finally {
      isBusy.value = false
      seedEntry.isRunning = false
      entries.value =
        seedEntry.values.length === 0 ? [] : [...entries.value]
    }
  }

  async function submit(text: string): Promise<void> {
    const repl = session.value
    if (repl === null || isBusy.value) return
    const entry: ReplEntry = {
      id: nextId++,
      source: text,
      values: [],
      isRunning: true,
    }
    entries.value = [...entries.value, entry]
    isBusy.value = true
    try {
      await repl.evaluate(text)
    } finally {
      entry.isRunning = false
      isBusy.value = false
      triggerRef(entries)
    }
  }

  function close(): void {
    session.value?.end()
    session.value = null
    entries.value = []
    banner.value = ''
    isBusy.value = false
  }

  return {
    session,
    entries,
    banner,
    isBusy,
    open,
    submit,
    interrupt: () => {
      session.value?.interrupt()
    },
    close,
  }
}

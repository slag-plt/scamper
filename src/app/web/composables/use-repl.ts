import { computed, ref, shallowRef, triggerRef, type Ref } from 'vue'
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
  /**
   * Whether it ran, and so counts as part of the program the next entry
   * continues. False for one that was refused or did not compile.
   */
  ran: boolean
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
   * True once the file has been edited since the session was seeded.
   *
   * The session is deliberately not reconciled with the file -- that is what
   * makes it scratch work -- so the most that can be done is to say so, as the
   * output pane says its results are out of date.
   */
  readonly isStale: Ref<boolean>
  /**
   * The program the next entry continues: the file the session was seeded from
   * followed by the entries so far.
   *
   * Not used to run anything -- the session's fiber is the environment. It is
   * what the language server analyses a cell inside, so that a name from the
   * file or from an earlier entry is in scope while it is being typed.
   */
  readonly context: Ref<string>
  /**
   * What has been typed at the prompt, oldest first.
   *
   * A record of the person's work rather than of the session, so `open` --
   * which is what Restart calls -- and `close` deliberately leave it alone,
   * unlike the transcript, which belongs to the session that produced it.
   */
  readonly history: Ref<string[]>
  /**
   * Opens a session seeded from `src`, replacing one already open -- which is
   * also how the window restarts.
   */
  open: (fileName: string | null, src: string) => Promise<void>
  /** Runs `text` as the next entry. */
  submit: (text: string) => Promise<void>
  /** Abandons the entry in flight. */
  interrupt: () => void
  /** Told when the file changes, which is what makes a session stale. */
  noteEdit: () => void
  /** Closes the session and everything it left running. */
  close: () => void
}

/**
 * How many typed entries the history keeps, so that a long session cannot grow
 * it without bound. The oldest fall off the front; what anyone recalls is near
 * the end.
 */
const HISTORY_LIMIT = 200

export function useRepl(): Repl {
  // Shallow, both of them. A session is a handle with methods on it rather than
  // state, and an entry holds rendered values -- deep structures whose type
  // alone defeats Vue's reactive unwrapping. Updates are published by replacing
  // the array or by triggerRef, as the output pane does.
  const session = shallowRef<ReplSession | null>(null)
  const entries = shallowRef<ReplEntry[]>([])
  const banner = ref('')
  const isBusy = ref(false)
  const isStale = ref(false)
  // Apart from the rest: everything above belongs to the session and is thrown
  // away with it, while this is the person's. Shallow and replaced wholesale
  // too -- a list of strings nobody edits in place needs nothing deeper.
  const history = shallowRef<string[]>([])
  // What the session was seeded from, kept for the context above.
  const seedSource = ref('')
  const context = computed(() =>
    [
      seedSource.value,
      // Only what ran. An entry that was refused or did not compile is not part
      // of the program, and one that does not parse would take the whole
      // context down with it -- leaving the analysis with nothing but the
      // standard library for the rest of the session.
      ...entries.value.filter((entry) => entry.ran).map((entry) => entry.source),
    ]
      .filter((src) => src.trim().length > 0)
      .join('\n'),
  )
  let nextId = 0
  /**
   * Which session the transcript belongs to.
   *
   * Opening twice in quick succession -- a double-clicked button, Restart while
   * a seed is still running -- would otherwise have the first call's `finally`
   * land on the second call's session: clearing its busy flag mid-seed and
   * replacing its entries with the ones it had itself.
   */
  let generation = 0

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
    // A closed session's handlers are torn down, but a value already on its way
    // must not resurrect a transcript that is gone.
    if (session.value === null) return
    let entry = entries.value.at(-1)
    if (entry === undefined) {
      // Nothing has been typed yet, so this came from something the seeded file
      // left running -- a timer, a key handler. It still has to be seen, so it
      // gets an entry of its own rather than being dropped on the floor.
      entry = { id: nextId++, source: '', values: [], isRunning: false, ran: false }
      entries.value = [...entries.value, entry]
    }
    entry.values.push(v)
    triggerRef(entries)
  }

  async function open(fileName: string | null, src: string): Promise<void> {
    close()
    const mine = ++generation
    const repl = Scamper.getInstance().startRepl({ out: channel, err: channel })
    session.value = repl
    seedSource.value = src
    // An entry for whatever seeding reports -- a file that does not compile, or
    // one that fails half way -- so it has somewhere to land. Dropped again
    // below if it stays empty, which is the usual case.
    const seedEntry: ReplEntry = {
      id: nextId++,
      source: '',
      values: [],
      isRunning: true,
      ran: false,
    }
    entries.value = [seedEntry]
    isBusy.value = true
    // Seeded from the file as it is right now, so whatever it was before does
    // not count against this session.
    isStale.value = false
    try {
      const seeded = await repl.seed(src)
      // Superseded while the file was running: everything below is about a
      // session that has already been closed and replaced.
      if (mine !== generation) return
      banner.value =
        fileName === null
          ? 'Starting from the standard library.'
          : seeded
            ? `Definitions from ${fileName} are available here. Nothing you type changes the file.`
            : `${fileName} did not run, so only the standard library is available here.`
    } finally {
      if (mine === generation) {
        isBusy.value = false
        seedEntry.isRunning = false
        // Nothing to show: drop it rather than leave a blank entry above the
        // banner. Anything that arrived after it -- from a handler the file
        // left running -- is kept.
        entries.value =
          seedEntry.values.length === 0
            ? entries.value.filter((entry) => entry !== seedEntry)
            : [...entries.value]
      }
    }
  }

  async function submit(text: string): Promise<void> {
    const repl = session.value
    if (repl === null || isBusy.value) return
    // Recorded before it runs and kept whatever becomes of it: one that was
    // refused or did not compile is the very thing someone wants back, to fix
    // the typo in it. A blank line and an immediate repeat are dropped, as a
    // shell drops them.
    if (text.trim().length > 0 && text !== history.value.at(-1)) {
      history.value = [...history.value, text].slice(-HISTORY_LIMIT)
    }
    const entry: ReplEntry = {
      id: nextId++,
      source: text,
      values: [],
      isRunning: true,
      ran: false,
    }
    entries.value = [...entries.value, entry]
    isBusy.value = true
    try {
      entry.ran = await repl.evaluate(text)
    } finally {
      entry.isRunning = false
      isBusy.value = false
      triggerRef(entries)
    }
  }

  /**
   * Ends the session and clears everything belonging to it.
   *
   * `history` is deliberately not among them: it is what the person typed, not
   * what the session produced, so it survives a close and the `open` that
   * Restart performs (#458).
   */
  function close(): void {
    session.value?.end()
    session.value = null
    entries.value = []
    banner.value = ''
    isBusy.value = false
    isStale.value = false
    seedSource.value = ''
  }

  return {
    session,
    entries,
    banner,
    isBusy,
    isStale,
    context,
    history,
    open,
    submit,
    interrupt: () => {
      session.value?.interrupt()
    },
    noteEdit: () => {
      // Only with a session open: an edit made before one is opened is part of
      // the file it will be seeded from, not a change to it.
      if (session.value !== null) isStale.value = true
    },
    close,
  }
}

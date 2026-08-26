/**
 * Where a run's output goes in a notebook (#410).
 *
 * The notebook runs the whole file, exactly as the Run button does -- it is one
 * program, not a cell at a time -- and the scheduler already announces each
 * source form on its way past through `beginStatement` (see
 * OutputChannel.beginStatement). This channel does nothing but file what
 * follows each announcement under the cell that form came from.
 *
 * Framework-free, so what lands where can be tested by feeding it a run's
 * announcements rather than by mounting a notebook.
 */
import type { Value } from '../../lpm'
import type { ErrorChannel, OutputChannel } from '../../lpm/output/channel'
import type { ScamperError } from '../../lpm/error'

/** A cell, as the channel needs to know it. */
export interface OutputSlot {
  /** Where the cell is in the document, for placing an error by its range. */
  from: number
  to: number
  /**
   * What `beginStatement` will announce for this cell, or '' for a prose cell,
   * which no announcement ever matches.
   */
  caption: string
}

export class NotebookDisplay implements OutputChannel, ErrorChannel {
  private slots: OutputSlot[] = []
  /** What each cell produced, one bucket per slot. */
  private buckets: Value[][] = []
  /**
   * Output belonging to no cell: what a program that would not compile
   * reported, when the error carries no range to place it by.
   */
  private preamble: Value[] = []
  /** The cell being run, or -1 before the first form is announced. */
  private at = -1
  private sends = 0

  /** @param onChange told whenever something arrives, so a view can redraw. */
  constructor(private readonly onChange: () => void = () => undefined) {}

  /**
   * Points the channel at the notebook's cells as they now stand, and empties
   * every bucket. Called before each run.
   */
  setSlots(slots: OutputSlot[]): void {
    this.slots = slots
    this.reset()
  }

  /** Empties every bucket, keeping the cells they belong to. */
  reset(): void {
    this.buckets = this.slots.map(() => [])
    this.preamble = []
    this.at = -1
    this.sends = 0
    this.onChange()
  }

  /** What the cell at `index` produced. Empty for a cell that printed nothing. */
  outputOf(index: number): Value[] {
    return this.buckets[index] ?? []
  }

  /** What belongs to no cell, shown above the notebook. */
  get unplaced(): Value[] {
    return this.preamble
  }

  /**
   * The next form of the program is starting.
   *
   * Matched by its text rather than taken in turn, so a form the split does not
   * know about -- one whose range points into a library, one typed since the
   * last split -- costs at most its own output rather than shifting every
   * cell's output down by one.
   */
  beginStatement(source: string): void {
    const from = this.at + 1
    for (let i = from; i < this.slots.length; i++) {
      if (this.slots[i].caption === source) {
        this.at = i
        return
      }
    }
    // No cell holds this form. Move to the next one that holds any, so what
    // follows still lands in program order instead of piling onto the cell
    // that happened to run last.
    for (let i = from; i < this.slots.length; i++) {
      if (this.slots[i].caption.length > 0) {
        this.at = i
        return
      }
    }
  }

  send(v: Value): void {
    this.sends++
    this.push(this.at < 0 ? this.preamble : this.buckets[this.at], v)
  }

  report(e: ScamperError): void {
    // Reported before anything ran, so it is a compile error: its range says
    // which cell it is about, which is where a student needs to see it.
    if (this.at < 0) {
      const cell = this.cellOf(e)
      this.push(cell === -1 ? this.preamble : this.buckets[cell], e)
      return
    }
    this.push(this.buckets[this.at], e)
  }

  pushLevel(): void {
    /* a cell's output is flat, as a REPL entry's is */
  }

  popLevel(): void {
    /* as above */
  }

  get totalSends(): number {
    return this.sends
  }

  /** @returns the cell `e` points into, or -1. */
  private cellOf(e: ScamperError): number {
    const at = e.range?.begin.idx
    if (at === undefined || at < 0) return -1
    return this.slots.findIndex(
      (slot) => slot.caption.length > 0 && at >= slot.from && at < slot.to,
    )
  }

  private push(bucket: Value[] | undefined, v: Value): void {
    // A run whose output outlives the notebook it was started from: the file
    // was closed, or the split moved under it.
    if (bucket === undefined) return
    bucket.push(v)
    this.onChange()
  }
}

import { Value } from '../lang.js'
import { ScamperError } from '../error.js'
import { ErrorChannel, OutputChannel } from './channel.js'

/**
 * Gathers the reductions of one statement into an array, so they can be paged
 * through afterwards rather than watched as they scroll past.
 *
 * Stepping is otherwise a one-way street: the scheduler emits each reduction
 * as it reaches it and there is no going back, and no way to know how many
 * there will be until the run ends. A window offering "step 12 of 35" and a
 * slider needs both, so it collects the whole trace first and seeks in it.
 *
 * Output from every other statement is discarded -- those statements still run,
 * since the traced one usually depends on what they defined.
 */
export class TraceCollector implements OutputChannel, ErrorChannel {
  readonly steps: Value[] = []
  /** True once `limit` was reached and the rest of the trace was dropped. */
  truncated = false

  /** The statement being executed, or -1 before the first is announced. */
  private current = -1

  /**
   * @param target index of the statement whose reductions to keep.
   * @param limit how many to keep before giving up, so a loop that never ends
   *        cannot fill memory or hang the page.
   * @param onLimit called once, when `limit` is hit, so the caller can stop the
   *        run rather than let it grind on producing steps nobody will see.
   */
  constructor(
    private readonly target: number,
    private readonly limit: number,
    private readonly onLimit: () => void,
  ) {}

  beginStatement(_source: string, index: number) {
    this.current = index
  }

  send(v: Value) {
    this.record(v)
  }

  report(e: ScamperError) {
    // An error ends the statement, and seeing it is the point of stepping
    // through one, so it is kept as the trace's last step.
    this.record(e)
  }

  private record(v: Value) {
    if (this.current !== this.target) return
    if (this.steps.length >= this.limit) {
      if (!this.truncated) {
        this.truncated = true
        this.onLimit()
      }
      return
    }
    this.steps.push(v)
  }

  pushLevel() {
    /* a trace is a flat list of steps; sections are the output pane's concern */
  }

  popLevel() {
    /* as above */
  }

  get totalSends(): number {
    return this.steps.length
  }
}

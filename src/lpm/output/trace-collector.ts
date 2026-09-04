import { Value } from '../lang.js'
import { ScamperError } from '../error.js'
import { ErrorChannel, OutputChannel } from './channel.js'

/**
 * How many reductions a single statement may take before the trace gives up.
 *
 * A step costs O(state size) -- the whole frame stack is rebuilt and rendered
 * for each one -- so n steps cost O(n^2), and a runaway statement is exactly
 * the case where the state keeps growing. Measured on `(factorial -1)`: the
 * IDE's old ceiling of 10,000 took 47s and a final step of 9.4MB, where 2000
 * takes under 2s -- a wait rather than a hung tab. It is also the scale of a
 * real recursive trace, `(fib 12)` being 2092 steps, so what it cuts short is
 * mostly what nobody was going to page through anyway.
 */
export const DEFAULT_TRACE_STEP_LIMIT = 2000

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
 * since the traced one usually depends on what they defined. The limit bounds
 * each statement's reductions rather than the array of kept steps, so a
 * statement either side of the traced one cannot loop forever unnoticed
 * (#369).
 */
export class TraceCollector implements OutputChannel, ErrorChannel {
  readonly steps: Value[] = []
  /**
   * True when the limit left the traced statement short of its end: it ran
   * past the limit, or a statement before it did and so it never ran at all.
   * A statement *after* it spending the limit stops the run too, but by then
   * the trace is whole.
   */
  truncated = false

  /** True once the limit stopped the run, so `onLimit` is called just once. */
  private stopped = false

  /** The statement being executed, or -1 before the first is announced. */
  private current = -1

  /** Reductions the current statement has produced, kept or not. */
  private emitted = 0

  /**
   * @param target index of the statement whose reductions to keep.
   * @param limit how many reductions one statement may take before giving up,
   *        so a loop that never ends cannot fill memory or hang the page.
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
    // Each statement is given the whole budget: the limit is a bound on one
    // statement, not on the run.
    this.emitted = 0
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
    // Charged before the target test, so that every statement is bounded and
    // not just the one being kept: a runaway statement earlier in the program
    // still has to run, and nothing but this stops it (#369). For the target
    // itself the two counts coincide, since each of its reductions is kept.
    if (this.emitted >= this.limit) {
      if (!this.stopped) {
        this.stopped = true
        // Whose limit was spent decides whether the trace is short: the traced
        // statement's own, or an earlier statement's, leaves steps missing. A
        // later statement's does not, and reporting one would tell the reader
        // that a trace they can see the end of stops early.
        this.truncated = this.current <= this.target
        this.onLimit()
      }
      return
    }
    this.emitted += 1
    if (this.current !== this.target) return
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

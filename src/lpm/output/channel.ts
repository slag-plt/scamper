import { Value } from '../lang.js'
import { ScamperError } from '../error.js'

/**
 * An abstract sink that can receive output from LPM. Output channels are also
 * structured in that output can be nested in a hierarchical structure.
 */
export interface OutputChannel {
  /**
   * Sends or "prints" the given value to this output channel.
   * @param v the value to print
   */
  send: (v: Value) => void

  /**
   * Pushes a new output level to this channel.
   * @param attrs attributes associated with this level
   */
  pushLevel: (...attrs: string[]) => void

  /**
   * Pops the current output level from this channel.
   */
  popLevel: () => void

  /**
   * Announces the source text of the statement whose output follows, so a
   * channel that can show it alongside the output has it to show.
   *
   * Optional because only the IDE's output pane does: a console or a log wants
   * the values alone. Called once per *source form*, in program order, whether
   * or not that form goes on to send anything -- once for a `struct`, not once
   * per define it expands into.
   *
   * @param index the position in the program of the first statement the form
   *        expanded into, so a channel collecting the output of one form can
   *        tell which is which.
   */
  beginStatement?: (source: string, index: number) => void

  readonly totalSends: number
}

/** An abstract sink that can receive errors from LPM. */
export interface ErrorChannel {
  report: (e: ScamperError) => void
}

import { Value } from "../lang.js"
import { ScamperError } from "../error.js"

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

  readonly totalSends: number
}

/** An abstract sink that can receive errors from LPM. */
export interface ErrorChannel {
  report: (e: ScamperError) => void
}

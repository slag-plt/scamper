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
   * @param label the label for this new level
   * @param attrs attributes associated with this level
   */
  pushLevel: (label: string, attrs: string[]) => void

  /**
   * Pops the current output level from this channel.
   */
  popLevel: () => void
}

/** An abstract sink that can receive errors from LPM. */
export interface ErrorChannel {
  report: (e: ScamperError) => void
}
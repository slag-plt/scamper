import { Value } from '../lang.js'
import { OutputChannel } from './channel.js'

/**
 * An output channel that drops everything sent to it.
 *
 * For a program run for what it defines rather than what it prints -- seeding a
 * REPL from the file it was opened on (#399). Omitting the channel entirely
 * would not do: a task with no `out` is a report task, which is a different
 * kind of run, so a program run that way would not print even where it should.
 */
export class DiscardOutput implements OutputChannel {
  private _totalSends = 0

  send(_v: Value) {
    this._totalSends++
  }

  pushLevel() {
    /* nothing is kept, so there is no structure to keep it in */
  }

  popLevel() {
    /* as above */
  }

  get totalSends(): number {
    return this._totalSends
  }
}

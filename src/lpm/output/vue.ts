import { ScamperError } from '../error'
import { Value } from '../lang'
import { ErrorChannel, OutputChannel } from './channel'

export interface DisplayCallbacks {
  StartSectionCallback: (...attrs: string[]) => void
  EndSectionCallback: () => void
  SendCallback: (value: Value) => void
  /** See OutputChannel.beginStatement. */
  SourceCallback: (source: string) => void
}

export interface TraceBlock {
  attrs: string[]
  value?: Value
  /**
   * The statement that produced the blocks after this one. A block carries
   * either this or a `value`, never both: it is a caption, not output.
   */
  source?: string
}

export class VueDisplay implements OutputChannel, ErrorChannel {
  private startSection: DisplayCallbacks['StartSectionCallback']
  private endSection: DisplayCallbacks['EndSectionCallback']
  private _send: DisplayCallbacks['SendCallback']
  private _source: DisplayCallbacks['SourceCallback']
  private _totalSends = 0

  constructor({
    StartSectionCallback,
    EndSectionCallback,
    SendCallback,
    SourceCallback,
  }: DisplayCallbacks) {
    this.startSection = StartSectionCallback
    this.endSection = EndSectionCallback
    this._send = SendCallback
    this._source = SourceCallback
  }

  send(v: Value) {
    this._send(v)
    this._totalSends++
  }

  pushLevel(...attrs: string[]) {
    this.startSection(...attrs)
  }

  popLevel() {
    this.endSection()
  }

  beginStatement(source: string) {
    this._source(source)
  }

  report(e: ScamperError) {
    this._send(e)
  }

  get totalSends() {
    return this._totalSends
  }
}

import { ScamperError } from '../error'
import { Value } from '../lang'
import { ErrorChannel, OutputChannel } from './channel'

export interface DisplayCallbacks {
  StartSectionCallback: (...attrs: string[]) => void
  EndSectionCallback: () => void
  SendCallback: (value: Value) => void
}

export interface TraceBlock {
  attrs: string[]
  value?: Value
}

export class VueDisplay implements OutputChannel, ErrorChannel {
  private startSection: DisplayCallbacks['StartSectionCallback']
  private endSection: DisplayCallbacks['EndSectionCallback']
  private _send: DisplayCallbacks['SendCallback']
  private _totalSends = 0

  constructor({
    StartSectionCallback,
    EndSectionCallback,
    SendCallback,
  }: DisplayCallbacks) {
    this.startSection = StartSectionCallback
    this.endSection = EndSectionCallback
    this._send = SendCallback
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

  report(e: ScamperError) {
    this._send(e)
  }

  get totalSends() {
    return this._totalSends
  }
}

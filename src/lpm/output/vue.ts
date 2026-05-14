import { ScamperError } from "../error"
import { Value } from "../lang"
import { ErrorChannel, OutputChannel } from "./channel"

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
  #startSection: DisplayCallbacks["StartSectionCallback"]
  #endSection: DisplayCallbacks["EndSectionCallback"]
  #send: DisplayCallbacks["SendCallback"]
  #totalSends = 0

  constructor({
    StartSectionCallback,
    EndSectionCallback,
    SendCallback,
  }: DisplayCallbacks) {
    this.#startSection = StartSectionCallback
    this.#endSection = EndSectionCallback
    this.#send = SendCallback
  }

  send(v: Value) {
    this.#send(v)
    this.#totalSends++
  }
  pushLevel(...attrs: string[]) {
    this.#startSection(...attrs)
  }
  popLevel() {
    this.#endSection()
  }
  report(e: ScamperError) {
    this.#send(e)
  }

  get totalSends() {
    return this.#totalSends
  }
}

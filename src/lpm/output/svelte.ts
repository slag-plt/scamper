import { ScamperError } from "../error"
import { Value } from "../lang"
import { ErrorChannel, OutputChannel } from "./channel"

export interface DisplayCallbacks {
  PushChildCallback: (element: HTMLElement) => void
  PopLevelCallback: () => void
  SendToCurrentLevelCallback: (value: Value) => void
}

export class SvelteDisplay implements OutputChannel, ErrorChannel {
  #pushChild: DisplayCallbacks["PushChildCallback"]
  #popLevel: DisplayCallbacks["PopLevelCallback"]
  #sendToCurrentLevel: DisplayCallbacks["SendToCurrentLevelCallback"]
  #totalSends = 0

  constructor({
    PushChildCallback,
    SendToCurrentLevelCallback,
    PopLevelCallback,
  }: DisplayCallbacks) {
    this.#pushChild = PushChildCallback
    this.#popLevel = PopLevelCallback
    this.#sendToCurrentLevel = SendToCurrentLevelCallback
  }

  send(v: Value) {
    this.#sendToCurrentLevel(v)
    this.#totalSends++
  }
  pushLevel(...attrs: string[]) {
    const elt = document.createElement("div")
    // HACK: if we're pushing a trace block, infuse it with an onclick to
    // collapse its enclosing trace-block, if it has one.
    if (attrs.includes("trace")) {
      elt.addEventListener("click", (_e) => {
        for (const child of elt.children) {
          if (
            child instanceof HTMLElement &&
            child.classList.contains("trace-block")
          ) {
            child.classList.toggle("collapsed")
          }
        }
      })
    }
    elt.classList.add(...attrs)
    this.#pushChild(elt)
  }
  popLevel() {
    this.#popLevel()
  }
  report(e: ScamperError) {
    this.#sendToCurrentLevel(e)
  }

  get totalSends() {
    return this.#totalSends
  }
}

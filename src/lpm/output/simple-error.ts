import { ErrorChannel } from "./channel"
import { ScamperError, SubthreadErrors } from "../error"

export class SimpleErrorChannel implements ErrorChannel {
  readonly errors: ScamperError[]

  constructor(errors?: ScamperError[]) {
    this.errors = []
    if (errors) {
      this.errors.push(...errors)
    }
  }

  report(e: ScamperError) {
    this.errors.push(e)
  }

  getSubthreadErrors() {
    return new SubthreadErrors(this.errors)
  }
}

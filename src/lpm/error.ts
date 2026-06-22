import { Range } from "./range.js"
import { Value } from "./lang"

/** Phases of scamper execution, used for the purposes of error reporting. */
type Phase = "Parser" | "Runtime"

/** Errors that arise during Scamper compilation and execution. */
export class ScamperError extends Error {
  phase: Phase
  modName?: string
  range?: Range
  source?: string

  constructor(
    phase: Phase,
    msg: string,
    modName?: string,
    range?: Range,
    source?: string,
  ) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
    this.source = source
  }

  toString(): string {
    const detail = `${this.modName ?? ""}${this.range && this.range !== Range.none ? this.range.toString() : ""}`
    const src = this.source ? `(${this.source}) ` : ""
    return `${this.phase} error${detail.length > 0 ? " [" + detail + "]" : ""}: ${src}${this.message}`
  }
}

/** Internal compiler errors arise due to bugs in Scamper. */
export class ICE extends Error {
  funcName: string

  constructor(funcName: string, msg: string) {
    super(msg)
    this.funcName = funcName
  }

  toString(): string {
    return `ICE (${this.funcName}): ${this.message}\n${this.stack ?? ""}`
  }
}

// TODO: likely deprecated after fiber change
export class SubthreadErrors extends Error {
  errors: ScamperError[]
  constructor(errors: ScamperError[]) {
    const msg = errors.map((e) => e.toString()).join(" ")
    super(msg)
    this.errors = [...errors]
  }
}

export class ReportError extends ScamperError {
  constructor(
    public value: Value,
    public range: Range,
  ) {
    super("Runtime", "Fiber hit a report operation!")
  }
}

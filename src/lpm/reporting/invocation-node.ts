import { Env, ScamperFn, Value } from "../lang"

export interface InvocationNode {
  // applied fn
  fn: ScamperFn
  // env bindings and inputs to fn
  env: Env
  args: Value[]
  // child invocations from this one
  children: InvocationNode[]
  // resulting value of fn ap
  result?: Value
  // denotes which ap this is within its parent
  apIdx: number
}

/** The stable root of one report evaluation; it is not an invocation itself. */
export interface ReportCaptureRoot {
  children: InvocationNode[]
}

/** Runtime state shared by frames evaluating one reported expression. */
export interface ReportCapture {
  root: ReportCaptureRoot
  stack: InvocationNode[]
}

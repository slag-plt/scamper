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
  // selected branches for matches evaluated within this invocation
  matches: MatchOutcome[]
}

/** One control-flow decision needed to reconstruct a completed page. */
export interface MatchOutcome {
  /** Static label for the match operation in the invocation's code block. */
  matchIdx: number
  /** The value against which the match selected its branch. */
  scrutinee: Value
  /** The index of the selected branch in the static match operation. */
  branchIdx: number
}

/** The stable root of one report evaluation; it is not an invocation itself. */
export interface ReportTraceRoot {
  children: InvocationNode[]
}

/** Temporary runtime state shared by frames evaluating one reported expression. */
export interface ReportTrace {
  root: ReportTraceRoot
  stack: InvocationNode[]
  /** First invocation of the selected application in the owning report. */
  targetNode?: InvocationNode
}

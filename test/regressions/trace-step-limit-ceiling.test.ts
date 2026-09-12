import { afterEach, describe, expect, test } from 'vitest'
import { DEFAULT_TRACE_STEP_LIMIT } from '../../src/lpm/output/trace-collector'
import {
  MAX_TRACE_STEP_LIMIT,
  setTraceStepLimit,
  traceStepLimit,
} from '../../src/app/web/run-prefs'

// Regression for #529: the preferences pane's trace step limit topped out at
// 100,000, four fifths of which is a dead tab rather than a slow one. There is
// no worker, so collection runs on the main thread and a limit the student is
// invited to type is a limit the page has to survive.
//
// Measured on `(factorial -1)` -- the runaway #369 was written against -- in
// Node 24, each limit in its own process, heap read after a forced collection:
//
//   limit   wall    CPU     retained heap
//   1,000   0.23s   0.45s   83 MB
//   2,500   1.2s    1.8s    264 MB     (the default)
//   3,500   2.1s    2.8s    455 MB
//   5,000   4.2s    5.4s    786 MB
//   7,500   10.2s   13.2s   1,773 MB
//
// Both curves are quadratic in the limit, so the fit extrapolates to ~3.0 GB
// and ~20s at 10,000 and ~11 GB at 20,000 -- the browser is worse still, since
// the trace is handed to a deep `ref` and so every step carries a reactive
// proxy too. 5,000 is the largest round limit measured to stay under a gigabyte
// and under about five seconds: a pause, which is the guarantee #369 exists to
// make, rather than a tab that does not come back.
const SURVIVABLE_TRACE_STEPS = 5_000

afterEach(() => {
  // Module-level and self-persisting, so a limit left behind here would be the
  // one the next test file in this process reads.
  setTraceStepLimit(DEFAULT_TRACE_STEP_LIMIT)
})

describe('the trace step limit the pane offers (#529)', () => {
  test('tops out at a limit measured to survive', () => {
    expect(MAX_TRACE_STEP_LIMIT).toBeLessThanOrEqual(SURVIVABLE_TRACE_STEPS)
  })

  // The pane clamps rather than refuses, so what keeps a typed 100,000 from
  // reaching the collector is the ceiling itself.
  test('clamps a limit typed past the ceiling down to it', () => {
    setTraceStepLimit(100_000)
    expect(traceStepLimit.value).toBeLessThanOrEqual(SURVIVABLE_TRACE_STEPS)
  })
})

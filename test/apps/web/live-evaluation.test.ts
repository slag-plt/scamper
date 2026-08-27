import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import {
  DEFAULT_IDLE_MS,
  DEFAULT_RUN_LIMIT_MS,
  useLiveEvaluation,
  type LiveEvaluationHooks,
} from '../../../src/app/web/composables/use-live-evaluation'
import {
  liveEvaluation,
  setLiveEvaluation,
} from '../../../src/app/web/run-prefs'

/**
 * Live evaluation (issue #378) is entirely a matter of timing: how long after
 * an edit the program runs, whether a burst of typing costs one run or many,
 * and when a run that will not end is stopped. So these drive the composable
 * directly with fake timers rather than through a mounted IDE.
 */

/** A stand-in for the session, recording what live evaluation asked of it. */
function mockHooks(overrides: Partial<LiveEvaluationHooks> = {}) {
  const state = {
    runs: 0,
    stops: 0,
    timeouts: [] as number[],
    /** What `currentRunId` reports; null stands for "nothing is running". */
    runId: null as string | null,
    allowed: true,
  }
  const hooks: LiveEvaluationHooks = {
    run: () => {
      state.runs += 1
      state.runId = `run-${state.runs.toString()}`
    },
    stopRun: () => {
      state.stops += 1
      state.runId = null
    },
    currentRunId: () => state.runId,
    canRun: () => state.allowed,
    reportTimeout: (limitMs) => state.timeouts.push(limitMs),
    ...overrides,
  }
  return { state, hooks }
}

describe('live evaluation', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    // The preference persists to localStorage by design, so start from the
    // default rather than from whatever the last test left.
    setLiveEvaluation(true)
  })

  afterEach(() => {
    vi.useRealTimers()
    setLiveEvaluation(true)
  })

  /** Lets the idle timer fire and the run it schedules settle. */
  async function idle(ms = DEFAULT_IDLE_MS) {
    await vi.advanceTimersByTimeAsync(ms)
  }

  test('is on by default', () => {
    expect(liveEvaluation.value).toBe(true)
  })

  test('runs after the user stops typing, not while they are typing', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS - 1)
    expect(state.runs).toBe(0)
    expect(live.pending.value).toBe(true)

    await vi.advanceTimersByTimeAsync(1)
    expect(state.runs).toBe(1)
    expect(live.pending.value).toBe(false)
  })

  test('coalesces a burst of typing into a single run', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    // Ten keystrokes, each within the idle window of the last.
    for (let i = 0; i < 10; i++) {
      live.noteEdit()
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS / 2)
    }
    expect(state.runs).toBe(0)

    await idle()
    expect(state.runs).toBe(1)
  })

  test('runs again for edits made after a run', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    live.noteEdit()
    await idle()
    expect(state.runs).toBe(2)
  })

  test('does nothing while the preference is off', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)
    setLiveEvaluation(false)

    live.noteEdit()
    expect(live.pending.value).toBe(false)
    await idle()
    expect(state.runs).toBe(0)
  })

  test('does not schedule a run the gate refuses', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)
    state.allowed = false

    live.noteEdit()
    await idle()
    expect(state.runs).toBe(0)
  })

  test('drops a scheduled run if the gate closes before it fires', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    // Halfway through the wait, a step starts (or the file is closed).
    await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS / 2)
    state.allowed = false
    await idle()
    expect(state.runs).toBe(0)
  })

  test('runNow() runs at once, without waiting out the idle delay', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    await live.runNow()
    expect(state.runs).toBe(1)
  })

  test('runNow() supersedes a run the last keystroke had scheduled', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await live.runNow()
    expect(state.runs).toBe(1)
    expect(live.pending.value).toBe(false)

    // The timer it replaced must not fire a second run behind it.
    await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS * 2)
    expect(state.runs).toBe(1)
  })

  test('runNow() stands down if cancel() lands while it is starting', async () => {
    const { state, hooks } = mockHooks()
    // A run the test holds open, so cancel() can land mid-flight.
    let finishStarting: () => void = () => {
      /* replaced by the mock below */
    }
    const live = useLiveEvaluation({
      ...hooks,
      run: () =>
        new Promise<void>((resolve) => {
          finishStarting = () => {
            state.runs += 1
            state.runId = `run-${state.runs.toString()}`
            resolve()
          }
        }),
    })

    const starting = live.runNow()
    live.cancel() // the file was switched away in the meantime
    finishStarting()
    await starting

    // The run went ahead, but it is no longer live evaluation's to watch:
    // claiming it would undo the cancel, and the watchdog would later stop it
    // and blame live evaluation for it.
    expect(state.runs).toBe(1)
    expect(live.liveRunId.value).toBe(null)
    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS * 2)
    expect(state.stops).toBe(0)
    expect(state.timeouts).toEqual([])
  })

  test('runNow() respects the preference and the gate', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    setLiveEvaluation(false)
    await live.runNow()
    expect(state.runs).toBe(0)

    setLiveEvaluation(true)
    state.allowed = false
    await live.runNow()
    expect(state.runs).toBe(0)
  })

  test('cancel() drops a pending run', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    live.cancel()
    expect(live.pending.value).toBe(false)
    await idle()
    expect(state.runs).toBe(0)
  })

  test('stops a live run that outlives the time limit, and says why', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    expect(state.runs).toBe(1)

    // The program is still going one tick short of the limit...
    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS - 1)
    expect(state.stops).toBe(0)

    await vi.advanceTimersByTimeAsync(1)
    expect(state.stops).toBe(1)
    expect(state.timeouts).toEqual([DEFAULT_RUN_LIMIT_MS])
  })

  test('leaves a run that finishes in time alone', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    state.runId = null // the run finished by itself

    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS * 2)
    expect(state.stops).toBe(0)
    expect(state.timeouts).toEqual([])
  })

  /**
   * The watchdog belongs to the run that armed it. A manual run has no time
   * limit -- it is the escape hatch from this one -- so a watchdog left over
   * from an earlier live run must not reach it.
   */
  test('does not stop a later run that is not the one it was watching', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    // The live run ends and the user presses Run on a deliberately long program.
    state.runId = 'manual-run'

    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS * 2)
    expect(state.stops).toBe(0)
    expect(state.runId).toBe('manual-run')
  })

  test('arms no watchdog when the program did not start', async () => {
    // A file that does not compile schedules nothing, so there is nothing to
    // watch and nothing to stop.
    const { state, hooks } = mockHooks({ run: () => undefined })
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS * 2)
    expect(state.stops).toBe(0)
    expect(state.timeouts).toEqual([])
  })

  test('a new live run gets the full allowance, not the leftover of the old', async () => {
    const { state, hooks } = mockHooks()
    // Short delays so the second run can start while the first still has time
    // left, which is the case this is about.
    const live = useLiveEvaluation(hooks, { idleMs: 100, runLimitMs: 1000 })

    live.noteEdit()
    await vi.advanceTimersByTimeAsync(100) // run 1 starts, watched until t=1100
    await vi.advanceTimersByTimeAsync(800)
    live.noteEdit()
    await vi.advanceTimersByTimeAsync(100) // t=1000: run 2 supersedes run 1
    expect(state.runs).toBe(2)

    // Past where run 1's watchdog would have fired, so it did not outlive the
    // run it belonged to.
    await vi.advanceTimersByTimeAsync(200)
    expect(state.stops).toBe(0)

    // Run 2 is stopped a full limit after *it* started, not after run 1 did.
    await vi.advanceTimersByTimeAsync(800)
    expect(state.stops).toBe(1)
  })

  /**
   * The watchdog is about the run, not about the keyboard: typing again does
   * not buy a program that is already looping forever any more time.
   */
  test('stops a run that hits the limit while the user is still typing', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks)

    live.noteEdit()
    await idle()
    // The user types on while the program spins; the limit still applies.
    await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS - 100)
    live.noteEdit()
    await vi.advanceTimersByTimeAsync(100)
    expect(state.stops).toBe(1)
  })

  test('honours the delays it is given', async () => {
    const { state, hooks } = mockHooks()
    const live = useLiveEvaluation(hooks, { idleMs: 20, runLimitMs: 50 })

    live.noteEdit()
    await vi.advanceTimersByTimeAsync(20)
    expect(state.runs).toBe(1)

    await vi.advanceTimersByTimeAsync(50)
    expect(state.stops).toBe(1)
    expect(state.timeouts).toEqual([50])
  })
})

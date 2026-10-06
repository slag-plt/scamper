// Regression (#725): a reactive component's message queue was unbounded.
// `update` pushed every message and `processQueue` spawned one fiber per
// message, taking the next only when that one finished -- so when messages
// arrived faster than they could be processed, the queue grew linearly and
// forever. The symptom a student reports is responsiveness: a click or keypress
// sits behind the whole backlog, so the program answers input from seconds ago
// and never catches up.
//
// The fix classifies messages. A *sampled* message (a timer tick, a hover
// position) reports a continuous signal, so a newer one stands in for an older
// one still waiting. A *discrete* message (click, button, key, note) is
// something the student did, and every one is delivered, in order.
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import * as L from '../../src/lpm'
import {
  music_makeNoteHandlers,
  type NoteHandlers,
} from '../../src/js/music/index.js'
import {
  reactive_onKeyDown,
  reactive_onKeyUp,
  reactive_onMouseClick,
  reactive_onMouseHover,
  reactive_onNote,
  reactive_onTimer,
  reactive_reactiveCanvas,
  reactive_reactiveContainer,
} from '../../src/js/reactive'
import {
  clearRun,
  runFibers as runHeldFibers,
  stubRun,
  type Fiber,
} from '../reactive-run'

/** One delivered message, as a short string, so a spec reads as a sequence. */
function summary(msg: L.Value): string {
  const m = msg as unknown as Record<string, unknown>
  const kind = String(m[L.structKind])
  switch (kind) {
    case 'event-timer':
      return `timer(${String(m.elapsed)})`
    case 'event-mouse-hover':
      return `hover(${String(m.x)},${String(m.y)})`
    case 'event-note':
      return `note(${String(m.id)})`
    case 'event-key-down':
    case 'event-key-up':
      return `${kind}(${String(m.key)})`
    default:
      return kind
  }
}

describe('#725: a reactive component that falls behind', () => {
  let fibers: Fiber[]
  /** Every message the update function was actually handed, in order. */
  let delivered: string[]

  /** Records what it is given, so the spec can assert on the delivery. */
  const update = (msg: L.Value, st: L.Value) => {
    delivered.push(summary(msg))
    return (st as number) + 1
  }

  /** Runs every held fiber, including any spawned by one that runs. */
  function runFibers(): void {
    runHeldFibers(fibers)
  }

  beforeEach(() => {
    fibers = []
    delivered = []
    stubRun(fibers)
    // The canvas asks for an animation frame in its constructor, and jsdom has
    // none. Nothing here drives the draw loop: a view fiber would only add
    // noise to the fiber queue these tests step through.
    vi.stubGlobal('requestAnimationFrame', () => 0)
    // `performance` as well as the timers: reactive_onTimer reads
    // performance.now() for `elapsed`, so without faking it the elapsed sums
    // asserted below are whatever the wall clock happened to do.
    vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval', 'performance'] })
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.unstubAllGlobals()
    clearRun()
  })

  /** A canvas whose view does nothing, since no frame is ever drawn here. */
  function canvasWith(...subs: Parameters<typeof reactive_reactiveCanvas>[5][]) {
    return reactive_reactiveCanvas(
      100,
      50,
      0,
      () => undefined,
      update,
      ...subs,
    )
  }

  test('delivers the ticks it missed as one, with their time summed', () => {
    canvasWith(reactive_onTimer(10))

    // One tick, whose update is left in flight.
    vi.advanceTimersByTime(10)
    expect(fibers).toHaveLength(1)
    // Four more arrive behind it.
    vi.advanceTimersByTime(40)

    runFibers()

    // The four are one message, and no time is lost: 10 + 40 is the 50ms that
    // actually passed. Unbounded, this was five messages of 10.
    expect(delivered).toEqual(['timer(10)', 'timer(40)'])
  })

  test('delivers only the latest hover position', () => {
    const canvas = canvasWith(reactive_onMouseHover())
    const move = (x: number, y: number) => {
      canvas.dispatchEvent(new MouseEvent('mousemove', { clientX: x, clientY: y }))
    }

    move(1, 1)
    expect(fibers).toHaveLength(1)
    move(2, 2)
    move(3, 3)

    runFibers()

    expect(delivered).toEqual(['hover(1,1)', 'hover(3,3)'])
  })

  // The case that a back-of-the-queue-only merge would get wrong: with both a
  // timer and hover subscribed, arrivals alternate, so the newest message is
  // never the same kind as the one at the back and nothing would ever merge.
  test('coalesces each kind even when the kinds interleave', () => {
    const canvas = canvasWith(reactive_onTimer(10), reactive_onMouseHover())
    const move = (x: number, y: number) => {
      canvas.dispatchEvent(new MouseEvent('mousemove', { clientX: x, clientY: y }))
    }

    vi.advanceTimersByTime(10)
    expect(fibers).toHaveLength(1)

    // timer, hover, timer, hover -- alternating.
    vi.advanceTimersByTime(10)
    move(1, 1)
    vi.advanceTimersByTime(10)
    move(2, 2)

    runFibers()

    // Two messages behind the one in flight, not four.
    expect(delivered).toEqual(['timer(10)', 'timer(20)', 'hover(2,2)'])
  })

  test('never drops or reorders anything the student did', () => {
    const canvas = canvasWith(
      reactive_onTimer(10),
      reactive_onMouseClick(),
      reactive_onKeyDown(),
      reactive_onKeyUp(),
    )
    const click = () => {
      canvas.dispatchEvent(new MouseEvent('click'))
    }
    const key = (type: 'keydown' | 'keyup', k: string) => {
      document.dispatchEvent(new KeyboardEvent(type, { key: k }))
    }

    vi.advanceTimersByTime(10)
    expect(fibers).toHaveLength(1)

    click()
    key('keydown', 'a')
    vi.advanceTimersByTime(10)
    key('keyup', 'a')
    click()
    // A second tick, which merges with the first one waiting and so moves to
    // the back -- where it belongs, since that is when it happened.
    vi.advanceTimersByTime(10)

    runFibers()

    expect(delivered).toEqual([
      'timer(10)',
      'event-mouse-click',
      'event-key-down(a)',
      'event-key-up(a)',
      'event-mouse-click',
      'timer(20)',
    ])
  })

  test('delivers every note, including a chord that fires at once', () => {
    const handlers: NoteHandlers = music_makeNoteHandlers()
    canvasWith(reactive_onNote(handlers))
    const play = (id: string) => {
      handlers.forEach((h) => {
        h({
          [L.scamperTag]: 'struct',
          [L.structKind]: 'event-note',
          id,
        })
      })
    }

    play('root')
    expect(fibers).toHaveLength(1)
    play('third')
    play('fifth')

    runFibers()

    expect(delivered).toEqual(['note(root)', 'note(third)', 'note(fifth)'])
  })

  test('changes nothing for a program that keeps up', () => {
    canvasWith(reactive_onTimer(10))

    // Each tick is fully processed before the next arrives, so nothing is ever
    // waiting and no message is ever merged.
    for (let i = 0; i < 5; i++) {
      vi.advanceTimersByTime(10)
      runFibers()
    }

    expect(delivered).toEqual([
      'timer(10)',
      'timer(10)',
      'timer(10)',
      'timer(10)',
      'timer(10)',
    ])
  })

  test('bounds the queue however far behind it falls', () => {
    canvasWith(reactive_onTimer(10))

    vi.advanceTimersByTime(10)
    expect(fibers).toHaveLength(1)
    // A thousand more ticks with nothing able to run.
    vi.advanceTimersByTime(10_000)

    runFibers()

    // Two updates rather than 1001, and all 10.01 seconds still accounted for.
    expect(delivered).toEqual(['timer(10)', 'timer(10000)'])
  })

  test('bounds a reactive container the same way', () => {
    reactive_reactiveContainer(
      0,
      () => document.createElement('div'),
      update,
      reactive_onTimer(10),
    )
    // The container renders its initial view on construction, so the first
    // held fiber is that view rather than an update.
    runFibers()

    vi.advanceTimersByTime(10)
    expect(fibers).toHaveLength(1)
    vi.advanceTimersByTime(40)

    runFibers()

    expect(delivered).toEqual(['timer(10)', 'timer(40)'])
  })
})

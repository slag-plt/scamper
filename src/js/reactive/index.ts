import * as L from '../../lpm'

import { NoteHandlers, NoteMsg } from '../music/index.js'
import { context2d } from '../image/context.js'
import { requireBrowser } from '../browser.js'

/***** Reactive Components ****************************************************/

/**
 * A ReactiveElement is an element that reacts to messages.
 */
export interface ReactiveElement {
  draw (): void
  update (msg: Msg): void
  getElement (): HTMLElement
}

/**
 * A Subscription is a function that registers an event of interest with
 * a ReactiveElement.
 */
interface Subscription extends L.Struct {
  [L.scamperTag]: 'struct',
  [L.structKind]: 'subscription',
  register: (react: ReactiveElement) => void
}

export function reactive_subscriptionQ(v: L.Value): boolean {
  return L.isStructKind(v, 'subscription')
}

/**
 * A ReactiveCanvas is a canvas that acts as a view over a model of type T.
 */
class ReactiveCanvas<T> implements ReactiveElement {
  // The run this component belongs to, captured at construction -- which
  // happens while the program is stepping. Its draw loop and its updates fire
  // long afterwards, when a lookup would find the foreground program instead
  // (#375).
  private readonly run: L.RunHandle = L.currentRun()

  canvas: HTMLCanvasElement
  state: T
  /* (state: T, canvas: HTMLCanvasElement) => void */
  viewFunc: L.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: L.ScamperFn
  isDirty: boolean
  finished: boolean
  // Update and view can no longer be called synchronously from JS -- each runs
  // as a fiber (L.spawn). Messages are queued and applied one at a time (so the
  // model is never mutated by two overlapping update fibers), and the view is
  // coalesced on the rAF loop: at most one view fiber in flight, run only when
  // the model is dirty.
  private queue: Msg[] = []
  private updating = false
  private drawing = false

  constructor(
      width: number,
      height: number,
      state: T,
      view: L.ScamperFn,
      update: L.ScamperFn) {
    this.canvas = document.createElement('canvas')
    this.canvas.width = width
    this.canvas.height = height
    this.state = state
    this.viewFunc = view
    this.updateFunc = update
    this.isDirty = true
    this.finished = false
    // Stop the draw loop (and short-circuit updates) when the program is
    // re-run/stopped, so this component doesn't leak into the next run.
    this.run.signal?.addEventListener('abort', () => {
      this.finished = true
    })

    const doFrame = (_time: number) => {
      this.draw()
      if (!this.finished) {
        requestAnimationFrame(doFrame)
      }
    }

    requestAnimationFrame(doFrame)
  }

  getState() { return this.state }

  draw () {
    if (this.finished || this.drawing || !this.isDirty) {
      return
    }
    this.drawing = true
    this.isDirty = false
    context2d(this.canvas).clearRect(0, 0, this.canvas.width, this.canvas.height)
    this.run.spawn(this.viewFunc, [this.state as L.Value, this.canvas], (result) => {
      this.drawing = false
      // A view error is reported to the output pane (result === null); stop.
      if (result === null) {
        this.finished = true
      }
    })
  }

  update (msg: Msg) {
    if (this.finished) {
      return
    }
    this.queue.push(msg)
    this.processQueue()
  }

  private processQueue() {
    if (this.updating || this.finished || this.queue.length === 0) {
      return
    }
    this.updating = true
    const msg = L.shiftRequired(this.queue, 'the message queue')
    this.run.spawn(this.updateFunc, [msg, this.state as L.Value], (newState) => {
      if (newState === null) {
        this.finished = true // an update error was reported to the output pane
      } else {
        this.state = newState as T
        this.isDirty = true
      }
      this.updating = false
      this.processQueue()
    })
  }

  getElement (): HTMLElement { return this.canvas }
}

export function reactive_reactiveCanvas (
    width: number,
    height: number,
    init: L.Value,
    view: L.ScamperFn,
    update: L.ScamperFn,
    ...subscriptions: Subscription[]): HTMLCanvasElement {
  requireBrowser()
  const react = new ReactiveCanvas(width, height, init, view, update)
  subscriptions.forEach(sub => { sub.register(react) })
  return react.getElement() as HTMLCanvasElement
}

/**
 * A ReactiveContainer is an HTML subtree that acts as a view over a model of
 * type T.
 */
class ReactiveContainer<T> implements ReactiveElement {
  // The run this component belongs to, captured at construction -- which
  // happens while the program is stepping. Its draw loop and its updates fire
  // long afterwards, when a lookup would find the foreground program instead
  // (#375).
  private readonly run: L.RunHandle = L.currentRun()

  container: HTMLDivElement
  state: T
  /* (state: T) => HTMLElement */
  viewFunc: L.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: L.ScamperFn
  finished: boolean
  // As with ReactiveCanvas, update and view run as fibers (L.spawn). Messages
  // are processed one at a time, each fully (update, then re-render) before the
  // next, so a message never sees a half-applied update.
  private queue: Msg[] = []
  private processing = false

  constructor(state: T, view: L.ScamperFn, update: L.ScamperFn) {
    this.container = document.createElement('div')
    this.state = state
    this.viewFunc = view
    this.updateFunc = update
    this.finished = false
    // Stop processing messages when the program is re-run/stopped, so this
    // component doesn't leak into the next run.
    this.run.signal?.addEventListener('abort', () => {
      this.finished = true
    })
  }

  // Renders the current state: run the view fiber, then swap its HTMLElement
  // result into the container. N.B., a virtual-DOM diff would be more efficient
  // than the full re-render here.
  private renderView(onDone?: () => void) {
    this.run.spawn(this.viewFunc, [this.state as L.Value], (result) => {
      if (result instanceof HTMLElement) {
        this.container.innerHTML = ''
        this.container.appendChild(result)
      }
      // A view error (result === null) is reported to the output pane; a
      // non-HTMLElement result leaves the current contents in place.
      onDone?.()
    })
  }

  draw () {
    this.renderView()
  }

  update (msg: Msg) {
    if (this.finished) {
      return
    }
    this.queue.push(msg)
    this.processQueue()
  }

  private processQueue() {
    if (this.processing || this.finished || this.queue.length === 0) {
      return
    }
    this.processing = true
    const msg = L.shiftRequired(this.queue, 'the message queue')
    this.run.spawn(this.updateFunc, [msg, this.state as L.Value], (newState) => {
      if (newState === null) {
        this.finished = true // an update error was reported to the output pane
        this.processing = false
        return
      }
      this.state = newState as T
      this.renderView(() => {
        this.processing = false
        this.processQueue()
      })
    })
  }

  getElement (): HTMLElement { return this.container }
}

export function reactive_reactiveContainer(
    init: L.Value,
    view: L.ScamperFn,
    update: L.ScamperFn,
    ...subscriptions: Subscription[]): HTMLDivElement {
  requireBrowser()
  const react = new ReactiveContainer(init, view, update)
  subscriptions.forEach(sub => { sub.register(react) })
  react.draw()
  return react.getElement() as HTMLDivElement
}

/***** Messages and Events ****************************************************/

type Msg =
    ButtonClickMsg
  | MouseClickMsg
  | MouseHoverMsg
  | KeyDownMsg
  | KeyUpMsg
  | TimerMsg
  | NoteMsg   // from music

interface ButtonClickMsg extends L.Struct {
  [L.structKind]: 'event-button-click',
  id: string
}

interface MouseClickMsg extends L.Struct {
  [L.structKind]: 'event-mouse-click',
  button: number, x: number, y: number
}

interface MouseHoverMsg extends L.Struct {
  [L.structKind]: 'event-mouse-hover',
  x: number, y: number
}

interface TimerMsg extends L.Struct {
  [L.structKind]: 'event-timer',
  time: number, elapsed: number
}

interface KeyDownMsg extends L.Struct {
  [L.structKind]: 'event-key-down',
  key: string
}

interface KeyUpMsg extends L.Struct {
  [L.structKind]: 'event-key-up',
  key: string
}

function subscription(sub: (react: ReactiveElement) => void): Subscription {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'subscription', register: sub }
}

// N.B., every subscription registers its listener/timer with the current run's
// AbortSignal so it is torn down when the program is re-run/stopped (see
// currentRunSignal); otherwise a previous run's events would keep updating.

export function reactive_onButtonClick(button: HTMLButtonElement): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    button.addEventListener('click', () => {
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-button-click',
        id: button.id
      })
    }, { signal: run.signal })
  })
}

export function reactive_onMouseClick(): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    react.getElement().addEventListener('click', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-mouse-click',
        button: event.button, x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    }, { signal: run.signal })
  })
}

export function reactive_onMouseHover(): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    react.getElement().addEventListener('mousemove', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-mouse-hover',
        x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    }, { signal: run.signal })
  })
}

export function reactive_onKeyDown(): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    document.addEventListener('keydown', (event) => {
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-key-down',
        key: event.key
      })
    }, { signal: run.signal })
  })
}

export function reactive_onKeyUp(): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    document.addEventListener('keyup', (event) => {
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-key-up',
        key: event.key
      })
    }, { signal: run.signal })
  })
}

export function reactive_onTimer(interval: number): Subscription {
  // Captured here rather than inside the callback below, which may run
  // after the step that registered it (#375).
  const run = L.currentRun()
  return subscription((react) => {
    let time = performance.now()
    const signal = run.signal
    const id = setInterval(() => {
      const now = performance.now()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-timer',
        time: now, elapsed: now - time
      })
      time = now
    }, interval)
    signal?.addEventListener('abort', () => { clearInterval(id) })
  })
}

export function reactive_onNote(handlers: NoteHandlers): Subscription {
  // No run captured: this registers no listener and no timer, so there is
  // nothing to tear down and nothing to spawn.
  return subscription((react) => {
    handlers.push((msg) => { react.update(msg) })
  })
}

/***** Exports ****************************************************************/

// Reactive components

// Subscriptions

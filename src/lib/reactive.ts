import { checkContract, contract } from './contract.js'
import * as C from './contract.js'
import * as L from '../lpm'

import { NoteHandlers, NoteMsg } from './music.js'

const lib: L.Library = new L.Library()

/***** Reactive Components ****************************************************/

/**
 * A ReactiveElement is an element that reacts to messages.
 */
interface ReactiveElement {
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

/**
 * A ReactiveCanvas is a canvas that acts as a view over a model of type T.
 */
class ReactiveCanvas implements ReactiveElement {
  canvas: HTMLCanvasElement
  state: L.Value
  /* (state: T, canvas: HTMLCanvasElement) => void */
  viewFunc: L.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: L.ScamperFn
  isDirty: boolean
  finished: boolean

  constructor(
      width: number,
      height: number,
      state: L.Value,
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

    const doFrame = (): void => {
      this.draw()
      if (!this.finished) {
        requestAnimationFrame(doFrame)
      }
    }
    requestAnimationFrame(doFrame)
  }

  getState() { return this.state }

  draw () {
    if (this.isDirty) {
      const ctx = this.canvas.getContext('2d')
      if (ctx === null) {
        throw new Error('2d canvas context is not available')
      }
      ctx.clearRect(0, 0, this.canvas.width, this.canvas.height)
      try {
        L.callScamperFn(this.viewFunc, this.state, this.canvas)
      } catch (e) {
        alert(`reactive-canvas: view function generated an error:\n\n${(e as Error).toString()}`)
        this.finished = true
      }
      this.isDirty = false
    }
  }

  update (msg: Msg) {
    try {
      this.state = L.callScamperFn(this.updateFunc, msg, this.state)
    } catch (e) {
      alert(`reactive-canvas: update function generated an error:\n\n${(e as Error).toString()}`)
      this.finished = true
    }
    this.isDirty = true
  }

  getElement (): HTMLElement { return this.canvas }
}

function reactiveCanvas (
  ...args: [number, number, L.Value, L.ScamperFn, L.ScamperFn, ...Subscription[]]): HTMLCanvasElement {
checkContract(args, contract('reactive-canvas', [C.nonneg, C.nonneg, C.any, C.func, C.func], C.struct('subscription')))
const [width, height, init, view, update, ...subscriptions] = args
const react = new ReactiveCanvas(width, height, init, view, update)
subscriptions.forEach(sub => { sub.register(react) })
return react.canvas
}

/**
 * A ReactiveContainer is an HTML subtree that acts as a view over a model of
 * type T.
 */
class ReactiveContainer implements ReactiveElement {
  container: HTMLDivElement
  state: L.Value
  /* (state: T) => HTMLElement */
  viewFunc: L.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: L.ScamperFn

  constructor(state: L.Value, view: L.ScamperFn, update: L.ScamperFn) {
    this.container = document.createElement('div')
    this.state = state
    this.viewFunc = view
    this.updateFunc = update
  }

  draw () {
    // N.B., this is where a virtual DOM implementation with diffing ala
    // react would be much more efficient.
    this.container.innerHTML = ''
    try {
      const result = L.callScamperFn(this.viewFunc, this.state)
      if (result instanceof HTMLElement) {
        this.container.appendChild(result)
      } else {
        alert(`reactive-container: view function must return an HTMLElement, but received ${L.typeOf(result)}`)
      }
    } catch (e) {
      alert(`reactive-container: update function generated an error:\n\n${(e as Error).toString()}`)
    }
  }

  update (msg: Msg) {
    try {
      this.state = L.callScamperFn(this.updateFunc, msg, this.state)
    } catch (e) {
      alert(`reactive-container: update function generated an error:\n\n${(e as Error).toString()}`)
    }
    this.draw()
  }

  getElement (): HTMLElement { return this.container }
}

function reactiveContainer (
  ...args: [L.Value, L.ScamperFn, L.ScamperFn, ...Subscription[]]): HTMLDivElement {
checkContract(args, contract('reactive-container', [C.any, C.func, C.func], C.struct('subscription')))
const [init, view, update, ...subscriptions] = args
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

function onButtonClick (...args: [HTMLButtonElement]): Subscription {
  checkContract(args, contract('on-button-click', [C.html]))
  const [button] = args
  return subscription((react) => {
    button.addEventListener('click', () => {
      react.update({ 
        [L.scamperTag]: 'struct', [L.structKind]: 'event-button-click',
        id: button.id
      })
    })
  })
}

function onMouseClick (...args: []): Subscription {
  checkContract(args, contract('on-mouse-click', []))
  return subscription((react) => {
    react.getElement().addEventListener('click', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-mouse-click',
        button: event.button, x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    })
  })
}

function onMouseHover (...args: []): Subscription {
  checkContract(args, contract('on-mouse-hover', []))
  return subscription((react) => {
    react.getElement().addEventListener('mousemove', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-mouse-hover',
        x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    })
  })
}

function onKeyDown (...args: []): Subscription {
  checkContract(args, contract('on-key-down', []))
  return subscription((react) => {
    document.addEventListener('keydown', (event) => {
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-key-down',
        key: event.key
      })
    })
  })
}

function onKeyUp (...args: []): Subscription {
  checkContract(args, contract('on-key-up', []))
  return subscription((react) => {
    document.addEventListener('keyup', (event) => {
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-key-up',
        key: event.key
      })
    })
  })
}

function onTimer (...args: [number]): Subscription {
  checkContract(args, contract('on-timer', [C.nonneg]))
  const [interval] = args
  return subscription((react) => {
    let time = performance.now()
    setInterval(() => {
      const now = performance.now()
      react.update({
        [L.scamperTag]: 'struct', [L.structKind]: 'event-timer',
        time: now, elapsed: now - time
      })
      time = now
    }, interval)
  })
}

function onNote (...args: [NoteHandlers]): Subscription {
  checkContract(args, contract('on-note', [C.any]))
  const [handlers] = args
  return subscription((react) => {
    handlers.push((msg) => { react.update(msg) })
  })
}

/***** Exports ****************************************************************/

// Reactive components
lib.registerValue('reactive-canvas', reactiveCanvas)
lib.registerValue('reactive-container', reactiveContainer)

// Subscriptions
lib.registerValue('on-button-click', onButtonClick)
lib.registerValue('on-mouse-click', onMouseClick)
lib.registerValue('on-mouse-hover', onMouseHover)
lib.registerValue('on-key-down', onKeyDown)
lib.registerValue('on-key-up', onKeyUp)
lib.registerValue('on-timer', onTimer)
lib.registerValue('on-note', onNote)

export default lib
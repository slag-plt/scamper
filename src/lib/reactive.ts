import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { emptyLibrary, Library, registerValue, Value } from '../lang.js'
import { callFunction } from '../sem.js'

const lib: Library = emptyLibrary()

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
interface Subscription extends Value.Struct {
  [Value.scamperTag]: 'struct',
  [Value.structKind]: 'subscription',
  register: (react: ReactiveElement) => void
}

/**
 * A ReactiveCanvas is a canvas that acts as a view over a model of type T.
 */
class ReactiveCanvas<T> implements ReactiveElement {
  canvas: HTMLCanvasElement
  state: T
  /* (state: T, canvas: HTMLCanvasElement) => void */
  viewFunc: Value.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: Value.ScamperFn
  isDirty: boolean
  finished: boolean

  constructor(
      width: number,
      height: number,
      state: T,
      view: Value.ScamperFn,
      update: Value.ScamperFn) {
    this.canvas = document.createElement('canvas')
    this.canvas.width = width
    this.canvas.height = height
    this.state = state
    this.viewFunc = view
    this.updateFunc = update
    this.isDirty = true
    this.finished = false

    var blob = this
    function doFrame(_time: number) {
      blob.draw()
      if (!blob.finished) {
        requestAnimationFrame(doFrame)
      }
    }
    requestAnimationFrame(doFrame)
  }

  getState() { return this.state }

  draw () {
    if (this.isDirty) {
      this.canvas.getContext('2d')!.clearRect(0, 0, this.canvas.width, this.canvas.height)
      try {
        callFunction(this.viewFunc, this.state, this.canvas)
      } catch (e) {
        alert(`reactive-canvas: view function generated an error:\n\n${(e as Error).toString()}`)
        this.finished = true
      }
      this.isDirty = false
    }
  }

  update (msg: Msg) {
    try {
      this.state = callFunction(this.updateFunc, msg, this.state)
    } catch (e) {
      alert(`reactive-canvas: update function generated an error:\n\n${(e as Error).toString()}`)
      this.finished = true
    }
    this.isDirty = true
  }

  getElement (): HTMLElement { return this.canvas }
}

function reactiveCanvas<T> (
    width: number,
    height: number,
    init: T,
    view: Value.ScamperFn,
    update: Value.ScamperFn,
    ...subscriptions: Subscription[]): HTMLCanvasElement {
  checkContract(arguments, contract('reactive-canvas', [C.nonneg, C.nonneg, C.any, C.func, C.func], C.struct('subscription')))
  var react = new ReactiveCanvas(width, height, init, view, update)
  subscriptions.forEach(sub => { sub.register(react) })
  return react.getElement() as HTMLCanvasElement
}

/**
 * A ReactiveContainer is an HTML subtree that acts as a view over a model of
 * type T.
 */
class ReactiveContainer<T> implements ReactiveElement {
  container: HTMLDivElement
  state: T
  /* (state: T) => HTMLElement */
  viewFunc: Value.ScamperFn
  /* (msg: Msg, state: T) => T */
  updateFunc: Value.ScamperFn

  constructor(state: T, view: Value.ScamperFn, update: Value.ScamperFn) {
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
      const result = callFunction(this.viewFunc, this.state)
      if (result instanceof HTMLElement) {
        this.container.appendChild(result)
      } else {
        alert(`reactive-container: view function must return an HTMLElement, but received ${Value.typeOf(result)}`)
      }
    } catch (e) {
      alert(`reactive-container: update function generated an error:\n\n${(e as Error).toString()}`)
    }
  }

  update (msg: Msg) {
    try {
      this.state = callFunction(this.updateFunc, msg, this.state)
    } catch (e) {
      alert(`reactive-container: update function generated an error:\n\n${(e as Error).toString()}`)
    }
    this.draw()
  }

  getElement (): HTMLElement { return this.container }
}

function reactiveContainer<T>(
    init: T,
    view: Value.ScamperFn,
    update: Value.ScamperFn,
    ...subscriptions: Subscription[]): HTMLDivElement {
  checkContract(arguments, contract('reactive-container', [C.any, C.func, C.func], C.struct('subscription')))
  var react = new ReactiveContainer(init, view, update)
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

interface ButtonClickMsg extends Value.Struct {
  [Value.structKind]: 'event-button-click',
  id: string
}

interface MouseClickMsg extends Value.Struct {
  [Value.structKind]: 'event-mouse-click',
  button: number, x: number, y: number
}

interface MouseHoverMsg extends Value.Struct {
  [Value.structKind]: 'event-mouse-hover',
  x: number, y: number
}

interface TimerMsg extends Value.Struct {
  [Value.structKind]: 'event-timer',
  time: number, elapsed: number
}

interface KeyDownMsg extends Value.Struct {
  [Value.structKind]: 'event-key-down',
  key: string
}

interface KeyUpMsg extends Value.Struct {
  [Value.structKind]: 'event-key-up',
  key: string
}

function subscription(sub: (react: ReactiveElement) => void): Subscription {
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'subscription', register: sub }
}

function onButtonClick (button: HTMLButtonElement): Subscription {
  checkContract(arguments, contract('on-button-click', [C.html]))
  return subscription((react) => {
    button.addEventListener('click', () => {
      react.update({ 
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-button-click',
        id: button.id
      })
    })
  })
}

function onMouseClick (): Subscription {
  checkContract(arguments, contract('on-mouse-click', []))
  return subscription((react) => {
    react.getElement().addEventListener('click', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-mouse-click',
        button: event.button, x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    })
  })
}

function onMouseHover (): Subscription {
  checkContract(arguments, contract('on-mouse-hover', []))
  return subscription((react) => {
    react.getElement().addEventListener('mousemove', (event) => {
      const rect = react.getElement().getBoundingClientRect()
      react.update({
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-mouse-hover',
        x: event.clientX - rect.left, y: event.clientY - rect.top
      })
    })
  })
}

function onKeyDown (): Subscription {
  checkContract(arguments, contract('on-key-down', []))
  return subscription((react) => {
    document.addEventListener('keydown', (event) => {
      react.update({
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-key-down',
        key: event.key
      })
    })
  })
}

function onKeyUp (): Subscription {
  checkContract(arguments, contract('on-key-up', []))
  return subscription((react) => {
    document.addEventListener('keyup', (event) => {
      react.update({
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-key-up',
        key: event.key
      })
    })
  })
}

function onTimer (interval: number): Subscription {
  checkContract(arguments, contract('on-timer', [C.nonneg]))
  return subscription((react) => {
    let time = performance.now()
    setInterval(() => {
      const now = performance.now()
      react.update({
        [Value.scamperTag]: 'struct', [Value.structKind]: 'event-timer',
        time: now, elapsed: now - time
      })
      time = now
    }, interval)
  })
}

/***** Exports ****************************************************************/

// Reactive components
registerValue('reactive-canvas', reactiveCanvas, lib)
registerValue('reactive-container', reactiveContainer, lib)

// Subscriptions
registerValue('on-button-click', onButtonClick, lib)
registerValue('on-mouse-click', onMouseClick, lib)
registerValue('on-mouse-hover', onMouseHover, lib)
registerValue('on-key-down', onKeyDown, lib)
registerValue('on-key-up', onKeyUp, lib)
registerValue('on-timer', onTimer, lib)

export default lib
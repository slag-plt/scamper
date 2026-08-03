import { Fiber } from './fiber.js'
import { Value } from './lang.js'

/**
 * Language services bundle together related functions that allow LPM to work
 * generically over that language. A language service is paramterized by
 * the type representing expressions over that language. Furthermore it is
 * assumed that the expression type is registered with appropriate renderers.
 */
export interface FiberRaiser<Exp> {
  /** Raises the given thread back to an expression */
  raise: (fiber: Fiber) => Exp
  /** Compares two expressions for equality */
  equals: (a: Exp, b: Exp) => boolean
}

/**
 * A stateful, per-run reduction stepper. `render` gives the fiber's current
 * state as the next user-visible reduction step -- a renderable Value -- or
 * undefined to skip (an internal or repeated step); `final` gives a completed
 * statement's value as its last step, or undefined if it duplicates the last.
 * Encapsulates the language-specific raise/sugar/granularity policy so the
 * (language-generic) scheduler can drive step mode without importing it.
 */
export interface FiberTraceStepper {
  render: (fiber: Fiber) => Value | undefined
  final: (value: Value) => Value | undefined
}
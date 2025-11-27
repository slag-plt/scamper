import { Thread } from './lang.js'

/**
 * Language services bundle together related functions that allow LPM to work
 * generically over that language. A language service is paramterized by
 * the type representing expressions over that language. Furthermore it is
 * assumed that the expression type is registered with appropriate renderers.
 */
export type Raiser<Exp> = {
  /** Raises the given thread back to an expression */
  raise: (thread: Thread) => Exp
  /** Compares two expressions for equality */
  equals: (a: Exp, b: Exp) => boolean
}
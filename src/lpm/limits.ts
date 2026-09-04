/**
 * Bounds on the machine's resource use that a program is allowed to know about.
 *
 * A separate module from fiber.ts because index.ts deliberately does not
 * re-export fiber.js, yet a library primitive -- `set-maximum-recursion-depth!`
 * -- has to validate its argument against the ceiling.
 */

/** The call stack depth a fresh Fiber starts with, absent a session's choice. */
export const DEFAULT_MAX_CALL_STACK_DEPTH = 10_000

/**
 * The largest depth `set-maximum-recursion-depth!` will accept.
 *
 * Scamper's frames live in an array and the scheduler steps through them
 * iteratively, so a deep recursion costs memory rather than JS stack: measured
 * at roughly 1 KB and 0.04 ms per frame, 200,000 frames is about 200 MB and 8
 * seconds. That is the point where the machine can still report the depth error
 * it exists to report, and it leaves headroom over the case that motivated the
 * limit being settable at all -- a naive recursion over a 400x300 image, at
 * 120,000 frames.
 */
export const MAX_CALL_STACK_DEPTH = 200_000

/**
 * The shallowest depth a *default* may be set to.
 *
 * Higher than the 1 `set-maximum-recursion-depth!` accepts, and deliberately: a
 * program may ask for a depth of 1 to show what the limit does, but that is its
 * own fiber's business, whereas a default is what every fiber starts at --
 * including the ones running library code the student did not write. `sort`
 * merges recursively and so spends about a frame per element: at a default of
 * 100, `(sort (range 200) <)` fails with an error naming a number the student
 * set in a dialog and has long since forgotten. 1,000 keeps the library usable
 * at the sizes a class works with while still being far below the default.
 */
export const MIN_CALL_STACK_DEPTH = 1_000

// The depth in force for fibers built from now on. Module state, which the rest
// of the machine avoids, because it is a session's setting rather than a
// program's: an app sets it once and every Fiber built afterwards starts there,
// with no depth threaded through the nine places one is constructed.
let sessionMaxCallStackDepth: number = DEFAULT_MAX_CALL_STACK_DEPTH

/** The call stack depth a Fiber built now starts with (see Fiber.pushFrame). */
export function defaultMaxCallStackDepth(): number {
  return sessionMaxCallStackDepth
}

/**
 * Sets that depth, for fibers built after this call; one already running keeps
 * the depth it started with. Clamped rather than refused, since the caller is a
 * preference rather than a program -- `set-maximum-recursion-depth!` is the one
 * that reports a bad number back to the student.
 *
 * @param n the depth to start fresh fibers at, clamped to
 *        [{@link MIN_CALL_STACK_DEPTH}, {@link MAX_CALL_STACK_DEPTH}]. Not a
 *        number at all leaves the depth alone: NaN survives both Math.min and
 *        Math.max, and `frames.length >= NaN` is false however deep the stack,
 *        so clamping it would quietly remove the limit rather than set one.
 */
export function setDefaultMaxCallStackDepth(n: number): void {
  if (!Number.isFinite(n)) return
  sessionMaxCallStackDepth = Math.min(
    MAX_CALL_STACK_DEPTH,
    Math.max(MIN_CALL_STACK_DEPTH, Math.round(n)),
  )
}

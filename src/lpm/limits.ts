/**
 * Bounds on the machine's resource use that a program is allowed to know about.
 *
 * A separate module from fiber.ts because index.ts deliberately does not
 * re-export fiber.js, yet a library primitive -- `set-maximum-recursion-depth!`
 * -- has to validate its argument against the ceiling.
 */

/** The call stack depth a fresh Fiber starts with (see Fiber.pushFrame). */
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

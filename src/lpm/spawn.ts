import { Value } from './lang'
import { ScamperError } from './error'

/**
 * Runs the Scamper closure `fn` applied to `args` as a fresh fiber; `onComplete`
 * receives its result (or null if it errored / was skipped).
 */
export type SpawnFn = (
  fn: Value,
  args: Value[],
  onComplete?: (result: Value | null) => void,
) => void

let impl: SpawnFn | undefined

/**
 * Registered by the Scamper singleton at startup. This indirection lets library
 * event handlers (src/js/*) spawn a fiber WITHOUT importing the heavy Scamper
 * singleton -- an eager import of scamper.ts from those modules would run at
 * test-setup time and grab real, unmocked transitive deps (see test/setup.ts).
 */
export function setSpawn(fn: SpawnFn): void {
  impl = fn
}

/**
 * Runs `(fn ...args)` as a new fiber in the active program's top-level
 * environment (see Scamper.spawnClosure). Used by event/callback library
 * functions now that JS code can no longer call Scamper closures directly.
 */
export function spawn(
  fn: Value,
  args: Value[],
  onComplete?: (result: Value | null) => void,
): void {
  if (impl === undefined) {
    throw new ScamperError(
      'Runtime',
      'Cannot run a callback: no Scamper program is active',
    )
  }
  impl(fn, args, onComplete)
}

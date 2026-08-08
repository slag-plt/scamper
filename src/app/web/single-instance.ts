// Two Scamper tabs on one origin share one file system, and both autosave, so
// the second silently clobbers the first. This guard lets only one tab run.
//
// The Web Locks API is the right primitive because the *browser* owns the
// lock's lifetime: it is released when the tab closes or crashes. That is what
// lets this be a plain held lock rather than the timestamped `.scamper.lock`
// file it replaces -- there is no abandoned lock to time out, so no heartbeat,
// no staleness window, and no writes to the file system at all. It also fixes
// two faults of that design: a sleeping machine could make a live lock look
// stale, and the release-on-hidden/re-acquire-on-visible dance left a gap in
// which two tabs could both end up live.
//
// Scope: locks are per-origin per-browser-profile, which is exactly OPFS's
// scope, so this covers precisely the tabs that share storage. A server-backed
// file system (#357) is shared more widely than that -- other browsers, other
// machines -- and is deliberately not this module's problem: excluding a
// student from their own files on a second machine would defeat the point of
// storing them centrally, so concurrent writes there are settled per-file by
// the server, which can arbitrate where this cannot.

const LOCK_NAME = 'scamper-single-instance'

// Set while the lock is held; calling it resolves the promise keeping the lock
// alive, which is how the Web Locks API is told we are done with it.
let release: (() => void) | null = null

/**
 * Acquires the single-instance lock, held until {@link releaseLock} or until
 * the tab goes away. Acquiring when already held is a no-op.
 * @returns true if this tab may run, false if another live tab holds the lock
 */
export function acquireLock(): Promise<boolean> {
  if (release !== null) {
    return Promise.resolve(true)
  }
  // Web Locks needs a secure context. Without it we have no way to detect a
  // second tab, so run anyway: the worst case is the multi-tab hazard that
  // predates any guard, which beats refusing to start the IDE.
  if (!('locks' in navigator)) {
    return Promise.resolve(true)
  }
  return new Promise<boolean>((resolve) => {
    void navigator.locks.request(LOCK_NAME, { ifAvailable: true }, (lock) => {
      if (lock === null) {
        resolve(false)
        return
      }
      resolve(true)
      // The lock is held for as long as this callback's promise is pending, so
      // leave it unsettled until releaseLock() resolves it.
      return new Promise<void>((done) => {
        release = () => {
          release = null
          done()
        }
      })
    })
  })
}

/** Releases the lock, if held. */
export function releaseLock(): void {
  release?.()
}

/** @returns true iff this tab currently holds the lock. */
export function holdsLock(): boolean {
  return release !== null
}

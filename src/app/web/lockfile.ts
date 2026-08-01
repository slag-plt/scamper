import * as FS from '../../fs'

export const lockfileName = '.scamper.lock'

// The active instance refreshes the lock's timestamp every HEARTBEAT_MS. A lock
// whose timestamp is older than STALE_MS is treated as abandoned -- its owner
// crashed or was force-closed without releasing it -- so a new instance takes it
// over. The app releases the lock whenever the tab is hidden and re-acquires it
// when visible (see IdeApp.vue's visibility handlers), so the heartbeat only ever
// runs in the foreground, where timers aren't throttled; that keeps a fresh lock
// reliably fresh and lets STALE_MS stay tight.
//
// N.B., this is a stopgap. A crashed tab used to leave a lock that blocked every
// later session forever; the timeout fixes that, but a truly robust
// single-instance guard (e.g. the Web Locks API) is still wanted -- among other
// gaps, a machine that sleeps with Scamper in the foreground can make a live
// lock look stale.
const HEARTBEAT_MS = 5_000
const STALE_MS = 20_000

let heartbeat: ReturnType<typeof setInterval> | undefined

/** The lock's timestamp in ms since the epoch, or null if it's absent or unreadable. */
async function readLockTime(fs: FS.t): Promise<number | null> {
  if (!(await fs.fileExists(lockfileName))) {
    return null
  }
  const time = new Date(await fs.loadFile(lockfileName)).getTime()
  return Number.isNaN(time) ? null : time
}

async function stampLock(fs: FS.t): Promise<void> {
  await fs.saveFile(lockfileName, new Date().toISOString())
}

function stopHeartbeat(): void {
  if (heartbeat !== undefined) {
    clearInterval(heartbeat)
    heartbeat = undefined
  }
}

/**
 * Attempts to acquire the single-instance lock. Succeeds when no lock exists or
 * the existing one is stale (older than STALE_MS); fails when a live instance is
 * still refreshing it. On success, starts a heartbeat that keeps the lock fresh
 * until {@link releaseLockFile} is called.
 * @returns true if the lock was acquired, false if another live instance holds it
 */
export async function acquireLockFile(fs: FS.t): Promise<boolean> {
  const lockTime = await readLockTime(fs)
  if (lockTime !== null && Date.now() - lockTime < STALE_MS) {
    return false
  }
  await stampLock(fs)
  stopHeartbeat()
  heartbeat = setInterval(() => {
    void stampLock(fs).catch(() => {
      // A transient write failure just means one skipped heartbeat.
    })
  }, HEARTBEAT_MS)
  return true
}

/** Stops the heartbeat and removes the lock, if present. */
export async function releaseLockFile(fs: FS.t): Promise<void> {
  stopHeartbeat()
  if (await fs.fileExists(lockfileName)) {
    await fs.deleteFile(lockfileName)
  }
}

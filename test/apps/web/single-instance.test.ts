import { afterEach, describe, expect, test } from 'vitest'
import * as SingleInstance from '../../../src/app/web/single-instance'

/** The subset of `LockGrantedCallback` this module actually uses. */
type Granted<T> = (lock: Lock | null) => T | PromiseLike<T>

/**
 * A stand-in for `navigator.locks` modelling the one behaviour under test: an
 * `ifAvailable` request is granted only when no one holds the name, and the
 * holder keeps it until the promise it returns settles.
 */
class FakeLockManager {
  private readonly held = new Set<string>()

  /** Simulates another live tab holding `name` until the returned function runs. */
  hold(name: string): () => void {
    this.held.add(name)
    return () => this.held.delete(name)
  }

  isHeld(name: string): boolean {
    return this.held.has(name)
  }

  request<T>(name: string, options: LockOptions, callback: Granted<T>): Promise<T> {
    if (options.ifAvailable !== true) {
      throw new Error('this fake only models ifAvailable requests')
    }
    if (this.held.has(name)) {
      return Promise.resolve(callback(null))
    }
    this.held.add(name)
    return Promise.resolve(callback({ name, mode: 'exclusive' })).finally(() => {
      this.held.delete(name)
    })
  }
}

const LOCK_NAME = 'scamper-single-instance'

function installLocks(): FakeLockManager {
  const manager = new FakeLockManager()
  Object.defineProperty(navigator, 'locks', {
    value: manager,
    configurable: true,
  })
  return manager
}

function uninstallLocks(): void {
  // @ts-expect-error -- jsdom has no `locks`; removing the stub restores that.
  delete navigator.locks
}

describe('single-instance guard', () => {
  afterEach(() => {
    SingleInstance.releaseLock()
    uninstallLocks()
  })

  test('acquires when no other tab holds the lock', async () => {
    const locks = installLocks()
    expect(await SingleInstance.acquireLock()).toBe(true)
    expect(SingleInstance.holdsLock()).toBe(true)
    expect(locks.isHeld(LOCK_NAME)).toBe(true)
  })

  test('is refused while another tab holds the lock', async () => {
    const locks = installLocks()
    locks.hold(LOCK_NAME)
    expect(await SingleInstance.acquireLock()).toBe(false)
    expect(SingleInstance.holdsLock()).toBe(false)
  })

  test('releasing hands the lock to the next acquirer', async () => {
    const locks = installLocks()
    await SingleInstance.acquireLock()
    SingleInstance.releaseLock()
    // The browser's own bookkeeping frees the name once our callback settles.
    await Promise.resolve()
    expect(SingleInstance.holdsLock()).toBe(false)
    expect(locks.isHeld(LOCK_NAME)).toBe(false)
    expect(await SingleInstance.acquireLock()).toBe(true)
  })

  test('re-acquiring while held is a no-op', async () => {
    installLocks()
    expect(await SingleInstance.acquireLock()).toBe(true)
    expect(await SingleInstance.acquireLock()).toBe(true)
    // One release is enough, because only one lock was ever taken.
    SingleInstance.releaseLock()
    expect(SingleInstance.holdsLock()).toBe(false)
  })

  test('releasing when nothing is held is harmless', () => {
    installLocks()
    expect(() => {
      SingleInstance.releaseLock()
    }).not.toThrow()
    expect(SingleInstance.holdsLock()).toBe(false)
  })

  test('a tab that goes away frees the lock without releasing it', async () => {
    const locks = installLocks()
    await SingleInstance.acquireLock()
    // Standing in for tab death: the browser drops the holder's claim, and the
    // next tab is admitted even though release() never ran. This is the whole
    // reason for using Web Locks over a timestamped file -- no stale lock.
    locks.hold(LOCK_NAME)()
    expect(locks.isHeld(LOCK_NAME)).toBe(false)
  })

  test('runs anyway where Web Locks is unavailable', async () => {
    uninstallLocks()
    expect('locks' in navigator).toBe(false)
    // Degrading to the pre-existing multi-tab hazard beats refusing to start.
    expect(await SingleInstance.acquireLock()).toBe(true)
  })
})

// jsdom provides a working localStorage, but Node 26 defines a global
// `localStorage` of its own that is `undefined` unless --localstorage-file is
// passed, and that is what wins under the vitest jsdom environment. So tests
// that touch storage install this instead: the same Storage contract, in
// memory, per test.

class MemoryStorage implements Storage {
  private readonly entries = new Map<string, string>()

  get length(): number {
    return this.entries.size
  }

  clear(): void {
    this.entries.clear()
  }

  getItem(key: string): string | null {
    return this.entries.get(key) ?? null
  }

  key(index: number): string | null {
    return [...this.entries.keys()][index] ?? null
  }

  removeItem(key: string): void {
    this.entries.delete(key)
  }

  setItem(key: string, value: string): void {
    this.entries.set(key, value)
  }
}

/** Installs a fresh in-memory `window.localStorage`, returning it. */
export function installMemoryStorage(): Storage {
  const storage = new MemoryStorage()
  Object.defineProperty(window, 'localStorage', {
    value: storage,
    configurable: true,
  })
  return storage
}

/** Removes the stub, restoring whatever the environment had. */
export function uninstallMemoryStorage(): void {
  // @ts-expect-error -- putting back the environment's (absent) global.
  delete window.localStorage
}

import { afterEach, describe, expect, test } from 'vitest'
import {
  readStoredConfig,
  writeStoredConfig,
  type Config,
} from '../../../src/app/web/ide-config'
import {
  installMemoryStorage,
  uninstallMemoryStorage,
} from '../../stubs/memory-storage'

const STORAGE_KEY = 'scamper.config'

const CONFIG: Config = {
  lastOpenedFilename: 'hello.scm',
  lastVersionAccessed: '3.5.0',
  localFilesOffered: false,
  recentFiles: ['hello.scm'],
}

/** Installs a storage whose every method throws, as a blocked browser's does. */
function installHostileStorage(): void {
  const boom = () => {
    throw new Error('storage is blocked')
  }
  Object.defineProperty(window, 'localStorage', {
    value: {
      get length(): number {
        return boom()
      },
      clear: boom,
      getItem: boom,
      key: boom,
      removeItem: boom,
      setItem: boom,
    },
    configurable: true,
  })
}

describe('IDE config storage', () => {
  afterEach(() => {
    uninstallMemoryStorage()
  })

  test('reads back what it wrote', () => {
    installMemoryStorage()
    writeStoredConfig(CONFIG)
    expect(readStoredConfig()).toEqual(CONFIG)
  })

  test('reads as absent before anything is written', () => {
    installMemoryStorage()
    expect(readStoredConfig()).toBeNull()
  })

  test('stores under a key of its own', () => {
    const storage = installMemoryStorage()
    writeStoredConfig(CONFIG)
    expect(storage.getItem(STORAGE_KEY)).toBe(JSON.stringify(CONFIG))
  })

  test('treats unparseable contents as absent', () => {
    const storage = installMemoryStorage()
    storage.setItem(STORAGE_KEY, 'not json')
    expect(readStoredConfig()).toBeNull()
  })

  test('treats valid JSON that is not an object as absent', () => {
    const storage = installMemoryStorage()
    // A config file truncated to a fragment still parses, so the shape check
    // is what keeps `{ ...defaults, ...stored }` from spreading a number.
    storage.setItem(STORAGE_KEY, '42')
    expect(readStoredConfig()).toBeNull()
  })

  test('passes a partial config through for the caller to merge', () => {
    const storage = installMemoryStorage()
    // Written before lastVersionAccessed existed. The caller fills the gap
    // from its defaults, so this must not be rejected outright.
    storage.setItem(STORAGE_KEY, JSON.stringify({ lastOpenedFilename: 'a.scm' }))
    expect(readStoredConfig()).toEqual({ lastOpenedFilename: 'a.scm' })
  })

  test('degrades to absent where there is no storage at all', () => {
    uninstallMemoryStorage()
    expect(readStoredConfig()).toBeNull()
    expect(() => {
      writeStoredConfig(CONFIG)
    }).not.toThrow()
  })

  test('degrades to absent where storage throws on access', () => {
    installHostileStorage()
    expect(readStoredConfig()).toBeNull()
    // A full quota throws on write; the IDE carries on without remembering.
    expect(() => {
      writeStoredConfig(CONFIG)
    }).not.toThrow()
  })
})

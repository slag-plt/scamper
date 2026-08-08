import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import {
  afterEach,
  beforeAll,
  beforeEach,
  describe,
  expect,
  test,
  vi,
} from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import {
  installMemoryStorage,
  uninstallMemoryStorage,
} from '../../stubs/memory-storage'
import { initialize } from '../../../src/scamper'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../../stubs/MockCodeMirrorEditor.vue'),
)

vi.mock(
  '../../../src/app/web/components/ResultsPane.vue',
  () => import('../../stubs/MockResultsPane.vue'),
)

await initialize()

const STORAGE_KEY = 'scamper.config'
const LEGACY_CONFIG_FILENAME = '.scamper.config'

// Patch notes are shown on the first visit to a new version (issue #306). These
// tests drive IdeApp against a seeded config to exercise the version gate.
describe('IDE patch-notes gate', () => {
  let fs: MockFileSystem

  // These tests need a real dotted-numeric app version, which `npm test` (and
  // validate-build) provide via npm_package_version. Fail fast with a clear
  // message rather than a confusing timeout if it's the 'unknown' fallback.
  beforeAll(() => {
    expect(APP_VERSION).toMatch(/^\d+(\.\d+)*$/)
  })

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    installMemoryStorage()
  })

  afterEach(() => {
    vi.restoreAllMocks()
    uninstallMemoryStorage()
    document.body.innerHTML = ''
  })

  function seedConfig(config: Record<string, unknown>): void {
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(config))
  }

  function readConfigVersion(): string | undefined {
    const raw = window.localStorage.getItem(STORAGE_KEY)
    return raw === null
      ? undefined
      : (JSON.parse(raw) as { lastVersionAccessed: string }).lastVersionAccessed
  }

  test('shows patch notes to a user upgrading from an older version', async () => {
    seedConfig({ lastOpenedFilename: null, lastVersionAccessed: '0.0.1' })

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'dialog', { name: /what's new/i })
      const dialog = getByRole(document.body, 'dialog', { name: /what's new/i })
      // The current release's notes are present.
      expect(dialog.textContent).toContain('Version')

      // The version is recorded as soon as the notes are shown -- so closing the
      // tab without clicking through would not re-show them.
      expect(readConfigVersion()).toBe(APP_VERSION)

      getByRole(dialog, 'button', { name: 'Got it' }).click()
      await flushPromises()

      // Dismissing closes it; it stays recorded, so it won't show again.
      expect(
        queryByRole(document.body, 'dialog', { name: /what's new/i }),
      ).toBeNull()
      expect(readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('treats a legacy config missing lastVersionAccessed as an upgrade', async () => {
    // Configs written before this feature have no lastVersionAccessed; the
    // default ('0.0.0') should make everything count as unseen.
    seedConfig({ lastOpenedFilename: null })

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'dialog', { name: /what's new/i })
      expect(readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('does not show patch notes to a brand-new user', async () => {
    // No config in storage, and none left in the file system either.
    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()
      expect(
        queryByRole(document.body, 'dialog', { name: /what's new/i }),
      ).toBeNull()
      // A fresh user is recorded as already caught up to the current version.
      expect(readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('adopts a config an older build left in the file system', async () => {
    // Before the config moved to localStorage it was a file. Existing users
    // must carry their seen version across, or every one of them would look
    // brand new and silently skip this release's notes.
    await fs.saveFile(
      LEGACY_CONFIG_FILENAME,
      JSON.stringify({ lastOpenedFilename: null, lastVersionAccessed: '0.0.1' }),
    )

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'dialog', { name: /what's new/i })
      expect(readConfigVersion()).toBe(APP_VERSION)
      // The leftover is cleared, so it can't be uploaded to a server-backed
      // file system later.
      expect(await fs.fileExists(LEGACY_CONFIG_FILENAME)).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('does not show patch notes to a user already on the current version', async () => {
    seedConfig({ lastOpenedFilename: null, lastVersionAccessed: APP_VERSION })

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()
      expect(
        queryByRole(document.body, 'dialog', { name: /what's new/i }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})

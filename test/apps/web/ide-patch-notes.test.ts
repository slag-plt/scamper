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
import { initialize } from '../../../src/scamper'

vi.mock('../../../src/app/web/lockfile', () => ({
  acquireLockFile: vi.fn(() => Promise.resolve(true)),
  releaseLockFile: vi.fn(() => Promise.resolve()),
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

const CONFIG_FILENAME = '.scamper.config'

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
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function readConfigVersion(): Promise<string> {
    const raw = await fs.loadFile(CONFIG_FILENAME)
    return (JSON.parse(raw) as { lastVersionAccessed: string })
      .lastVersionAccessed
  }

  test('shows patch notes to a user upgrading from an older version', async () => {
    await fs.saveFile(
      CONFIG_FILENAME,
      JSON.stringify({ lastOpenedFilename: null, lastVersionAccessed: '0.0.1' }),
    )

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'dialog', { name: /what's new/i })
      const dialog = getByRole(document.body, 'dialog', { name: /what's new/i })
      // The current release's notes are present.
      expect(dialog.textContent).toContain('Version')

      // The version is recorded as soon as the notes are shown -- so closing the
      // tab without clicking through would not re-show them.
      expect(await readConfigVersion()).toBe(APP_VERSION)

      getByRole(dialog, 'button', { name: 'Got it' }).click()
      await flushPromises()

      // Dismissing closes it; it stays recorded, so it won't show again.
      expect(
        queryByRole(document.body, 'dialog', { name: /what's new/i }),
      ).toBeNull()
      expect(await readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('treats a legacy config missing lastVersionAccessed as an upgrade', async () => {
    // Configs written before this feature have no lastVersionAccessed; the
    // default ('0.0.0') should make everything count as unseen.
    await fs.saveFile(
      CONFIG_FILENAME,
      JSON.stringify({ lastOpenedFilename: null }),
    )

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'dialog', { name: /what's new/i })
      expect(await readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('does not show patch notes to a brand-new user', async () => {
    // No config file exists.
    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()
      expect(
        queryByRole(document.body, 'dialog', { name: /what's new/i }),
      ).toBeNull()
      // A fresh user is recorded as already caught up to the current version.
      expect(await readConfigVersion()).toBe(APP_VERSION)
    } finally {
      wrapper.unmount()
    }
  })

  test('does not show patch notes to a user already on the current version', async () => {
    await fs.saveFile(
      CONFIG_FILENAME,
      JSON.stringify({
        lastOpenedFilename: null,
        lastVersionAccessed: APP_VERSION,
      }),
    )

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

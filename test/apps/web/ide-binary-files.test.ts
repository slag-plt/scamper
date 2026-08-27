import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
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

/** The first bytes of a real PNG: a signature that is not valid UTF-8. */
const PNG = new Uint8Array([
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0xff, 0xfe,
])

// Opening a binary file used to decode it as UTF-8 and let the three-second
// autosave write the result back, destroying it. #385 keeps binary out of the
// editor entirely: the file is still selected, shown, and downloadable.
describe('binary files in the IDE', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    try {
      localStorage.removeItem('scamper.config')
    } catch {
      /* no storage in this environment; nothing to clear */
    }
    // jsdom has no object URLs, and the image preview makes one.
    vi.stubGlobal('URL', {
      ...URL,
      createObjectURL: vi.fn(() => 'blob:mock'),
      revokeObjectURL: vi.fn(),
    })
  })

  afterEach(() => {
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
    document.body.innerHTML = ''
  })

  async function mountIde() {
    await fs.saveFile('one.scm', '(display 1)')
    await fs.saveBytes('cat.png', PNG)
    await fs.saveBytes('work.zip', new Uint8Array([0x50, 0x4b, 0x03, 0x04]))
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  async function openFile(name: string) {
    getByRole(document.body, 'button', { name: `Open ${name}` }).click()
    await flushPromises()
  }

  function selectedFile(): string | undefined {
    return document
      .querySelector('.file.selected .file-name')
      ?.textContent.trim()
  }

  test('a binary file that is not an image shows a notice', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('work.zip')

      const notice = getByRole(document.body, 'note')
      expect(notice.textContent).toContain('work.zip')
      expect(notice.textContent).toContain('binary file')
      expect(notice.querySelector('img')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('an image is shown as a picture', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('cat.png')

      const image = getByRole(document.body, 'img', { name: 'cat.png' })
      expect(image.getAttribute('src')).toBe('blob:mock')
      // Drawn from the file's bytes, which is the whole point: reading it as
      // text would have produced a broken image.
      expect(URL.createObjectURL).toHaveBeenCalled()
    } finally {
      wrapper.unmount()
    }
  })

  test('it is still the selected file, so the drawer actions apply to it', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('cat.png')
      expect(selectedFile()).toBe('cat.png')
    } finally {
      wrapper.unmount()
    }
  })

  test('autosave never writes it back, so the file survives being opened', async () => {
    vi.useFakeTimers()
    const wrapper = await mountIde()
    try {
      await openFile('cat.png')
      // Well past the three-second autosave interval, which is what used to
      // overwrite the file with a decoded copy of itself.
      await vi.advanceTimersByTimeAsync(10_000)
      await flushPromises()

      expect(await fs.loadBytes('cat.png')).toEqual(PNG)
    } finally {
      wrapper.unmount()
      vi.useRealTimers()
    }
  })

  test('Run is refused: a picture is not a program', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('cat.png')
      // The control is labelled "Autorun" while live evaluation is on and
      // "Run" while it is off (#378); either way it is the same button.
      const run = getByRole<HTMLButtonElement>(document.body, 'button', {
        name: /^(Run|Autorun)$/,
      })
      expect(run.disabled).toBe(true)
    } finally {
      wrapper.unmount()
    }
  })

  test('opening a text file afterwards puts the editor back', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('cat.png')
      expect(queryByRole(document.body, 'note')).not.toBeNull()

      await openFile('one.scm')
      expect(queryByRole(document.body, 'note')).toBeNull()
      expect(
        getByRole<HTMLTextAreaElement>(document.body, 'textbox', {
          name: 'Source code',
        }).value,
      ).toBe('(display 1)')
      // The picture's object URL is released rather than leaked.
      expect(URL.revokeObjectURL).toHaveBeenCalledWith('blob:mock')
    } finally {
      wrapper.unmount()
    }
  })

  test('creating one is declined, since there is nothing to put in it', async () => {
    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Create file' }).click()
      await flushPromises()
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'New file',
      })
      const { fireEvent } = await import('@testing-library/dom')
      fireEvent.input(getByRole(prompt, 'textbox'), {
        target: { value: 'new.png' },
      })
      getByRole(prompt, 'button', { name: 'OK' }).click()
      await flushPromises()

      const alert = await findByRole(document.body, 'dialog')
      expect(alert.textContent).toContain('cannot create')
      expect(await fs.fileExists('new.png')).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })
})

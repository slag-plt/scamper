import { flushPromises, mount } from "@vue/test-utils"
import { findByRole, fireEvent, getByRole, waitFor } from "@testing-library/dom"
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest"
import IdeApp from "../../src/web/components/IdeApp.vue"
import * as FS from "../../src/fs"
import { noLoadedFileText } from "../../src/web/codemirror"
import { mockEditorHandle } from "../stubs/mock-editor-handle"

vi.mock("../../src/fs/opfs", async () => {
  const { MockFileSystem } = await import("../stubs/mock-file-system")
  return { default: MockFileSystem }
})

vi.mock("../../src/web/lockfile", () => ({
  acquireLockFile: vi.fn(() => Promise.resolve(true)),
  releaseLockFile: vi.fn(() => Promise.resolve()),
}))

vi.mock(
  "../../src/web/components/CodeMirrorEditor.vue",
  () => import("../stubs/MockCodeMirrorEditor.vue"),
)

vi.mock(
  "../../src/web/components/ResultsPane.vue",
  () => import("../stubs/MockResultsPane.vue"),
)

const REAL_CONTENT = "(define x 42)"
const FILENAME = "regression.scm"

/**
 * Bug: on HMR, CodeMirrorEditor remounts to the unloaded placeholder while
 * IdeApp still has currentFile set and autosave running. saveCurrentFile()
 * then overwrites the real file on disk with the placeholder text.
 */
describe("IDE does not overwrite open file after editor reset (HMR split-brain)", () => {
  beforeEach(() => {
    vi.useFakeTimers()
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
    mockEditorHandle.adapter = null
    document.body.innerHTML = ""
  })

  test("autosave does not persist the unloaded placeholder over real file content", async () => {
    vi.spyOn(window, "prompt").mockReturnValue(FILENAME)

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await flushPromises()
      await findByRole(document.body, "button", { name: "Create file" })
      fireEvent.click(
        getByRole(document.body, "button", { name: "Create file" }),
      )
      await findByRole(document.body, "button", {
        name: `Open ${FILENAME}`,
      })

      const editor = getByRole(document.body, "textbox", {
        name: "Source code",
      })
      fireEvent.input(editor, { target: { value: REAL_CONTENT } })
      await waitFor(() => {
        expect(editor).toHaveValue(REAL_CONTENT)
      })

      // Persist real content so the on-disk precondition is unambiguous.
      await vi.advanceTimersByTimeAsync(3000)
      await flushPromises()

      const fs = FS.getFS()
      expect(await fs.loadFile(FILENAME)).toBe(REAL_CONTENT)

      // Simulate HMR: editor resets to unloaded doc, currentFile + autosave stay.
      const adapter = mockEditorHandle.adapter
      if (!adapter) {
        throw new Error("mock editor adapter not registered")
      }
      adapter.initializeDummyDoc()
      expect(adapter.getDoc()).toBe(noLoadedFileText)

      // Autosave tick while in split-brain state.
      await vi.advanceTimersByTimeAsync(3000)
      await flushPromises()

      expect(await fs.loadFile(FILENAME)).toBe(REAL_CONTENT)
      expect(await fs.loadFile(FILENAME)).not.toBe(noLoadedFileText)
    } finally {
      wrapper.unmount()
    }
  })
})

import { shallowRef } from "vue"
import { describe, expect, test } from "vitest"
import { Loc } from "../../src/lpm"
import type { CodeMirrorEditorAdapter } from "../../src/web/components/codemirror-editor-adapter"

function createTestEditorContext() {
  const holder = shallowRef<CodeMirrorEditorAdapter | null>(null)
  return {
    editor: () => {
      if (!holder.value) {
        throw new Error("Editor is not ready")
      }
      return holder.value
    },
    register(adapter: CodeMirrorEditorAdapter) {
      holder.value = adapter
    },
    unregister(adapter: CodeMirrorEditorAdapter) {
      if (holder.value === adapter) {
        holder.value = null
      }
    },
  }
}

describe("editor context", () => {
  test("editor accessor throws before register", () => {
    const { editor } = createTestEditorContext()
    expect(() => editor()).toThrow("Editor is not ready")
  })

  test("editor accessor delegates to the registered adapter", () => {
    const ctx = createTestEditorContext()
    const adapter = {
      getDoc: () => "hello",
      isLoaded: (): boolean => true,
      initializeDoc: (src: string) => {
        adapter.getDoc = () => src
      },
      initializeDummyDoc: () => {
        adapter.getDoc = () => ""
        adapter.isLoaded = () => false
      },
      getCursorLoc: () => new Loc(1, 2, 3),
      coordsAtPos: () => null,
      onViewChange: () => () => {
        /* noop */
      },
    } satisfies CodeMirrorEditorAdapter
    ctx.register(adapter)

    expect(ctx.editor().getDoc()).toBe("hello")
    ctx.editor().initializeDoc("world")
    expect(ctx.editor().getDoc()).toBe("world")
    ctx.editor().initializeDummyDoc()
    expect(ctx.editor().getDoc()).toBe("")
    expect(ctx.editor().getCursorLoc()).toEqual(new Loc(1, 2, 3))

    ctx.unregister(adapter)
    expect(() => ctx.editor()).toThrow("Editor is not ready")
  })

  test("unregister only clears matching adapter", () => {
    const ctx = createTestEditorContext()
    const first = {
      getDoc: () => "first",
      isLoaded: () => true,
      initializeDoc: (_src: string) => {
        void _src
      },
      initializeDummyDoc: () => {
        /* noop */
      },
      getCursorLoc: () => new Loc(0, 0, 0),
      coordsAtPos: () => null,
      onViewChange: () => () => {
        /* noop */
      },
    } satisfies CodeMirrorEditorAdapter
    const second = {
      getDoc: () => "second",
      isLoaded: () => true,
      initializeDoc: (_src: string) => {
        void _src
      },
      initializeDummyDoc: () => {
        /* noop */
      },
      getCursorLoc: () => new Loc(1, 1, 1),
      coordsAtPos: () => null,
      onViewChange: () => () => {
        /* noop */
      },
    } satisfies CodeMirrorEditorAdapter

    ctx.register(first)
    ctx.register(second)
    ctx.unregister(first)

    expect(ctx.editor().getDoc()).toBe("second")
  })
})

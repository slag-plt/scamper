import {
  inject,
  type InjectionKey,
  provide,
  shallowRef,
  type ShallowRef,
} from "vue"
import type { CodeMirrorEditorAdapter } from "./codemirror-editor-adapter"

const EditorHolderKey: InjectionKey<
  ShallowRef<CodeMirrorEditorAdapter | null>
> = Symbol("EditorHolder")

export function provideEditor(): EditorAccessor {
  const holder = shallowRef<CodeMirrorEditorAdapter | null>(null)
  provide(EditorHolderKey, holder)
  return makeAccessor(holder)
}

export function useEditorRegistration() {
  const holder = injectHolder()
  return {
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

export function useEditor(): EditorAccessor {
  return makeAccessor(injectHolder())
}

export type EditorAccessor = () => CodeMirrorEditorAdapter

function makeAccessor(
  holder: ShallowRef<CodeMirrorEditorAdapter | null>,
): EditorAccessor {
  return () => {
    if (!holder.value) {
      throw new Error("Editor is not ready")
    }
    return holder.value
  }
}

function injectHolder() {
  const holder = inject(EditorHolderKey)
  if (!holder) {
    throw new Error(
      "Editor context missing: call provideEditor() in an ancestor",
    )
  }
  return holder
}

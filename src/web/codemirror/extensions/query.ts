import {
  Decoration,
  DecorationSet,
  ViewPlugin,
  ViewUpdate,
} from "@codemirror/view"

export const QueryExtension = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet

    constructor() {
      this.decorations = Decoration.none
    }

    update(update: ViewUpdate) {
      if (update.docChanged) {
        this.decorations = Decoration.none
      }
    }
  },
  {
    decorations: (v) => v.decorations,
  },
)

import type { Text } from '@codemirror/state'
import { StateEffect, StateField, Transaction } from '@codemirror/state'
import {
  Decoration,
  DecorationSet,
  EditorView,
  WidgetType,
} from '@codemirror/view'
import { toString } from '../../../../lpm'
import type { ExampleOutcome } from '../../../../scheme/examples'

/**
 * The pass/fail marks drawn at the end of each `;;; @example ...` line
 * (issue #374).
 *
 * Decorations, never diagnostics: an example the code disagrees with is
 * something to notice, not a squiggle under otherwise valid code.
 *
 * Unlike the query extension, which reads the Scamper singleton, this is told
 * its outcomes -- see {@link setExampleDecorations} -- so it can be tested
 * with nothing but an editor.
 */

const marks: Record<ExampleOutcome['status'], string> = {
  pass: '✅',
  fail: '❌',
  error: '⚠️',
  timeout: '⏱️',
}

/** The tooltip on a mark: what the example did, in one line. */
export function exampleMarkTitle(outcome: ExampleOutcome): string {
  switch (outcome.status) {
    case 'pass':
      return 'This example agrees with your code.'
    case 'fail':
      return `Expected ${toString(outcome.expected)}, but got ${toString(outcome.actual)}.`
    case 'timeout':
      return 'This example took too long to finish, so Scamper stopped it.'
    case 'error':
      return outcome.message ?? 'This example could not be run.'
  }
}

class ExampleMarkWidget extends WidgetType {
  constructor(
    readonly status: ExampleOutcome['status'],
    readonly title: string,
  ) {
    super()
  }

  toDOM(): HTMLElement {
    const el = document.createElement('span')
    el.className = `cm-example-mark cm-example-${this.status}`
    el.textContent = marks[this.status]
    el.title = this.title
    return el
  }

  eq(other: WidgetType): boolean {
    return (
      other instanceof ExampleMarkWidget &&
      other.status === this.status &&
      other.title === this.title
    )
  }
}

/**
 * Builds one mark per outcome, at the end of the line its example is on.
 *
 * Outcomes arrive after the run that produced them, by which point the
 * document may have lost lines, so a line past the end is dropped rather than
 * asked for.
 */
export function buildExampleMarks(
  outcomes: readonly ExampleOutcome[],
  doc: Text,
): DecorationSet {
  const widgets = outcomes
    .filter((o) => o.range.begin.line >= 1 && o.range.begin.line <= doc.lines)
    .map((o) =>
      Decoration.widget({
        widget: new ExampleMarkWidget(o.status, exampleMarkTitle(o)),
        side: 1,
      }).range(doc.line(o.range.begin.line).to),
    )
  return Decoration.set(widgets, true)
}

export const exampleDecorationsSet = StateEffect.define<DecorationSet>()

/** Replaces the marks with those for `outcomes`, without touching undo history. */
export function setExampleDecorations(
  view: EditorView,
  outcomes: readonly ExampleOutcome[],
) {
  view.dispatch({
    effects: exampleDecorationsSet.of(
      buildExampleMarks(outcomes, view.state.doc),
    ),
    annotations: Transaction.addToHistory.of(false),
  })
}

export const ExampleExtension = StateField.define<DecorationSet>({
  create() {
    return Decoration.none
  },
  update(deco, tr) {
    for (const e of tr.effects) {
      if (e.is(exampleDecorationsSet)) return e.value
    }
    // An edit moves the existing marks along rather than rebuilding them: they
    // are about the code as it was, and the next sweep replaces them.
    return tr.docChanged ? deco.map(tr.changes) : deco
  },
  provide: (f) => EditorView.decorations.from(f),
})

/** Keeps a mark clear of the comment text it follows. */
export const ExampleMarkTheme = EditorView.baseTheme({
  '.cm-example-mark': {
    marginLeft: '0.5ch',
    cursor: 'default',
  },
})

import { mount } from '@vue/test-utils'
import { beforeEach, describe, expect, test } from 'vitest'
import NotebookCell from '../../src/app/web/components/NotebookCell.vue'
import type { NotebookCell as Cell } from '../../src/app/web/composables/use-notebook'
import { EMBED_CLASS, runEmbeds } from '../../src/app/web/embed/embed'
import type { Value } from '../../src/lpm'
import { initialize } from '../../src/scamper'
import { query } from '../dom'
import '../../src/app/web/renderers'

await initialize()

// https://github.com/slag-plt/scamper/issues/635
//
// #612 made a void draw as an element that takes up no space. A void is still a
// *send*, though, so both surfaces that wrap a value in a container of their own
// went on drawing the container: a statement such as `(vector-set! v 0 5)` left
// a padded, completely empty output region where the word `void` used to be.
//
// The containers are what this pins, not their spacing: a void-only statement
// draws no container at all, so there is nothing for a stylesheet to pad.

/** A statement whose only result is void. */
const VOID_STATEMENT = '(vector-set! (vector 1 2) 0 5)'

/** A cell holding `text`, as the notebook hands one to NotebookCell. */
function cellOf(text: string): Cell {
  return {
    id: 1,
    kind: 'code',
    text,
    from: 0,
    to: text.length,
    stmtFrom: 0,
    stmtTo: text.length,
  }
}

/** Mounts one code cell showing `output` and returns its rendered markup. */
function cellWithOutput(output: Value[], text = VOID_STATEMENT) {
  return mount(NotebookCell, {
    props: { cell: cellOf(text), output, diagnostics: [], index: 0 },
  })
}

describe('a notebook cell', () => {
  test('draws no output region for a statement producing only void', () => {
    const cell = cellWithOutput([undefined])
    try {
      expect(cell.find('.cell-output').exists()).toBe(false)
    } finally {
      cell.unmount()
    }
  })

  // The first control: a statement producing a value keeps its box.
  test('still draws one for a statement producing a value', () => {
    const cell = cellWithOutput([5], '(+ 2 3)')
    try {
      expect(cell.find('.cell-output').exists()).toBe(true)
      expect(cell.find('.cell-output').text()).toContain('5')
    } finally {
      cell.unmount()
    }
  })

  // The second control: a statement producing nothing at all is unchanged.
  test('draws none for a statement producing nothing at all', () => {
    const cell = cellWithOutput([], '(define x 5)')
    try {
      expect(cell.find('.cell-output').exists()).toBe(false)
    } finally {
      cell.unmount()
    }
  })

  // A void alongside something visible keeps its place in the output, which is
  // what #612 gave it a zero-sized element for.
  test('a void beside a value keeps its place inside the region', () => {
    const cell = cellWithOutput([undefined, 5], '(begin (void) (display 5))')
    try {
      expect(cell.find('.cell-output').exists()).toBe(true)
      expect(cell.findAll('.cell-value')).toHaveLength(2)
    } finally {
      cell.unmount()
    }
  })
})

describe('a reading widget', () => {
  /** Builds a widget from `code` and runs it. */
  async function widget(code: string): Promise<HTMLElement> {
    document.body.innerHTML = `<div class="${EMBED_CLASS}">${code}</div>`
    const el = query(document.body, `.${EMBED_CLASS}`)
    await runEmbeds()
    return el
  }

  beforeEach(() => {
    document.body.innerHTML = ''
  })

  test('wraps nothing around a statement producing only void', async () => {
    const el = await widget(VOID_STATEMENT)
    // The statement is still in the transcript; only its empty output block is
    // gone.
    expect(el.querySelectorAll('.scamper-transcript-source')).toHaveLength(1)
    expect(el.querySelector('.scamper-output')).toBeNull()
  })

  // The same two controls, on the other surface.
  test('still wraps a statement producing a value', async () => {
    const el = await widget('(+ 2 3)')
    expect(query(el, '.scamper-output').textContent).toContain('5')
  })

  test('wraps nothing around a statement producing nothing at all', async () => {
    const el = await widget('(define x 5)')
    expect(el.querySelectorAll('.scamper-transcript-source')).toHaveLength(1)
    expect(el.querySelector('.scamper-output')).toBeNull()
  })
})

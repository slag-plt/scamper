import { describe, expect, test } from 'vitest'
import { transcriptText } from '../../../src/app/web/repl-transcript'
import { audio_sampleNode } from '../../../src/js/audio'
import { drawing_rectangle } from '../../../src/js/image/drawing'
import { ScamperError } from '../../../src/lpm/error'

// What the Copy button hands to the clipboard (#459): the transcript as plain
// text, shaped like what a drag across the entries copies. Not identical to it
// -- see the drawing case below, which a drag gets nothing at all for.
// Nothing here mounts anything -- the entries are plain objects, since that is
// all the function asks for.
describe('the REPL transcript as text', () => {
  test('what was typed and what it printed alternate, in order', () => {
    expect(
      transcriptText([
        { source: '(+ 1 2)', values: [3] },
        { source: '(define x 5)', values: [] },
        { source: '(list x x)', values: ['first', 'second'] },
      ]),
    ).toBe('(+ 1 2)\n3\n(define x 5)\n(list x x)\n"first"\n"second"')
  })

  test('an entry that printed nothing contributes only what was typed', () => {
    expect(transcriptText([{ source: '(define x 5)', values: [] }])).toBe(
      '(define x 5)',
    )
  })

  test('output from something the file left running contributes only itself', () => {
    // use-repl gives such output an entry of its own with no source, and the
    // window shows no prompt above it either.
    expect(transcriptText([{ source: '', values: ['tick'] }])).toBe('"tick"')
  })

  test('an error is copied as it was read', () => {
    expect(
      transcriptText([
        {
          source: '(/ 1 0)',
          values: [new ScamperError('Runtime', 'division by zero')],
        },
      ]),
    ).toBe('(/ 1 0)\nRuntime error: division by zero')
  })

  test('a drawing is copied as the expression that makes it', () => {
    const text = transcriptText([
      {
        source: '(rectangle 20 10 "solid" "red")',
        values: [drawing_rectangle(20, 10, 'solid', 'red')],
      },
    ])
    expect(text.split('\n')[1]).toBe('(rectangle 20 10 "solid" (rgba 255 0 0 255))')
  })

  test('a value far too large to copy is cut short', () => {
    // A sound holds every one of its samples and renders as all of them, so a
    // real clip is tens of megabytes of string built on a button click.
    const sound = audio_sampleNode(new Array<number>(2000).fill(0.5))
    const printed = transcriptText([
      { source: '(tone 440)', values: [sound] },
    ]).split('\n')[1]
    expect(printed.length).toBeLessThan(5000)
    expect(printed.endsWith('...')).toBe(true)
  })

  test('nothing run yet is nothing to copy', () => {
    expect(transcriptText([])).toBe('')
  })
})

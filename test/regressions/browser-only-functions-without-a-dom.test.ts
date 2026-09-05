// @vitest-environment node
import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness'

// #516: a function that genuinely needs a browser -- one that makes an
// element, plays a sound, or drives an animation frame -- cannot answer the
// way #508's predicates can: there is no canvas to hand back where there is no
// page. What it can do is say so. Before, the first DOM access died as a raw
// `ReferenceError: document is not defined`, naming an identifier the student
// never wrote and pointing at a line of the standard library rather than at
// the call. Gradescope autograders run through the CLI, so this is the message
// an instructor's harness got.
//
// N.B., this file overrides the suite's jsdom environment on purpose. Under
// jsdom `document` is defined and these functions work, so a test there could
// not see the bug at all. Node is the CLI's own condition.
const MESSAGE =
  'This function needs a browser, so it does not work on the command line'

/** `fn`'s report, with its range dropped -- see RunOptions.stripRanges. */
async function report(program: string): Promise<string> {
  const out = await runProgram(program, { stripRanges: true })
  return out.join('\n')
}

describe('#516: browser-only functions where there is no DOM', () => {
  test('the browser globals really are absent here', () => {
    expect(typeof document).toBe('undefined')
    expect(typeof window).toBe('undefined')
  })

  // `ignore` is the sharpest case: it lives in the prelude, so a student
  // reaches it without importing anything.
  test('ignore says what is missing instead of naming `document`', async () => {
    expect(await report('(ignore 5)')).toBe(`Runtime error: (ignore) ${MESSAGE}`)
  })

  // Each is named by `applyFn`, which labels a native's ScamperError with the
  // function the student called -- which is why the guard takes no argument.
  //
  // N.B., the last two check *before* throwing their SuspendSignal, and that is
  // what these two rows pin. Move either guard inside the action and the
  // message and range are unchanged, but the `(image-load)` label is gone: an
  // error raised in the action is reported by the scheduler, which has no frame
  // to name it from.
  test.each([
    ['make-canvas', '(import canvas)\n(make-canvas 100 100)'],
    ['tag', '(import html)\n(tag "div")'],
    ['audio-context', '(import audio)\n(audio-context 44100)'],
    ['drawing->canvas', '(import image)\n(drawing->canvas (circle 10 "solid" "red"))'],
    ['title', '(import lab)\n(title "A Lab")'],
    ['image-load', '(import image)\n(image-load "cat.png")'],
    ['with-image-from-url',
      '(import image)\n(with-image-from-url "http://e.com/a.png" (lambda (c) c))'],
  ])('%s says what is missing instead of naming `document`', async (name, program) => {
    expect(await report(program)).toBe(`Runtime error: (${name}) ${MESSAGE}`)
  })

  // The whole point: no raw Javascript error reaches the student or the
  // instructor.
  test('none of them report a Javascript ReferenceError', async () => {
    const programs = [
      '(ignore 5)',
      '(import canvas)\n(animate-with (lambda (t) #f))',
      '(import html)\n(button "hi" (lambda () 1))',
      '(import image)\n(image-load "cat.png")',
      '(import reactive)\n(reactive-canvas 10 10 0 (lambda (s c) c) (lambda (m s) s))',
    ]
    for (const program of programs) {
      const line = await report(program)
      expect(line).not.toContain('ReferenceError')
      expect(line).toContain(MESSAGE)
    }
  })
})

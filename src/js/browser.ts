import * as L from '../lpm'

/**
 * Guards a library function that genuinely cannot work outside a browser --
 * one that makes an element, plays a sound, or drives an animation frame.
 *
 * Without it the first DOM access dies as a raw `ReferenceError: document is
 * not defined`, naming an identifier the student never wrote and pointing at a
 * line of the standard library. This reports a Scamper error instead, which
 * `applyFn` labels with the function's name and the student's call site (#516).
 *
 * `document` stands for the browser as a whole: what is missing on the command
 * line is the page, not one global, so every call site asks the same question.
 * In a browser (and under jsdom) it is always there, so nothing changes there.
 *
 * N.B., a helper rather than #508's inline `typeof` clause: that was one
 * `instanceof` operand per predicate, whereas this is one sentence repeated
 * across six libraries. It lives here rather than in any library's `index.ts`
 * because src/js/index.ts flattens every one of those modules' exports into the
 * `js-var` map -- an export there would become a Scamper binding.
 */
export function requireBrowser(): void {
  if (typeof document === 'undefined') {
    throw new L.ScamperError(
      'Runtime',
      'This function needs a browser, so it does not work on the command line',
    )
  }
}

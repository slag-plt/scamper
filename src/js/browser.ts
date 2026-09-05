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
 * across seven libraries. `music` keeps its own wording -- `requireWaf` names
 * the audio it cannot reach, which is more specific than the missing page.
 *
 * It lives in its own module rather than anywhere src/js/index.ts flattens into
 * the `js-var` map -- every library module *and* the files they re-export, such
 * as image/drawing.ts and prelude/files.ts -- since an export reached from
 * there would become a Scamper binding named after it.
 */
export function requireBrowser(): void {
  if (typeof document === 'undefined') {
    throw new L.ScamperError(
      'Runtime',
      'This function needs a browser, so it does not work on the command line',
    )
  }
}

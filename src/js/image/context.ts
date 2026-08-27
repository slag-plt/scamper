import * as L from '../../lpm'

/**
 * A canvas's 2d drawing context.
 *
 * `getContext` is typed as nullable because a browser can refuse -- a canvas
 * already bound to a different context type, or one too large to back. Every
 * caller here has either just made the canvas or been handed one by the
 * library, so a refusal is an error rather than a case to handle, and saying
 * so beats the TypeError it used to raise several calls deeper.
 */
export function context2d(canvas: HTMLCanvasElement): CanvasRenderingContext2D {
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError(
      'Runtime',
      'This canvas does not have a 2d drawing context',
    )
  }
  return ctx
}

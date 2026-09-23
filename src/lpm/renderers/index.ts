import * as LPM from '../lang.js'
import { isVoid } from '../util.js'

export type TypeTest    = (v: LPM.Value) => boolean

/**
 * Whether rendering `v` puts nothing at all on the page.
 *
 * Void is the one value it is true of: both web renderers draw it as an element
 * taking up no space (#612). A surface that wraps a rendering in a container of
 * its own asks this first, so a statement producing only void leaves no empty
 * padded box behind (#635).
 */
export function drawsNothing (v: LPM.Value): boolean {
  return isVoid(v)
}

/**
 * Renders a value. `col` is the column the rendering begins at, for the
 * renderers that lay their output out over several lines and must indent the
 * continuation ones to match. Only the text renderer supplies it -- the DOM
 * backends position their output themselves -- so it is optional, and a
 * renderer with no use for it simply takes one argument.
 */
export type RenderFn<T> = (v: LPM.Value, col?: number) => T

/**
 * A `Renderer<T>` pretty-prints values to type `T`. It also supports custom
 * renderers for specific types of interest.
 */
export abstract class Renderer<T> {
  private customRenderers: [TypeTest, RenderFn<T>][]

  constructor () {
    this.customRenderers = []
  }

  public registerCustomRenderer (typeTest: TypeTest, renderer: RenderFn<T>): void {
    this.customRenderers.push([typeTest, renderer])
  }

  public getCustomRendererFor (v: LPM.Value): RenderFn<T> | null {
    for (const [test, renderer] of this.customRenderers) {
      if (test(v)) {
        return renderer
      }
    }
    return null
  }

  public abstract render (v: LPM.Value): T
}
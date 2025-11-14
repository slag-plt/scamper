import * as LPM from '../lang.js'

export type TypeTest    = (v: LPM.Value) => boolean
export type RenderFn<T> = (v: LPM.Value) => T

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
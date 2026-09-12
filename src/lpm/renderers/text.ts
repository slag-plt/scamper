import { Value } from '../lang.js'
import * as R from './index.js'
import * as U from '../util.js'

class Renderer extends R.Renderer<string> {
  /**
   * @param col the column this rendering starts at, so a custom renderer that
   *   breaks lines can indent the continuation ones to sit under it. Callers
   *   writing a prefix first -- the trace's "--> ", say -- pass its width.
   */
  public render(v: Value, col = 0): string {
    const customRenderer = this.getCustomRendererFor(v)
    // N.B., the recursion goes back through `render`, not `U.toString`, so that
    // a custom renderer also claims a value nested inside an aggregate.
    return customRenderer
      ? customRenderer(v, col)
      : U.valueToString(v, (x) => this.render(x))
  }
}

const TextRenderer = new Renderer()
export default TextRenderer

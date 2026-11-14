import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'
import TextRenderer from '../../src/lpm/renderers/text'
import type { Value } from '../../src/lpm'
import type { ErrorChannel, OutputChannel } from '../../src/lpm/output/channel'
import { VueDisplay } from '../../src/lpm/output/vue'
import { NotebookDisplay } from '../../src/app/web/notebook-display'
import { captionOf, splitIntoCells } from '../../src/app/web/notebook-cells'

// https://github.com/slag-plt/scamper/issues/750
//
// A side effect appeared to reach back and change output a statement that had
// already run had printed: the program below showed `(vector 0 1 0 0 0)` under
// *both* occurrences of `vec`, as if `vector-set!` had run before the first
// one.
//
// The two channels the IDE renders through -- the output pane (VueDisplay) and
// the notebook (NotebookDisplay) -- keep the Value they are sent and let Vue
// render it later. A vector is a JavaScript array, so what they kept was the
// live one, and by the time anything drew it `vector-set!` had mutated it. The
// channels that render on the way past, the CLI's text channel and the embed
// widget's HTML one, were never wrong -- which is the shape of the defect: a
// channel that defers rendering must snapshot what it was sent.
//
// Deliberately not a browser test. Nothing here is about drawing; it is about
// what the channel is holding once the run is over, which is what a later
// render would draw.

const SRC = `(define vec (make-vector 5 0))
vec
(vector-set! vec 1 1)
vec`

/** What a deferred render would draw for each value the run sent. */
const asDrawnLater = (vs: Value[]): string[] =>
  vs.map((v) => TextRenderer.render(v))

/** Runs the program above, sending its output to `out`. */
async function run(out: OutputChannel & ErrorChannel): Promise<void> {
  const { prog } = await Scheme.compile(SRC)
  if (prog === undefined) throw new Error('the program does not compile')
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
    src: SRC,
  })
}

describe('#750: a side effect does not change output already sent', () => {
  test('the output pane holds each statement\'s vector as it was', async () => {
    const sent: Value[] = []
    const display = new VueDisplay({
      StartSectionCallback: () => undefined,
      EndSectionCallback: () => undefined,
      SendCallback: (v) => sent.push(v),
      SourceCallback: () => undefined,
    })
    await run(display)
    expect(asDrawnLater(sent)).toEqual([
      '(vector 0 0 0 0 0)',
      'void',
      '(vector 0 1 0 0 0)',
    ])
  })

  test('the notebook files each cell\'s vector as it was', async () => {
    const cells = splitIntoCells(SRC)
    if (cells === null) throw new Error('the program does not split into cells')
    const display = new NotebookDisplay()
    display.setSlots(
      cells.map((cell) => ({
        from: cell.from,
        to: cell.to,
        caption: cell.kind === 'code' ? captionOf(cell, SRC) : '',
      })),
    )
    await run(display)
    expect(cells.map((_, i) => asDrawnLater(display.outputOf(i)))).toEqual([
      [],
      ['(vector 0 0 0 0 0)'],
      ['void'],
      ['(vector 0 1 0 0 0)'],
    ])
  })
})

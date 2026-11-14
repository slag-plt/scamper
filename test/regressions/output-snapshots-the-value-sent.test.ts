import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme'
import Scamper, { initialize } from '../../src/scamper'
import { Fiber } from '../../src/lpm/fiber'
import { Loc } from '../../src/lpm/range'
import { runFiberOnScheduler } from '../../src/lpm/run'
import TextRenderer from '../../src/lpm/renderers/text'
import type { Value } from '../../src/lpm'
import * as L from '../../src/lpm/lang'
import type { ErrorChannel, OutputChannel } from '../../src/lpm/output/channel'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
import { TRACE_MARKER } from '../../src/lpm/trace'
import { listToVector, mkCons, snapshot, vectorToList } from '../../src/lpm/util'
import { VueDisplay } from '../../src/lpm/output/vue'
import { NotebookDisplay } from '../../src/app/web/notebook-display'
import { captionOf, splitIntoCells } from '../../src/app/web/notebook-cells'
import { useRepl } from '../../src/app/web/composables/use-repl'

await initialize()

// https://github.com/slag-plt/scamper/issues/750
//
// A side effect appeared to reach back and change output a statement that had
// already run had printed: the program below showed `(vector 0 1 0 0 0)` under
// *both* occurrences of `vec`, as if `vector-set!` had run before the first
// one.
//
// The four channels that defer rendering -- the output pane (VueDisplay), the
// notebook (NotebookDisplay), the REPL transcript and the trace collector --
// kept the Value they were sent and let a framework draw it later. A vector is
// a JavaScript array, so what they kept was the live one, and by the time
// anything drew it `vector-set!` had mutated it. The channels
// that render on the way past, the CLI's text channel and the embed widget's
// HTML one, were never wrong -- which is the shape of the defect: a channel
// that defers rendering must snapshot what it was sent.
//
// Deliberately not a browser test. Nothing here is about drawing; it is about
// what the channel is holding once the run is over, which is what a later
// render would draw.

const MUTATED_VECTOR = `(define vec (make-vector 5 0))
vec
(vector-set! vec 1 1)
vec`

/** What a deferred render would draw for each value the run sent. */
const asDrawnLater = (vs: Value[]): string[] =>
  vs.map((v) => TextRenderer.render(v))

/** Runs `src`, sending its output to `out`. */
async function run(src: string, out: OutputChannel & ErrorChannel): Promise<void> {
  const { prog } = await Scheme.compile(src)
  if (prog === undefined) throw new Error('the program does not compile')
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
    src,
  })
}

/** Everything `src` sends to the output pane, in order. */
async function sentToOutputPane(src: string): Promise<Value[]> {
  const sent: Value[] = []
  const display = new VueDisplay({
    StartSectionCallback: () => undefined,
    EndSectionCallback: () => undefined,
    SendCallback: (v) => sent.push(v),
    SourceCallback: () => undefined,
  })
  await run(src, display)
  return sent
}

describe('#750: a side effect does not change output already sent', () => {
  test("the output pane holds each statement's vector as it was", async () => {
    expect(asDrawnLater(await sentToOutputPane(MUTATED_VECTOR))).toEqual([
      '(vector 0 0 0 0 0)',
      'void',
      '(vector 0 1 0 0 0)',
    ])
  })

  test("the notebook files each cell's vector as it was", async () => {
    const cells = splitIntoCells(MUTATED_VECTOR)
    if (cells === null) throw new Error('the program does not split into cells')
    const display = new NotebookDisplay()
    display.setSlots(
      cells.map((cell) => ({
        from: cell.from,
        to: cell.to,
        caption: cell.kind === 'code' ? captionOf(cell, MUTATED_VECTOR) : '',
      })),
    )
    await run(MUTATED_VECTOR, display)
    expect(cells.map((_, i) => asDrawnLater(display.outputOf(i)))).toEqual([
      [],
      ['(vector 0 0 0 0 0)'],
      ['void'],
      ['(vector 0 1 0 0 0)'],
    ])
  })

  test('a REPL entry keeps what it printed when a later entry mutates it', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '')
    try {
      await repl.submit('(define vec (make-vector 5 0))')
      await repl.submit('vec')
      await repl.submit('(vector-set! vec 1 1)')
      await repl.submit('vec')
      // An entry stays on screen while later ones run, so this is the same
      // defect with the statements separated in time rather than in a file.
      expect(repl.entries.value.map((e) => asDrawnLater(e.values))).toEqual([
        [],
        ['(vector 0 0 0 0 0)'],
        ['void'],
        ['(vector 0 1 0 0 0)'],
      ])
    } finally {
      repl.close()
    }
  })

  test('a trace keeps the reductions of the statement it stepped', async () => {
    // The statement after the traced one still runs -- stepping one statement
    // runs the whole program -- so without the snapshot the collected steps
    // would read `0 1 0 0 0`, having been mutated after they were collected.
    const src = `(define vec (make-vector 5 0))
(define f (lambda (x) x))
(f vec)
(vector-set! vec 1 1)`
    const result = await Scamper.getInstance().traceStatement({
      src,
      cursorLoc: new Loc(1, 1, src.indexOf('(f vec)') + 2),
      err: new SimpleErrorChannel(),
      maxSteps: 1000,
    })
    if (result === null) throw new Error('no statement under the cursor')
    expect(result.source).toBe('(f vec)')
    // Each step but the first carries the trace's `--> ` marker.
    const steps = asDrawnLater(result.steps).map((s) =>
      s.startsWith(TRACE_MARKER) ? s.slice(TRACE_MARKER.length) : s,
    )
    expect(steps).not.toContain('(vector 0 1 0 0 0)')
    expect(steps.at(-1)).toBe('(vector 0 0 0 0 0)')
  })
})

// The one place the snapshot deliberately does nothing. A canvas or an HTML tag
// is a DOM element, and the element the output pane is showing is the element
// the program holds -- so drawing on a canvas after displaying it still updates
// what is on screen, as it always has. Data freezes; the DOM stays live.
//
// Drawing itself needs a real Canvas2D, which only the browser suite has (see
// test/libs/canvas.browser.test.ts), so what is pinned here is the property
// that makes drawing show up: the element is never copied. `tag-set-children!`
// is the same exemption and does run under jsdom, so it carries the end-to-end
// half of the case.
describe('#750: a displayed DOM element stays live', () => {
  test('a canvas is handed on by reference, not copied', () => {
    const canvas = document.createElement('canvas')
    expect(snapshot(canvas)).toBe(canvas)
    // For contrast: the aggregate beside it is copied, which is the fix.
    const vec: Value[] = [0, 0]
    expect(snapshot(vec)).not.toBe(vec)
  })

  test('a tag mutated after it is shown still shows the change', async () => {
    const sent = await sentToOutputPane(`(import canvas)
(import html)
(define c (make-canvas 10 10))
c
(define elt (tag "div"))
elt
(tag-set-children! elt (tag "p" "hi"))`)
    expect(sent[0]).toBeInstanceOf(HTMLCanvasElement)
    expect((sent[1] as HTMLElement).outerHTML).toBe('<div><p>hi</p></div>')
  })
})

/** A list of `n` vectors, each holding the list itself. */
function listOfBackPointers(n: number): L.List {
  const vecs: Value[] = []
  for (let i = 0; i < n; i++) {
    vecs.push([0])
  }
  const lst = vectorToList(vecs)
  for (const v of vecs) {
    ;(v as Value[])[0] = lst
  }
  return lst
}

// The properties the copy has to have, asserted directly rather than through a
// channel. The list branch is the one that carries every program's output, so
// a mistake in it would corrupt the ordinary case rather than an exotic one.
describe('#750: what snapshot guarantees', () => {
  test('a long list costs a walk, not a stack frame per cell', () => {
    // The renderers walk a list rather than recurring through it, so the copy
    // has to as well: a cell-deep recursion overflows here long before the
    // renderer that drew the list would.
    const long = vectorToList(Array.from({ length: 200_000 }, (_, i) => i))
    const copy = snapshot(long)
    expect(listToVector(copy as L.List)).toHaveLength(200_000)
    expect(listToVector(copy as L.List).at(-1)).toBe(199_999)
  })

  test('a cycle back into a list terminates, and stays linear', () => {
    // Registering the whole spine before filling any head is what makes this
    // cheap. Walking the list afresh for each back-pointer is quadratic, and
    // past a few thousand elements it overflowed the stack -- out of `send`,
    // inside the scheduler's step, where a non-ScamperError kills the run.
    for (const n of [1000, 5000, 20_000]) {
      const copy = snapshot(listOfBackPointers(n))
      const elems = listToVector(copy as L.List)
      expect(elems).toHaveLength(n)
      // Each copied vector points at the copied list, not the original.
      expect((elems[0] as Value[])[0]).toBe(copy)
    }
  })

  test('a list holding itself terminates', () => {
    const vec: Value[] = [0]
    const lst = vectorToList([vec])
    vec[0] = lst
    const copy = snapshot(lst) as L.Cons
    expect((copy.head as Value[])[0]).toBe(copy)
  })

  test('structure shared between two values stays shared', () => {
    // Two lists over one tail copy to two lists over *one* copied tail, not
    // two. The same already held for vectors, structs and maps.
    const tail = vectorToList([1, 2])
    const shared: Value[] = [mkCons(0, tail), mkCons(9, tail)]
    const copy = snapshot(shared) as Value[]
    expect((copy[0] as L.Cons).tail).toBe((copy[1] as L.Cons).tail)
    expect((copy[0] as L.Cons).tail).not.toBe(tail)

    const vec: Value[] = [1]
    const twice = snapshot([vec, vec]) as Value[]
    expect(twice[0]).toBe(twice[1])
    expect(twice[0]).not.toBe(vec)
  })

  test("a struct's hidden fields are shared, not copied", () => {
    // Deliberate. `##run##` on a reactive file chooser is a live RunHandle,
    // and a copy of it would be a handle onto nothing -- its renderer reads
    // the field. Only what a student can see is snapshotted.
    const handle = { live: true }
    const struct: L.Struct = {
      [L.scamperTag]: 'struct',
      [L.structKind]: 'widget',
      value: [1, 2],
      '##run##': handle,
    }
    const copy = snapshot(struct) as L.Struct
    expect(copy['##run##']).toBe(handle)
    expect(copy.value).not.toBe(struct.value)
    expect(copy.value).toEqual([1, 2])
    expect(copy[L.structKind]).toBe('widget')
  })
})

import { beforeEach, describe, expect, test } from 'vitest'
import { EMBED_CLASS, readSpec, runEmbeds } from '../../../src/app/web/embed/embed'
import { initialize } from '../../../src/scamper'

await initialize()

// The reading widget (#375): a block of Scamper code in a page, replaced by a
// transcript of that code interleaved with what it produced. Several can sit on
// one page, each its own program, all run by the one Scamper instance.

/** Builds a widget from markup and returns it. */
function widget(html: string): HTMLElement {
  document.body.innerHTML = html
  return document.body.querySelector<HTMLElement>(`.${EMBED_CLASS}`)!
}

/** The transcript's rendered text, whitespace squashed so assertions read. */
function transcript(el: HTMLElement): string {
  return (el.textContent ?? '').replace(/\s+/g, ' ').trim()
}

/** The source captions a widget rendered, in order. */
function captions(el: HTMLElement): string[] {
  return [...el.querySelectorAll('.scamper-transcript-source')].map((node) =>
    (node.textContent ?? '').replace(/\s+/g, ' ').trim(),
  )
}

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('readSpec', () => {
  test('takes the code from a scamper script', () => {
    const el = widget(
      `<div class="${EMBED_CLASS}"><script type="text/scamper">(+ 1 2)</script></div>`,
    )
    expect(readSpec(el).code).toBe('(+ 1 2)')
  })

  test('takes it from the element text when there is no script', () => {
    // The form the issue's own example uses.
    const el = widget(`<div class="${EMBED_CLASS}">\n(+ 1 2)\n</div>`)
    expect(readSpec(el).code).toBe('(+ 1 2)')
  })

  test('reads a preamble, a continuation and a height', () => {
    const el = widget(
      `<div class="${EMBED_CLASS}" data-continues="first" data-height="10em">
         <script type="text/scamper-preamble">(define x 1)</script>
         <script type="text/scamper">x</script>
       </div>`,
    )
    expect(readSpec(el)).toMatchObject({
      code: 'x',
      preamble: '(define x 1)',
      continues: 'first',
      height: '10em',
    })
  })

  test('a widget with no preamble reports an empty one', () => {
    const el = widget(`<div class="${EMBED_CLASS}">1</div>`)
    expect(readSpec(el).preamble).toBe('')
    expect(readSpec(el).continues).toBeNull()
    expect(readSpec(el).height).toBeNull()
  })
})

describe('running a widget', () => {
  test('shows the code and what it produced', async () => {
    const el = widget(`<div class="${EMBED_CLASS}">(+ 1 2)</div>`)
    await runEmbeds()

    expect(captions(el)).toEqual(['(+ 1 2)'])
    expect(transcript(el)).toContain('3')
  })

  test('interleaves each statement with its own output', async () => {
    // The whole point of a transcript: not all the code and then all the
    // output, but each statement above what it produced.
    const el = widget(`<div class="${EMBED_CLASS}">(+ 1 2)\n(* 3 4)</div>`)
    await runEmbeds()

    expect(captions(el)).toEqual(['(+ 1 2)', '(* 3 4)'])
    const text = transcript(el)
    expect(text.indexOf('3')).toBeLessThan(text.indexOf('(* 3 4)'))
  })

  test('runs a preamble without showing it', async () => {
    const el = widget(
      `<div class="${EMBED_CLASS}">
         <script type="text/scamper-preamble">(define x 41)</script>
         <script type="text/scamper">(+ x 1)</script>
       </div>`,
    )
    await runEmbeds()

    expect(transcript(el)).toContain('42')
    // The preamble is setup the reader is not meant to see.
    expect(captions(el)).toEqual(['(+ x 1)'])
    expect(transcript(el)).not.toContain('41')
  })

  test('a fixed height gets a scrollbar; otherwise it sizes to its contents', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="tall" data-height="8em">1</div>
      <div class="${EMBED_CLASS}" id="auto">2</div>`
    await runEmbeds()

    const tall = document.getElementById('tall')!
    expect(tall.style.height).toBe('8em')
    expect(tall.style.overflowY).toBe('auto')
    expect(document.getElementById('auto')!.style.height).toBe('')
  })

  test('reports an error in the transcript rather than throwing', async () => {
    const el = widget(`<div class="${EMBED_CLASS}">(car null)</div>`)
    await expect(runEmbeds()).resolves.toBeUndefined()

    expect(transcript(el).toLowerCase()).toContain('error')
  })

  test('a widget that does not compile leaves its diagnostics and does not stop the page', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="broken">(+ 1</div>
      <div class="${EMBED_CLASS}" id="fine">(+ 20 22)</div>`
    await runEmbeds()

    // The later widget still ran, which is what matters on a page of examples.
    expect(transcript(document.getElementById('fine')!)).toContain('42')
  })
})

describe('several widgets on one page', () => {
  test('do not see each other by default', async () => {
    // Env is persistent and every widget starts from the standard library, so
    // one widget's definitions cannot leak into the next.
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(define x 1) x</div>
      <div class="${EMBED_CLASS}" id="b">x</div>`
    await runEmbeds()

    expect(transcript(document.getElementById('b')!).toLowerCase()).toContain(
      'error',
    )
  })

  test('data-continues carries the previous widget forward', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(define x 41)</div>
      <div class="${EMBED_CLASS}" id="b" data-continues>(+ x 1)</div>`
    await runEmbeds()

    expect(transcript(document.getElementById('b')!)).toContain('42')
  })

  test('data-continues can name a widget other than the one before', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="defs">(define x 41)</div>
      <div class="${EMBED_CLASS}" id="other">(define x 0)</div>
      <div class="${EMBED_CLASS}" id="uses" data-continues="defs">(+ x 1)</div>`
    await runEmbeds()

    // Named, so it continues `defs` rather than the `other` immediately above.
    expect(transcript(document.getElementById('uses')!)).toContain('42')
  })

  test('a chain continues through several widgets', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(define x 1)</div>
      <div class="${EMBED_CLASS}" id="b" data-continues>(define y (+ x 1))</div>
      <div class="${EMBED_CLASS}" id="c" data-continues>(+ x y)</div>`
    await runEmbeds()

    expect(transcript(document.getElementById('c')!)).toContain('3')
  })

  test('a widget continuing one that failed to compile still runs', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(+ 1</div>
      <div class="${EMBED_CLASS}" id="b" data-continues>(+ 20 22)</div>`
    await runEmbeds()

    // It gets the standard library rather than a broken environment.
    expect(transcript(document.getElementById('b')!)).toContain('42')
  })
})

// The reason the per-run refactor was worth doing (#375). Before it, the
// singleton held one set of run slots: starting a second program aborted the
// first one's AbortSignal, so every widget on a page tore down the handlers of
// the widget above it, and a callback resolved its environment and its error
// channel from whichever program happened to have run last.
//
// A button's callback discards its result and `display` is a statement form, so
// what a callback does is only observable when it fails -- which is convenient,
// because *where* the failure is reported is the thing under test.
describe('interactivity survives across widgets', () => {
  /** Clicks the button a widget rendered. */
  function clickButton(el: HTMLElement): void {
    el.querySelector('button')!.dispatchEvent(new MouseEvent('click'))
  }

  /** Lets the spawned callback fiber run to completion. */
  async function settle(): Promise<void> {
    for (let i = 0; i < 30; i++) await new Promise((r) => setTimeout(r, 0))
  }

  test("a later widget does not tear down an earlier one's button", async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="first">(import html)
(button "first" (lambda () (car null)))</div>
      <div class="${EMBED_CLASS}" id="second">(+ 1 2)</div>`
    await runEmbeds()

    const first = document.getElementById('first')!
    expect(transcript(first).toLowerCase()).not.toContain('error')

    // The click still reaches the handler even though another program has
    // started since -- which is exactly what used to fail, silently.
    clickButton(first)
    await settle()
    expect(transcript(first).toLowerCase()).toContain('error')
  })

  test('a callback reports into its own widget, not the last one to run', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="first">(import html)
(button "first" (lambda () (car null)))</div>
      <div class="${EMBED_CLASS}" id="second">(+ 1 2)</div>`
    await runEmbeds()

    clickButton(document.getElementById('first')!)
    await settle()

    expect(transcript(document.getElementById('first')!).toLowerCase()).toContain('error')
    expect(transcript(document.getElementById('second')!).toLowerCase()).not.toContain('error')
  })

  test("a callback sees its own widget's definitions", async () => {
    // spawnClosure reads the run's own evolving top level. `only-in-a` exists
    // in the first widget and nowhere else, so the first click succeeds and the
    // second cannot resolve it -- the plainest evidence that each callback got
    // its own environment rather than a shared last-one-wins.
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(import html)
(define only-in-a (list 1 2))
(button "a" (lambda () (car only-in-a)))</div>
      <div class="${EMBED_CLASS}" id="b">(import html)
(button "b" (lambda () (car only-in-a)))</div>`
    await runEmbeds()

    const a = document.getElementById('a')!
    const b = document.getElementById('b')!
    clickButton(a)
    clickButton(b)
    await settle()

    expect(transcript(a).toLowerCase()).not.toContain('error')
    expect(transcript(b)).toContain('only-in-a')
  })

  test('every widget on the page keeps its own button', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="a">(import html)
(button "a" (lambda () (car null)))</div>
      <div class="${EMBED_CLASS}" id="b">(import html)
(button "b" (lambda () (car null)))</div>
      <div class="${EMBED_CLASS}" id="c">(import html)
(button "c" (lambda () (car null)))</div>`
    await runEmbeds()

    for (const id of ['a', 'b', 'c']) {
      const el = document.getElementById(id)!
      clickButton(el)
      await settle()
      expect(transcript(el).toLowerCase()).toContain('error')
    }
  })
})

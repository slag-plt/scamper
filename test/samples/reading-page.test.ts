import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, test } from 'vitest'
import { EMBED_CLASS, runEmbeds } from '../../src/app/web/embed/embed'
import { initialize } from '../../src/scamper'

await initialize()
// Importing scamper.ts kicks off its renderer registration as a fire-and-forget
// module-load side effect; settle it here so it cannot land after teardown.
await import('../../src/app/web/renderers.js')

// samples/reading.html is two real readings on one page (#405): the first
// entirely non-interactive, the second interactive throughout. Running it here
// is what keeps it honest, and it covers a mixed page that no other spec does --
// embed-widget.test.ts builds its widgets from inline markup.
//
// Deliberately its own file, apart from scm-samples.test.ts. `runProgram` (the
// harness) gives each program its own private Scheduler, while a page runs on
// the Scamper singleton's; a harness program that registers a DOM handler -- a
// button, a reactive component, both of which libs.scm has -- wedges a singleton
// run later in the same environment. Vitest isolates per file, so keeping the
// two apart is all it takes, and nothing outside the test tier mixes them: the
// IDE, the CLI and a reading all go through the singleton.
//
// N.B., the page is run in two passes rather than one, and the second pass runs
// the song widget before the ball rather than in page order. That is to keep
// this spec off #415: when the machine is contended, a widget holding a live
// `on-timer` subscription can starve the widgets after it on the same page
// *permanently* -- observed stalling past four minutes with the stall landing on
// a different widget each time, while the same page finishes in ~200ms on an
// idle machine. Under `npm test` the other workers are exactly that contention.
// So no pass here runs a widget after a live timer: the first pass has no timer
// in it at all, and the second ends with one. Every widget on the page still
// runs. When #415 is fixed this can go back to one pass in page order.

const SAMPLES = path.resolve(import.meta.dirname, '../../samples')

/** How a reported error renders, whether from the compiler or the runtime. */
const ERROR = /^(Parser|Runtime|Docstring) error/m

/** The page's body, which is what a reading would paste into its own. */
function pageBody(): string {
  const html = readFileSync(path.join(SAMPLES, 'reading.html'), 'utf-8')
  return /<body[^>]*>([\s\S]*)<\/body>/.exec(html)![1]
}

/** A widget's rendered text, whitespace squashed so assertions read. */
function transcript(el: HTMLElement): string {
  return (el.textContent ?? '').replace(/\s+/g, ' ').trim()
}

/** Every widget currently on the page. */
function widgets(): HTMLElement[] {
  return [...document.querySelectorAll<HTMLElement>(`.${EMBED_CLASS}`)]
}

/** The widget holding the animated ball, the page's only timer. */
const BALL = 'reactive-ball'

/** The widget holding the composition that drives a canvas. */
const SONG = 'animated-song'

/**
 * Lays out the page afresh, dropping the widgets `keep` rejects and leaving the
 * rest where they are -- a widget continuing "the one above" needs its
 * neighbours in document order.
 */
function layOut(keep: (el: HTMLElement) => boolean): void {
  document.body.innerHTML = pageBody()
  for (const el of widgets()) {
    if (!keep(el)) el.remove()
  }
}

/** Asserts every widget ran and none of them reported an error. */
function expectAllRan(expected: number): void {
  const found = widgets()
  expect(found).toHaveLength(expected)
  for (const el of found) {
    expect(el.classList.contains('scamper-transcript-ready')).toBe(true)
    expect(transcript(el)).not.toMatch(ERROR)
  }
}

describe('reading.html', () => {
  test('the widgets without a timer all run, in page order', async () => {
    layOut((el) => el.id !== BALL && el.id !== SONG)
    const expected = widgets().length
    expect(expected).toBe(10)
    await runEmbeds()

    expectAllRan(expected)

    // `twice` and `increment` are defined in the widget with id `defs`; the two
    // after it pick that environment up rather than starting from the library.
    const chained = [
      ...document.querySelectorAll<HTMLElement>('[data-continues]'),
    ]
    expect(chained).toHaveLength(2)
    expect(transcript(chained[0])).toContain('42')
    expect(transcript(chained[1])).toContain('20')
  }, 30000)

  // Last, and the timer-bearing widget last within it: its subscription
  // outlives this spec by design, and nothing may run after it. See above.
  test("the interactive reading's widgets run", async () => {
    layOut((el) => el.id === BALL || el.id === SONG)
    // The ball goes last, after the song, so nothing runs behind its timer.
    document.body.appendChild(document.getElementById(BALL)!)
    await runEmbeds()

    expectAllRan(2)
  }, 30000)
})

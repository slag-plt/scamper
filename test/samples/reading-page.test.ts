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
// N.B., the page is not run in one pass, and the two interactive widgets are not
// waited on the way the rest are. That is to keep this spec off #415: on a
// contended machine an embedded run can render everything it is going to render
// and still never be signalled complete. Measured on two saturated cores, the
// song widget rendered its output at 144ms and its run had not resolved 90
// seconds later, while the identical program through `runProgram` -- the same
// code on a private Scheduler -- finished in 69ms. Because `runEmbeds` awaits
// each widget's run before starting the next, one such widget stops the page
// dead, and CI (a two-core runner under full load) hit exactly that.
//
// So: the widgets that register no handlers are run as a page and awaited, which
// is the stronger assertion and is stable. The two interactive ones are each run
// alone and waited on by what they *rendered* rather than by their run
// resolving. Every widget on the page is still run and still checked for errors.
// When #415 is fixed this can collapse back into a single awaited pass.

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

/** The two widgets whose runs register handlers and outlive their programs. */
const INTERACTIVE = [SONG, BALL]

/** Waits for `el` to have rendered something, or gives up and says so. */
async function waitForOutput(el: HTMLElement, budget = 20000): Promise<void> {
  const start = Date.now()
  while (Date.now() - start < budget) {
    if (el.querySelector('.scamper-output') !== null) return
    await new Promise((resolve) => setTimeout(resolve, 50))
  }
  throw new Error(
    `${el.id} rendered nothing within ${budget.toString()}ms; its transcript is ${JSON.stringify(transcript(el))}`,
  )
}

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
  test('the widgets that register nothing all run, in page order', async () => {
    layOut((el) => !INTERACTIVE.includes(el.id))
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

  // Last: these two register handlers that outlive their programs by design, so
  // they are run one to a page and judged by what they rendered. runEmbeds is
  // deliberately not awaited here -- see the note at the top about #415.
  test.each(INTERACTIVE)('%s runs and renders', async (id) => {
    layOut((el) => el.id === id)
    expect(widgets()).toHaveLength(1)

    void runEmbeds()
    const el = document.getElementById(id)!
    await waitForOutput(el)

    expect(el.classList.contains('scamper-transcript-ready')).toBe(true)
    expect(transcript(el)).not.toMatch(ERROR)
  }, 30000)
})

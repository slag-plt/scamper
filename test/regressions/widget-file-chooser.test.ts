import { beforeEach, describe, expect, test } from 'vitest'
import { EMBED_CLASS, runEmbeds } from '../../src/app/web/embed/embed'
import { initialize } from '../../src/scamper'

await initialize()
// Registers the Vue renderers, the file chooser's among them.
await import('../../src/app/web/renderers.js')

// #397: a reactive file chooser in a reading widget showed its file input, and
// choosing a file did nothing at all.
//
// #375 made a callback belong to the program that registered it, by capturing
// the run at registration -- which works for a library function that registers
// a DOM listener, because that runs while the program is stepping. The two Vue
// renderers cannot do the same: Vue mounts them after the step that produced
// the value has finished, so the free `spawn` resolved to the *foreground* run.
// In the IDE that fallback happens to be right, which is why this was invisible
// there. A reading page has no foreground run, so the spawn was dropped.
//
// The fix is for the value to carry its run, captured where the library
// function builds it. As with the button tests in embed-widget.test.ts, a
// callback's result is discarded, so what it did is observable only when it
// fails -- and *where* the failure is reported is the thing under test.

/** Lets the FileReader and then the spawned callback fiber run out. */
async function settle(): Promise<void> {
  for (let i = 0; i < 60; i++) await new Promise((r) => setTimeout(r, 0))
}

/** Chooses `text` as the file in the chooser `el` rendered. */
function chooseFile(el: HTMLElement, text: string): void {
  const input = el.querySelector<HTMLInputElement>('input[type="file"]')
  if (input === null) throw new Error('the widget rendered no file input')
  Object.defineProperty(input, 'files', {
    value: [new File([text], 'chosen.txt', { type: 'text/plain' })],
    configurable: true,
  })
  input.dispatchEvent(new Event('change'))
}

const transcript = (el: HTMLElement) =>
  el.textContent.replace(/\s+/g, ' ').trim()

const byId = (id: string): HTMLElement => {
  const el = document.getElementById(id)
  if (el === null) throw new Error(`no widget with id ${id}`)
  return el
}

describe('a file chooser works inside a reading widget (#397)', () => {
  beforeEach(() => {
    document.body.innerHTML = ''
  })

  test('choosing a file runs the callback', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="only">(with-file-chooser (lambda (s) (car null)))</div>`
    await runEmbeds()

    const el = byId('only')
    expect(transcript(el).toLowerCase()).not.toContain('error')

    chooseFile(el, 'anything')
    await settle()

    // The callback ran: `(car null)` is a contract violation, and it is
    // reported into this widget. Before the fix nothing happened at all.
    expect(transcript(el).toLowerCase()).toContain('error')
  })

  test('the callback reports into its own widget, not the last one to run', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="first">(with-file-chooser (lambda (s) (car null)))</div>
      <div class="${EMBED_CLASS}" id="second">(+ 1 2)</div>`
    await runEmbeds()

    chooseFile(byId('first'), 'anything')
    await settle()

    expect(transcript(byId('first')).toLowerCase()).toContain('error')
    expect(transcript(byId('second')).toLowerCase()).not.toContain('error')
  })

  test("the callback sees its own widget's definitions", async () => {
    // The run carries the program's *evolving* top level, so a definition made
    // beside the chooser is in scope when the callback finally fires.
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="first">(define only-here 1)
(with-file-chooser (lambda (s) only-here))</div>
      <div class="${EMBED_CLASS}" id="second">(with-file-chooser (lambda (s) only-here))</div>`
    await runEmbeds()

    chooseFile(byId('first'), 'anything')
    chooseFile(byId('second'), 'anything')
    await settle()

    expect(transcript(byId('first')).toLowerCase()).not.toContain('error')
    expect(transcript(byId('second')).toLowerCase()).toContain('error')
  })
})

import { mount } from '@vue/test-utils'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { nextTick } from 'vue'
import OutputPane from '../../../src/app/web/components/OutputPane.vue'
import Scamper, { initialize } from '../../../src/scamper'
import { setShowSourceWithOutput } from '../../../src/app/web/output-prefs'

await initialize()

/** The scroll container's height; anything smaller renders no rows at all. */
const PANE_HEIGHT = 600
const VALUE_HEIGHT = 20
const CAPTION_HEIGHT = 30

/**
 * jsdom lays nothing out, so every element is zero-sized and the virtualizer
 * renders nothing. These give the pieces the output pane is built from heights
 * that behave the way a browser's do -- in particular, a caption hidden with
 * `display: none` measures zero, which is the whole point of what follows.
 */
function installLayout() {
  Object.defineProperty(HTMLElement.prototype, 'offsetHeight', {
    configurable: true,
    get(this: HTMLElement) {
      if (this.classList.contains('output-scroll')) return PANE_HEIGHT
      if (this.classList.contains('base-item')) return VALUE_HEIGHT
      if (this.classList.contains('source-caption')) {
        const inner = this.querySelector<HTMLElement>('.source-caption')
        return inner && inner.style.display !== 'none' ? CAPTION_HEIGHT : 0
      }
      return 0
    },
  })
  Object.defineProperty(HTMLElement.prototype, 'offsetWidth', {
    configurable: true,
    get: () => 400,
  })
}

function removeLayout() {
  for (const prop of ['offsetHeight', 'offsetWidth']) {
    Object.defineProperty(HTMLElement.prototype, prop, {
      configurable: true,
      get: () => 0,
    })
  }
}

// The output pane is virtualized: it places each row by hand from heights it
// measured earlier. Revealing the captions changes those heights, and if the
// pane does not measure again every caption stays zero-tall and is drawn
// underneath the output it belongs to -- visible in the DOM, invisible on
// screen, and indistinguishable from the toggle doing nothing at all.
describe('output pane source captions', () => {
  beforeEach(() => {
    installLayout()
    setShowSourceWithOutput(false)
  })

  afterEach(() => {
    removeLayout()
    setShowSourceWithOutput(false)
    document.body.innerHTML = ''
  })

  /** Mounts the pane and runs `src` through it, as the IDE does. */
  async function runInPane(src: string) {
    const wrapper = mount(OutputPane, { attachTo: document.body })
    await nextTick()
    const channel = (
      wrapper.vm as unknown as { display: Parameters<typeof Scamper.prototype.execute>[0]['out'] }
    ).display
    const run = await Scamper.getInstance().execute({
      src,
      out: channel,
      err: channel,
      isTracing: false,
    })
    if (run === null) throw new Error(`${src} did not compile`)
    await run.done
    await new Promise((resolve) => requestAnimationFrame(resolve))
    await settle()
    return wrapper
  }

  /** Lets the pane re-render and re-measure; it takes a few passes. */
  async function settle() {
    for (let i = 0; i < 6; i++) await nextTick()
  }

  /** Each row's class and the y it is placed at, in order. */
  function layout(): { kind: string; y: number }[] {
    return [
      ...document.querySelectorAll<HTMLElement>('.output-scroll > div > div'),
    ].map((row) => ({
      kind: row.className,
      y: Number(/translateY\(([-\d.]+)px\)/.exec(row.style.transform)?.[1] ?? NaN),
    }))
  }

  /** The height the pane reserves for the whole list. */
  function totalHeight(): number {
    const inner = document.querySelector<HTMLElement>('.output-scroll > div')
    return Number.parseFloat(inner?.style.height ?? '0')
  }

  test('captions take no space until they are shown', async () => {
    const wrapper = await runInPane('(display 1)\n(display 2)')
    try {
      expect(layout()).toEqual([
        { kind: 'source-caption', y: 0 },
        { kind: 'base-item', y: 0 },
        { kind: 'source-caption', y: VALUE_HEIGHT },
        { kind: 'base-item', y: VALUE_HEIGHT },
      ])
      expect(totalHeight()).toBe(2 * VALUE_HEIGHT)
    } finally {
      wrapper.unmount()
    }
  })

  test('showing them re-lays out the list rather than overlapping it', async () => {
    const wrapper = await runInPane('(display 1)\n(display 2)')
    try {
      setShowSourceWithOutput(true)
      await settle()

      // Each caption now sits above its own value instead of underneath it.
      expect(layout()).toEqual([
        { kind: 'source-caption', y: 0 },
        { kind: 'base-item', y: CAPTION_HEIGHT },
        { kind: 'source-caption', y: CAPTION_HEIGHT + VALUE_HEIGHT },
        { kind: 'base-item', y: 2 * CAPTION_HEIGHT + VALUE_HEIGHT },
      ])
      expect(totalHeight()).toBe(2 * (CAPTION_HEIGHT + VALUE_HEIGHT))
    } finally {
      wrapper.unmount()
    }
  })

  test('hiding them gives the space back', async () => {
    const wrapper = await runInPane('(display 1)\n(display 2)')
    try {
      setShowSourceWithOutput(true)
      await settle()
      setShowSourceWithOutput(false)
      await settle()

      expect(totalHeight()).toBe(2 * VALUE_HEIGHT)
    } finally {
      wrapper.unmount()
    }
  })

  test('output that arrives with captions already on is laid out correctly', async () => {
    // The persisted preference is read at mount, so this is what a reload
    // looks like -- no toggle involved, and no re-measure to rescue it.
    setShowSourceWithOutput(true)
    const wrapper = await runInPane('(display 1)')
    try {
      expect(layout()).toEqual([
        { kind: 'source-caption', y: 0 },
        { kind: 'base-item', y: CAPTION_HEIGHT },
      ])
    } finally {
      wrapper.unmount()
    }
  })

  // Deliberate, and worth stating plainly because it looks like noise: the
  // interleaved view exists to show students which part of their code produced
  // which output, and a faithful interleaving is what does that. A `define`
  // with an empty box under it is information -- that statement ran and
  // produced nothing. Folding silent statements away would make the output a
  // list of results rather than a transcript of the program.
  test('a statement that prints nothing gets its own box', async () => {
    setShowSourceWithOutput(true)
    const wrapper = await runInPane('(define x 5)\n(define y 6)\n(display x)')
    try {
      // Three captions in three boxes, not two silent ones folded into the
      // third, and each is its own row in the list.
      const boxes = [
        ...document.querySelectorAll('.source-caption .source-caption'),
      ].map((b) => b.textContent)
      expect(boxes).toEqual(['(define x 5)', '(define y 6)', '(display x)'])

      expect(layout().map((r) => r.kind)).toEqual([
        'source-caption',
        'source-caption',
        'source-caption',
        'base-item',
      ])
    } finally {
      wrapper.unmount()
    }
  })

  test('the caption holds the statement, highlighted', async () => {
    setShowSourceWithOutput(true)
    const wrapper = await runInPane('(display 1)')
    try {
      const caption = document.querySelector('.source-caption .source-caption')
      expect(caption?.textContent).toBe('(display 1)')
      expect(
        caption?.querySelector('.scamper-hl-keyword')?.textContent,
      ).toBe('display')
    } finally {
      wrapper.unmount()
    }
  })
})

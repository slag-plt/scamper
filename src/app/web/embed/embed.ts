import Scamper, { type Env } from '../../../scamper'
import { EmbedDisplay } from './embed-display'

/**
 * The reading widget (#375): a block of Scamper code in a page, replaced by a
 * transcript of that code interleaved with what it produced.
 *
 * Every widget on a page is run by the one Scamper instance, each as its own
 * program, so one widget's definitions do not leak into another's unless the
 * author says they should.
 */

/** The class an author puts on a widget. */
export const EMBED_CLASS = 'scamper-transcript'

/** A widget as read off the page, before anything has run. */
export interface EmbedSpec {
  el: HTMLElement
  /** The code shown in the transcript and run. */
  code: string
  /** Code run before `code` but not shown, if any. */
  preamble: string
  /**
   * The widget whose environment this one continues, if any: `data-continues`
   * naming an id, or empty for the widget before it in document order.
   */
  continues: string | null
  /** A fixed height with a scrollbar, or null to size to the contents. */
  height: string | null
}

/**
 * @returns the text of the first `<script type="text/scamper...">` matching
 *          `suffix`, or null if the widget has none.
 *
 * A `<script>` rather than the element's own text because a browser does not
 * parse its contents as markup, so code holding `<` or `&` survives verbatim.
 */
function scriptText(el: HTMLElement, suffix: string): string | null {
  const script = el.querySelector<HTMLScriptElement>(
    `script[type="text/scamper${suffix}"]`,
  )
  return script === null ? null : script.textContent
}

/**
 * Reads one widget's code off the page.
 *
 * The code may be written either as a `<script type="text/scamper">` or as the
 * element's own text, which is what the issue's example does. Bare text cannot
 * carry a preamble, so a widget wanting one uses the script form.
 */
export function readSpec(el: HTMLElement): EmbedSpec {
  const scripted = scriptText(el, '')
  const continues = el.getAttribute('data-continues')
  return {
    el,
    code: (scripted ?? el.textContent).trim(),
    preamble: (scriptText(el, '-preamble') ?? '').trim(),
    continues,
    height: el.getAttribute('data-height'),
  }
}

/**
 * Replaces `el`'s contents with the shell a transcript is rendered into.
 * @returns the element output is appended to
 */
function prepare(spec: EmbedSpec): HTMLElement {
  const { el, height } = spec
  el.textContent = ''
  el.classList.add('scamper-transcript-ready')
  if (height !== null) {
    el.style.height = height
    el.style.overflowY = 'auto'
  }
  const body = document.createElement('div')
  body.classList.add('scamper-transcript-body')
  el.appendChild(body)
  return body
}

/**
 * @returns the environment each widget should start from, keyed by element.
 *
 * A widget with `data-continues` starts from the *final* environment of the
 * widget it names -- or of the one before it, when the attribute is empty --
 * which is what makes a reading's examples build on each other. Anything else
 * starts from the standard library alone.
 */
function seedFor(
  spec: EmbedSpec,
  previous: EmbedSpec | undefined,
  finished: Map<HTMLElement, Env>,
): Env | undefined {
  if (spec.continues === null) return undefined
  const source =
    spec.continues === ''
      ? previous?.el
      : (document.getElementById(spec.continues) ?? undefined)
  return source === undefined ? undefined : finished.get(source)
}

/**
 * Runs one widget, showing its preamble's output nowhere and its code's output
 * in the transcript.
 *
 * @returns the program's final top-level environment, so a later widget can
 *          continue it. Undefined if the code did not compile.
 */
async function runOne(
  spec: EmbedSpec,
  seed: Env | undefined,
): Promise<Env | undefined> {
  const scamper = Scamper.getInstance()
  const body = prepare(spec)

  // The preamble runs first and separately, into a display nothing is attached
  // to: it is setup the reader is not meant to see, and its environment is what
  // the visible code then starts from.
  let env = seed
  if (spec.preamble.length > 0) {
    const hidden = new EmbedDisplay(document.createElement('div'))
    const request = await scamper.executeEmbedded({
      src: spec.preamble,
      out: hidden,
      err: hidden,
      env,
    })
    if (request === null) return undefined
    env = await request.done
  }

  const display = new EmbedDisplay(body)
  const request = await scamper.executeEmbedded({
    src: spec.code,
    out: display,
    err: display,
    env,
  })
  if (request === null) return undefined
  return await request.done
}

/**
 * Runs every widget in `root`, in document order.
 *
 * Sequential rather than concurrent, because a widget may continue the one
 * before it and so cannot start until that one's environment exists. It is also
 * the order a reader reads in.
 */
export async function runEmbeds(root: ParentNode = document): Promise<void> {
  const specs = [...root.querySelectorAll<HTMLElement>(`.${EMBED_CLASS}`)].map(
    readSpec,
  )
  const finished = new Map<HTMLElement, Env>()
  for (const [index, spec] of specs.entries()) {
    const env = await runOne(spec, seedFor(spec, specs[index - 1], finished))
    if (env !== undefined) finished.set(spec.el, env)
  }
}

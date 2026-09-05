/**
 * The REPL transcript as plain text, for the Copy button (#459).
 *
 * Shaped like what a drag across the entries already puts on the clipboard:
 * each entry's source followed by what it printed, with no prompt markers --
 * the `>` is `user-select: none` -- and no comment prefixes. That is also what
 * the embedded reading widget calls a transcript, so there is one such thing
 * rather than two.
 *
 * The two are not identical, and cannot be: a drag copies the DOM the Vue
 * renderers built, so a drawing (a canvas) copies as nothing at all, while
 * this renders every value through TextRenderer and so writes the drawing's
 * constructor expression. Where they differ, this is the more useful text.
 *
 * Framework-free, so it can be tested on plain objects rather than by mounting
 * the window; see notebook-display.ts.
 */
import type { Value } from '../../lpm'
import TextRenderer from '../../lpm/renderers/text'

/** An entry, as the transcript needs to know it. */
export interface TranscriptEntry {
  /** What was typed, or '' for output from something the file left running. */
  source: string
  /** What it printed, errors included, in the order it arrived. */
  values: readonly Value[]
}

/**
 * The most of one value that is copied. A value can be arbitrarily large -- a
 * sound holds every one of its samples, and TextRenderer spells an unknown
 * object out as JSON -- and handing tens of megabytes to the clipboard helps
 * nobody. This bounds what is written, not what is built: TextRenderer has no
 * limit to pass, so the whole string is rendered and then cut.
 */
const MAX_VALUE_CHARS = 4000

/** What stands in for the rest of a value too long to copy. */
const ELISION = '...'

/** One value, as the text a person read, capped at {@link MAX_VALUE_CHARS}. */
function renderValue(v: Value): string {
  const text = TextRenderer.render(v)
  return text.length <= MAX_VALUE_CHARS
    ? text
    : text.slice(0, MAX_VALUE_CHARS) + ELISION
}

/**
 * `entries` as one block of text: every source line and every result, in the
 * order they happened.
 *
 * The banner is left out on purpose -- it says what the session was seeded
 * from, which is chrome rather than work.
 */
export function transcriptText(entries: readonly TranscriptEntry[]): string {
  const lines: string[] = []
  for (const entry of entries) {
    // An entry with no source is output from something the seeded file left
    // running, so there is nothing typed to write above it.
    if (entry.source.length > 0) {
      lines.push(entry.source)
    }
    for (const value of entry.values) {
      lines.push(renderValue(value))
    }
  }
  return lines.join('\n')
}

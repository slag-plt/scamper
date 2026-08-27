import { markdown } from '@codemirror/lang-markdown'
import { StreamLanguage } from '@codemirror/language'
import type { Extension } from '@codemirror/state'
import { fileKindOf } from '../../../fs/fs'
import { ScamperSupport } from './extensions/language'

/**
 * How a file is edited: which language colours it, and whether the features
 * that only make sense for a Scamper program apply (#385).
 *
 * Before this, every file opened as if it were Scheme -- so a `.txt` file was
 * marked up with diagnostics from a language it is not written in.
 */
export interface EditorMode {
  /** Language support for highlighting, or nothing for a file we cannot. */
  language: Extension
  /**
   * Whether this file gets the Scamper-only extensions: the LSP, the
   * formatter, the re-indenter, inline queries, and the Run button.
   */
  isScamper: boolean
}

/**
 * Comma- and tab-separated data, highlighted just enough to see the columns.
 *
 * Written here rather than pulled in as a dependency: a separator, a quoted
 * field, and everything else is the whole of the grammar worth having, and a
 * package for it would be larger than the rule it encodes.
 */
const csvLanguage = StreamLanguage.define<{ afterSeparator: boolean }>({
  startState: () => ({ afterSeparator: true }),
  token(stream, state) {
    if (stream.eat(',') || stream.eat('\t')) {
      state.afterSeparator = true
      return 'punctuation'
    }

    // A quoted field runs to its closing quote, doubled quotes included.
    if (stream.peek() === '"') {
      stream.next()
      while (!stream.eol()) {
        if (stream.next() === '"' && stream.peek() !== '"') break
        if (stream.peek() === '"') stream.next()
      }
      state.afterSeparator = false
      return 'string'
    }

    stream.eatWhile((ch) => ch !== ',' && ch !== '\t')
    const wasFirst = state.afterSeparator
    state.afterSeparator = false
    // The leading field of a row reads as its label, which is what makes a
    // header row stand out without parsing one.
    return wasFirst ? 'keyword' : 'number'
  },
})

/** Languages we can highlight, by lower-cased extension. */
const LANGUAGES: Record<string, () => Extension> = {
  md: markdown,
  markdown: markdown,
  csv: () => csvLanguage,
  tsv: () => csvLanguage,
}

/** @returns `name`'s lower-cased extension without its dot, or ''. */
function extensionOf(name: string): string {
  const dot = name.lastIndexOf('.')
  return dot <= 0 ? '' : name.slice(dot + 1).toLowerCase()
}

/**
 * @returns how a file of this name is edited.
 *
 * A Scamper program gets everything. Any other text file gets highlighting if
 * we have it and a plain editor otherwise -- never the LSP, whose diagnostics
 * would be about a language the file is not written in.
 */
export function modeFor(filename: string): EditorMode {
  if (fileKindOf(filename) === 'scamper') {
    return { language: ScamperSupport(), isScamper: true }
  }

  // A lookup by an arbitrary extension misses far more often than it hits; it
  // reads as always-present only because `noUncheckedIndexedAccess` is off.
  const language = LANGUAGES[extensionOf(filename)]
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  return { language: language === undefined ? [] : language(), isScamper: false }
}

/** The mode an empty editor uses, so the placeholder still reads as Scheme. */
export const scamperMode: EditorMode = {
  language: ScamperSupport(),
  isScamper: true,
}

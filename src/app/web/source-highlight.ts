import { highlightTree, tagHighlighter, tags as t } from '@lezer/highlight'
import { ScamperLanguage } from './codemirror/extensions/language'

/**
 * Static syntax highlighting for a snippet of Scamper source.
 *
 * The editor highlights through CodeMirror, which is far too much machinery to
 * put behind every line of output -- a read-only EditorView per statement, all
 * of them inside a virtualized list. This runs the same Lezer grammar the
 * editor uses and returns plain tokens, which a component renders as spans. No
 * editor, no `v-html`, and the classes are the `scamper-hl-*` ones that
 * traces already use, so highlighted source matches highlighted traces.
 */

/** A run of source text and the highlight class it should carry, if any. */
export interface SourceToken {
  text: string
  cls: string | null
}

const highlighter = tagHighlighter([
  { tag: t.keyword, class: 'scamper-hl-keyword' },
  { tag: t.number, class: 'scamper-hl-number' },
  { tag: [t.string, t.character], class: 'scamper-hl-string' },
  { tag: [t.bool, t.atom, t.null], class: 'scamper-hl-literal' },
  { tag: [t.lineComment, t.blockComment], class: 'scamper-hl-comment' },
])

/**
 * Splits `src` into highlighted tokens.
 * @returns the tokens, in order, covering `src` exactly -- unhighlighted gaps
 *          (whitespace, parens, plain identifiers) included, with a null class.
 */
export function highlightScamper(src: string): SourceToken[] {
  const tokens: SourceToken[] = []
  let pos = 0
  highlightTree(
    ScamperLanguage.parser.parse(src),
    highlighter,
    (from, to, cls) => {
      // highlightTree only reports the styled ranges, so the text between them
      // has to be carried across or the snippet would lose its whitespace.
      if (from > pos) tokens.push({ text: src.slice(pos, from), cls: null })
      tokens.push({ text: src.slice(from, to), cls })
      pos = to
    },
  )
  if (pos < src.length) tokens.push({ text: src.slice(pos), cls: null })
  return tokens
}

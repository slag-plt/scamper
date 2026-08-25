import { parser } from '../../../../scheme/generated/parser.js'
import {
  foldInside,
  foldNodeProp,
  LanguageSupport,
  LRLanguage,
} from '@codemirror/language'
import { styleTags, tags as t } from '@lezer/highlight'
import { reservedWords } from '../../../../scheme/reserved-words'
import { scamperIndentation } from './indentation'

export const ScamperLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      // DrRacket-style indentation, driven by src/scheme/style.ts.
      scamperIndentation,
      foldNodeProp.add({
        Application: foldInside,
      }),
      styleTags({
        [reservedWords.join(' ')]: t.keyword,
        Identifier: t.variableName,
        Boolean: t.bool,
        Number: t.number,
        String: t.string,
        Char: t.character,
        LineComment: t.lineComment,
        '( )': t.paren,
        '[ ]': t.squareBracket,
        '{ }': t.brace,
        Amp: t.punctuation,
      }),
    ],
  }),
  languageData: {
    commentTokens: { line: ';' },
    // Typing a closing bracket as the first thing on a line snaps that line to
    // its proper indentation. `indentOnInput()` is already installed in
    // codemirror.ts, but it does nothing until the language supplies this.
    indentOnInput: /^\s*[)\]}]$/,
  },
})

export function ScamperSupport() {
  return new LanguageSupport(ScamperLanguage)
}

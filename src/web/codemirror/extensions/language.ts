import { parser } from "../syntax.grammar"
import {
  continuedIndent,
  foldInside,
  foldNodeProp,
  indentNodeProp,
  LanguageSupport,
  LRLanguage,
} from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"
import { reservedWords } from "../../../scheme/parser"

export const ScamperLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Import: continuedIndent({ except: undefined, units: 1 }),
        Define: continuedIndent({ except: undefined, units: 1 }),
        Display: continuedIndent({ except: undefined, units: 1 }),
        Lambda: continuedIndent({ except: undefined, units: 1 }),
        arglist: continuedIndent({ except: undefined, units: 1 }),
        Application: continuedIndent({ except: undefined, units: 1 }),
        And: continuedIndent({ except: undefined, units: 1 }),
        Or: continuedIndent({ except: undefined, units: 1 }),
        If: continuedIndent({ except: undefined, units: 1 }),
        Cond: continuedIndent({ except: undefined, units: 1 }),
        binding: continuedIndent({ except: undefined, units: 1 }),
      }),
      foldNodeProp.add({
        Application: foldInside,
      }),
      styleTags({
        [reservedWords.join(" ")]: t.keyword,
        Identifier: t.variableName,
        Boolean: t.bool,
        Number: t.number,
        String: t.string,
        Char: t.character,
        LineComment: t.lineComment,
        "( )": t.paren,
        "[ ]": t.squareBracket,
        "{ }": t.brace,
      }),
    ],
  }),
  languageData: {
    commentTokens: { line: ";" },
  },
})

export function ScamperSupport() {
  return new LanguageSupport(ScamperLanguage)
}

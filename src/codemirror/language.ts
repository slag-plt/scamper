import {parser} from "./syntax.grammar"
import {LRLanguage, LanguageSupport, indentNodeProp, foldNodeProp, foldInside, delimitedIndent} from "@codemirror/language"
import {styleTags, tags as t} from "@lezer/highlight"

export const ScamperLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Application: delimitedIndent({closing: ")", align: false})
      }),
      foldNodeProp.add({
        Application: foldInside
      }),
      styleTags({
        "and begin cond define display if import lambda let let* letrec match or struct": t.keyword,        
        Identifier: t.variableName,
        Boolean: t.bool,
        Number: t.number,
        String: t.string,
        LineComment: t.lineComment,
        "( )": t.paren,
        "[ ]": t.squareBracket,
        "{ }": t.brace
      })
    ]
  }),
  languageData: {
    commentTokens: {line: ";"}
  }
})

export function ScamperSupport() {
  return new LanguageSupport(ScamperLanguage)
}

import {parser} from "./syntax.grammar"
import {LRLanguage, LanguageSupport, indentNodeProp, foldNodeProp, foldInside, continuedIndent, delimitedIndent} from "@codemirror/language"
import {styleTags, tags as t} from "@lezer/highlight"

export const ScamperLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Import: continuedIndent({except: undefined, units: 1}),
        Define: continuedIndent({except: undefined, units: 1}),
        Display: continuedIndent({except: undefined, units: 1}),
        Lambda: continuedIndent({except: undefined, units: 1}),
        arglist: continuedIndent({except: undefined, units: 1}),
        Application: continuedIndent({except: undefined, units: 1}),
        And: continuedIndent({except: undefined, units: 1}),
        Or: continuedIndent({except: undefined, units: 1}),
        Cond: continuedIndent({except: undefined, units: 1}),
        binding: continuedIndent({except: undefined, units: 1}),
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

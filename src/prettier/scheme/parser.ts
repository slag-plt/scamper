import { Parser } from "prettier"
import { SchemeNode, progToNode } from "../../scheme/ast"
import { tokenizeAndParse } from "../../scheme"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import { throwNull } from "../../util"

export const SchemeParserName = "scamper-scheme"

const errChannel = new SimpleErrorChannel()

export const SchemeParserASTFormat = `${SchemeParserName}-ast`

export const SchemeParser: Parser<SchemeNode> = {
  parse: (text) => {
    return progToNode(
      tokenizeAndParse(errChannel, text) ??
        throwNull(errChannel.getSubthreadErrors().message),
    )
  },
  astFormat: SchemeParserASTFormat,
  locStart: (node) => node.range.begin.idx,
  locEnd: (node) => node.range.end.idx,
}

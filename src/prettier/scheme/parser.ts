import { Parser } from 'prettier'
import { SchemeNode, progToNode } from '../../scheme/ast'
import { tokenizeAndParse } from '../../scheme'

export const SchemeParserName = 'scamper-scheme'

function throwNull(message: string): never {
  throw new Error(message)
}

export const SchemeParserASTFormat = `${SchemeParserName}-ast`

export const SchemeParser: Parser<SchemeNode> = {
  parse: (text) => {
    const { program, diagnostics } = tokenizeAndParse(text)
    return progToNode(
      program ??
        throwNull(diagnostics.map((d) => d.message).join('; ')),
    )
  },
  astFormat: SchemeParserASTFormat,
  locStart: (node) => node.range.begin.idx,
  locEnd: (node) => node.range.end.idx,
}

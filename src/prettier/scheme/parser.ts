import { Parser } from 'prettier'
import { SchemeNode, progToNode } from '../../scheme/ast'
import { tokenizeAndParse } from '../../scheme'
import { attachComments, collectComments } from '../../scheme/comments'

export const SchemeParserName = 'scamper-scheme'

function throwNull(message: string): never {
  throw new Error(message)
}

export const SchemeParserASTFormat = `${SchemeParserName}-ast`

export const SchemeParser: Parser<SchemeNode> = {
  parse: (text) => {
    const { program, diagnostics } = tokenizeAndParse(text)
    const root = progToNode(
      program ?? throwNull(diagnostics.map((d) => d.message).join('; ')),
    )
    // Ornament the AST with its source comments so the printer can re-emit them
    // (see comments.ts). Prettier's own comment machinery is not used.
    attachComments(root, collectComments(text))
    return root
  },
  astFormat: SchemeParserASTFormat,
  locStart: (node) => node.range.begin.idx,
  locEnd: (node) => node.range.end.idx,
}

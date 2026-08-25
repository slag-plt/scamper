import { layoutToString, progToNode, stmtToLayout } from './ast.js'
import { attachComments, collectComments } from './comments.js'
import { tokenizeAndParse } from './index.js'
import { PRINT_WIDTH } from './style.js'

/**
 * Reformatting a whole file: what the editor's reformat command runs.
 *
 * Parses `src`, ornaments the AST with the comments the compiler discards (see
 * comments.ts), and lays each statement out with the printer the output and
 * step panes use. So the reformat command, the panes, and the editor's
 * indenter all follow the one rule table in style.ts, and cannot disagree.
 *
 * @param width the column at which to break lines
 * @returns the reformatted source, without a trailing newline
 * @throws if `src` does not parse -- the caller decides what to do about that
 */
export function formatSource(src: string, width = PRINT_WIDTH): string {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) {
    throw new Error(diagnostics.map((d) => d.message).join('; '))
  }
  const root = progToNode(program)
  attachComments(root, collectComments(src))
  const lines = program.map((s) => layoutToString(stmtToLayout(s), width))
  // Comments with no statement after them -- those below the last one, or the
  // whole of a comment-only file -- dangle on the program itself.
  if (root.dangling !== undefined) {
    lines.push(...root.dangling.map((c) => c.line))
  }
  return lines.join('\n')
}

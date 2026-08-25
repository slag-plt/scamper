import { layoutToString, progToNode, stmtToLayout, type Stmt } from './ast.js'
import { attachComments, collectComments } from './comments.js'
import { tokenizeAndParse } from './index.js'
import {
  DEFAULT_FORMAT_MODE,
  PRINT_WIDTH,
  type UserFormatMode,
} from './style.js'

/** One run of output, and the source lines it was written on. */
interface Block {
  text: string
  /** The first source line it occupied, its leading comments included. */
  begin: number
  /** The last, its trailing comment included. */
  end: number
}

/** The source lines `s` occupies, the comments attached to it included. */
function stmtLines(s: Stmt): { begin: number; end: number } {
  const lead = s.leading ?? []
  const trail = s.trailing ?? []
  return {
    begin: lead.length > 0 ? lead[0].range.begin.line : s.range.begin.line,
    end:
      trail.length > 0
        ? Math.max(s.range.end.line, trail[trail.length - 1].range.end.line)
        : s.range.end.line,
  }
}

/**
 * Whether `b` follows `a` with no blank line between them.
 *
 * A run of one-line statements the author wrote together -- a block of imports,
 * a few short defines -- reads as one thought, and stays as they left it.
 * Everything else is separated: a statement the printer spread over several
 * lines, one with a comment above it, or one the author had already set apart.
 * That keeps a file from running together into a wall of parentheses without
 * overriding the grouping the author chose.
 *
 * Both tests are on the *formatted* text and the *original* lines, so the
 * result is a fixed point: a packed pair stays one line apart and a separated
 * pair stays two, however often the file is reformatted.
 */
function packs(a: Block, b: Block): boolean {
  return !a.text.includes('\n') && !b.text.includes('\n') && b.begin - a.end <= 1
}

/**
 * Reformatting a whole file: what the editor's reformat command runs.
 *
 * Parses `src`, ornaments the AST with the comments the compiler discards (see
 * comments.ts), and lays each statement out with the printer the output and
 * step panes use. So the reformat command, the panes, and the editor's
 * indenter all follow the one rule table in style.ts, and cannot disagree.
 *
 * Blank lines between statements are this function's own business rather than
 * the printer's -- they are the one piece of spacing the style rules cannot
 * derive, since they mark how the author grouped their file. See {@link packs}.
 *
 * @param width the column at which to break lines
 * @param mode how much of the rules' mandated breaking to apply (see style.ts)
 * @returns the reformatted source, without a trailing newline
 * @throws if `src` does not parse -- the caller decides what to do about that
 */
export function formatSource(
  src: string,
  width = PRINT_WIDTH,
  mode: UserFormatMode = DEFAULT_FORMAT_MODE,
): string {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) {
    throw new Error(diagnostics.map((d) => d.message).join('; '))
  }
  const root = progToNode(program)
  attachComments(root, collectComments(src))
  const blocks: Block[] = program.map((s) => ({
    text: layoutToString(stmtToLayout(s), width, mode),
    ...stmtLines(s),
  }))
  // Comments with no statement after them -- those below the last one, or the
  // whole of a comment-only file -- dangle on the program itself. They are one
  // block, so a run of them stays together instead of being spaced apart.
  const dangling = root.dangling ?? []
  if (dangling.length > 0) {
    blocks.push({
      text: dangling.map((c) => c.line).join('\n'),
      begin: dangling[0].range.begin.line,
      end: dangling[dangling.length - 1].range.end.line,
    })
  }
  return blocks.reduce(
    (acc, b, i) =>
      i === 0
        ? b.text
        : acc + (packs(blocks[i - 1], b) ? '\n' : '\n\n') + b.text,
    '',
  )
}

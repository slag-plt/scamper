// Module comments (issue #411): the block at the top of a file saying what the
// module is *for*, as opposed to what any one of its definitions does.
//
// The syntax needs no new sigil. A `;;;` block directly above a definition is
// that definition's docstring (see lezer-bridge.ts's precedingComments); a
// `;;;` block that is not is free-floating, and the first one in a file is the
// module comment. `;;` stays an ordinary comment for free, since
// commentsToDocComments drops every line that is not `;;; `.
import * as A from '../ast.js'
import { collectComments } from '../comments.js'
import { commentsToDocComments } from './docstring.js'

/** What a module comment says about its module. */
export interface ModuleDoc {
  /** The blurb, as one paragraph -- see {@link parseModuleDocFromComments}. */
  description: string
}

/**
 * Groups `comments` into runs of consecutive lines.
 *
 * The same notion of "a block" that precedingComments walks backwards over: a
 * blank line ends a run, because a comment separated by one belongs to whatever
 * is above it rather than to what follows. Its other rule -- that a comment
 * trailing a line of code annotates that line and joins no run -- is applied by
 * the caller, which drops those before grouping.
 */
function commentRuns(comments: A.Comment[]): A.Comment[][] {
  const runs: A.Comment[][] = []
  let run: A.Comment[] = []
  for (const comment of comments) {
    const last = run.at(-1)
    if (last !== undefined && comment.range.begin.line === last.range.begin.line + 1) {
      run.push(comment)
    } else {
      if (run.length > 0) runs.push(run)
      run = [comment]
    }
  }
  if (run.length > 0) runs.push(run)
  return runs
}

/**
 * Finds the module comment in `src`: the first free-floating `;;;` block.
 *
 * Free-floating means no statement begins on the line directly below the block
 * -- if one does, the block is that statement's docstring and belongs to it,
 * not to the module. A block sharing a line with the end of a statement is a
 * trailing comment on that line and is skipped, as precedingComments skips it.
 *
 * Comments come from {@link collectComments} rather than a second walk of the
 * Lezer tree, so `;` inside a string and `#\;` are not mistaken for one.
 *
 * @param prog the program `src` parsed to, for the statement lines to test
 *        against.
 * @returns the block's comments, or undefined if the file has no such block.
 */
export function moduleCommentOf(
  src: string,
  prog: A.Prog,
): A.Comment[] | undefined {
  // A block directly above one of these belongs to it, not to the module.
  //
  // `define` and `define-export` because they are what takes a docstring (see
  // lezer-bridge.ts, which asks for precedingComments for those two and nothing
  // else). `struct` because it does not take one *yet*: a student who writes a
  // block above a struct is describing that struct, and quietly promoting it to
  // the file's header would be a surprising way to be wrong. Passing over it
  // leaves the door open for struct docstrings later.
  //
  // A header above an `(import ...)` is not in this set: it has nothing to
  // attach to, and a file opening with its imports is exactly where one lands.
  const definitionLines = new Set(
    prog
      .filter(
        (stmt) =>
          stmt.tag === 'define' ||
          stmt.tag === 'defexport' ||
          stmt.tag === 'struct',
      )
      .map((stmt) => stmt.range.begin.line),
  )
  // The lines a statement occupies, so a comment trailing code -- which shares
  // its line with the end of a statement -- can be recognised and skipped.
  const codeLines = new Set<number>()
  for (const stmt of prog) {
    for (let l = stmt.range.begin.line; l <= stmt.range.end.line; l++) {
      codeLines.add(l)
    }
  }
  // Dropped before grouping, not after: a comment trailing a line of code --
  // `(import image) ; shapes` -- annotates that line and belongs to no block,
  // so leaving it in would bridge the blocks either side of it into one. That
  // is the difference between a header being found and being swallowed by the
  // first definition below it.
  const standalone = collectComments(src).filter(
    (comment) => !codeLines.has(comment.range.begin.line),
  )
  for (const run of commentRuns(standalone)) {
    if (definitionLines.has(run[run.length - 1].range.begin.line + 1)) {
      // A definition sits directly below: this block is its docstring.
      continue
    }
    if (commentsToDocComments(run).length > 0) {
      return run
    }
    // A free-floating block of ordinary `;;` comments says nothing about the
    // module, so it is passed over rather than ending the search.
  }
  return undefined
}

/**
 * Parses a module comment's lines into its blurb.
 *
 * Joined with a space and trimmed, exactly as a function's description is (see
 * description.ts), so the two wrap the same way wherever they are shown.
 *
 * Unlike a docstring this cannot fail -- there is no signature or parameter
 * list to get wrong -- so it needs no diagnostic channel.
 *
 * @returns the doc, or undefined when the block carries no `;;; ` line or says
 *          nothing once they are stripped.
 */
export function parseModuleDocFromComments(
  comments: A.Comment[],
): ModuleDoc | undefined {
  const docComments = commentsToDocComments(comments)
  if (docComments.length === 0) return undefined
  const description = docComments
    .map((c) => c.line)
    .join(' ')
    .trim()
  return description.length === 0 ? undefined : { description }
}

/**
 * Reads `src`'s module comment, if it has one.
 *
 * @param prog the program `src` parsed to.
 */
export function moduleDocOf(src: string, prog: A.Prog): ModuleDoc | undefined {
  const comments = moduleCommentOf(src, prog)
  return comments === undefined
    ? undefined
    : parseModuleDocFromComments(comments)
}

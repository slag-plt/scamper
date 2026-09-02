/**
 * How each surface form is laid out, following DrRacket. This table is the
 * single source of truth for formatting: the editor's indenter consumes it to
 * decide how far in a line starts, and the pretty-printer consumes it to decide
 * where line breaks go. Keeping one table is what keeps those two from drifting
 * (see docs/formatting.md).
 */

/** The column width of one indentation step. */
export const INDENT_UNIT = 2

/** The column at which a line is considered too long and must break. */
export const PRINT_WIDTH = 80

/**
 * How much line breaking to do.
 *
 * - `strict`: the shape each rule draws is the shape you get. A `lambda`'s
 *   body, an `if`'s branches, a `let`'s body and both halves of every
 *   `cond`/`match` clause take their own lines however short they are.
 * - `relaxed`: those forms still break, but a clause keeps its guard and its
 *   consequent on one line while the two fit -- as the request's own worked
 *   example writes them.
 * - `flat`: no breaking at all, one line however long. Not something a person
 *   chooses; it is how a form is embedded in an error message and how two
 *   layouts are compared for equality (see `layoutToFlatString`).
 */
export type FormatMode = 'strict' | 'relaxed' | 'flat'

/** The two modes the IDE offers; `flat` is internal. */
export type UserFormatMode = Exclude<FormatMode, 'flat'>

/** What formatting does unless a caller says otherwise. */
export const DEFAULT_FORMAT_MODE: UserFormatMode = 'strict'

/**
 * A form's layout family.
 *
 * - `align`: continuation lines sit under the *first argument*, so
 *   `(f a b)` breaks to `(f a` / `   b)`. This is the default, and `if` is a
 *   plain instance of it -- `(if test` puts the branches at column 4.
 * - `body`: the first `head` arguments stay on the opening line and the rest
 *   is a body indented one unit, as in `(lambda (x)` / `  body)`.
 * - `clauses`: a body form whose parts are bracketed clauses, each of which
 *   splits internally -- `cond` and `match`.
 *
 * `breaks: 'always'` marks the forms whose shape the rules *mandate*. Rules 1,
 * 3, 4, 5 and 6 each give exactly one form, with no alternative and no
 * condition, so those break however short they are. Rules 2 and 7 spell out a
 * one-line alternative and the condition for it, so `define` and every
 * application break only when they must.
 */
export type FormStyle = { breaks?: 'always' } & (
  | { kind: 'align' }
  | { kind: 'body'; head: number }
  | { kind: 'clauses'; head: number }
)

/** The layout of any form not named in {@link FORM_STYLES}: rule 7. */
export const DEFAULT_STYLE: FormStyle = { kind: 'align' }

/**
 * Forms that are *not* laid out by the default rule, keyed by the keyword as it
 * is written in source. `head` counts the arguments that stay on the opening
 * line with the keyword.
 *
 * Anything absent -- `and`, `or`, `display`, `struct`, `import`, `export`, and
 * every user-defined procedure -- gets {@link DEFAULT_STYLE}. None of those is
 * named by the seven rules, so all of them fall under rule 7.
 */
export const FORM_STYLES: Record<string, FormStyle> = {
  // Rule 1. (lambda (x y)
  //           body)
  lambda: { kind: 'body', head: 1, breaks: 'always' },
  // Rule 2, the one body form with a one-line alternative: (define id expr)
  // while it fits, (define id\n  expr) once it does not.
  define: { kind: 'body', head: 1 },
  'define-export': { kind: 'body', head: 1 },
  // Rule 3. `if` is an aligned form, not a body one: its branches sit under the
  // test, at column 4, rather than at +2.
  if: { kind: 'align', breaks: 'always' },
  // Rule 4. (let ([a 1]
  //               [b 2])
  //           body)
  let: { kind: 'body', head: 1, breaks: 'always' },
  // Rules 5 and 6.  (cond            (match e
  //                    [guard           [pat
  //                     consequent])     body])
  cond: { kind: 'clauses', head: 0, breaks: 'always' },
  match: { kind: 'clauses', head: 1, breaks: 'always' },
  // Not one of the seven. DrRacket indents a begin's body rather than aligning
  // it under the first expression, so it is a body form holding nothing on the
  // opening line -- but it breaks only when it must.
  begin: { kind: 'body', head: 0 },
}

/** The layout for `keyword`, falling back to {@link DEFAULT_STYLE}. */
export function styleOf(keyword: string | undefined): FormStyle {
  return keyword === undefined
    ? DEFAULT_STYLE
    : (FORM_STYLES[keyword] ?? DEFAULT_STYLE)
}

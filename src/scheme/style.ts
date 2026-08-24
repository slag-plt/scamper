/**
 * How each surface form is laid out, following DrRacket. This table is the
 * single source of truth for formatting: the editor's indenter consumes it to
 * decide how far in a line starts, and the pretty-printer consumes it to decide
 * where line breaks go. Keeping one table is what keeps those two from drifting
 * (see FORMATTING.md).
 */

/** The column width of one indentation step. */
export const INDENT_UNIT = 2

/** The column at which a line is considered too long and must break. */
export const PRINT_WIDTH = 80

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
 */
export type FormStyle =
  | { kind: 'align' }
  | { kind: 'body'; head: number }
  | { kind: 'clauses'; head: number }

/** The layout of any form not named in {@link FORM_STYLES}: rule 7. */
export const DEFAULT_STYLE: FormStyle = { kind: 'align' }

/**
 * Forms that are *not* laid out by the default rule, keyed by the keyword as it
 * is written in source. `head` counts the arguments that stay on the opening
 * line with the keyword.
 *
 * Anything absent -- `if`, `and`, `or`, `display`, `struct`, `import`,
 * `export`, and every user-defined procedure -- gets {@link DEFAULT_STYLE}.
 */
export const FORM_STYLES: Record<string, FormStyle> = {
  // (lambda (x y)     (define id      (let ([a 1])
  //   body)             expression)     body)
  lambda: { kind: 'body', head: 1 },
  define: { kind: 'body', head: 1 },
  'define-export': { kind: 'body', head: 1 },
  let: { kind: 'body', head: 1 },
  // DrRacket indents a begin's body rather than aligning it under the first
  // expression, so it is a body form with nothing held on the opening line.
  begin: { kind: 'body', head: 0 },
  // (cond            (match e
  //   [guard           [pat
  //    consequent])     body])
  cond: { kind: 'clauses', head: 0 },
  match: { kind: 'clauses', head: 1 },
}

/** The layout for `keyword`, falling back to {@link DEFAULT_STYLE}. */
export function styleOf(keyword: string | undefined): FormStyle {
  return keyword === undefined
    ? DEFAULT_STYLE
    : (FORM_STYLES[keyword] ?? DEFAULT_STYLE)
}

# Formatting and indentation

Scamper formats code by DrRacket's rules, in the editor and in the output and step panes.
One rule table drives all of it.

| Behaviour | Implemented by |
|---|---|
| The rule table itself | `src/scheme/style.ts` |
| Enter indents the next line | `src/app/web/codemirror/extensions/indentation.ts` |
| Ctrl-I re-indents the buffer | same file, via `indentRange` over the document |
| Ctrl-Shift-I reformats the buffer | `src/scheme/format.ts` |
| Output and step panes | `src/scheme/pretty.ts`, `LayoutRenderer.vue` |

Typing a closing bracket snaps the line into place (`indentOnInput`), and Tab re-indents the selection rather than inserting an indent unit.

Ctrl-I and Ctrl-Shift-I are different verbs.
Ctrl-I re-indents: it changes leading whitespace only and leaves the author's line breaks alone, which is DrRacket's semantics.
Ctrl-Shift-I reformats, choosing the line breaks itself.

## The style rules

The rules fall into two families:

### Family 1 — body forms: fixed head, body indented two spaces

```scheme
(lambda (x y)                    (define id
  body)                            expression)

(let ([name1 exp1]               (cond
      [name2 exp2]                 [guard1
      [name3 exp3])                 consequent1]
  body)                            [guard2
                                    consequent2])
```

`define` also has a one-line form, `(define id expression)`, used when the whole statement fits in 80 columns and the expression is a single line.

`let` is the awkward member: its binding list opens on the `let` line, so the second and subsequent bindings align at column 6, one past `(let (`.

`cond` is the other: each clause sits at +2 and the consequent at +3, one past the clause's own `[`.

### Family 2 — aligned forms: continuation lines under the first argument

```scheme
(fun arg1 arg2 arg3)             (if test
                                     consequent
(fun arg1                            alternate)
     arg2
     arg3)
```

`if` and every other application are the same rule: continuation lines align to the column of the first argument, `1 + len(head) + 1` past the open paren, which for `if` is exactly 4.
Arguments are all on one line or all on separate lines, never packed greedily.

Both families break when the form exceeds 80 columns, or when any subexpression is itself multi-line.

### Forced breaks

`lambda`, `if`, `let`, `cond`, and `match` each draw exactly one shape and offer no one-line alternative, so they break however short they are.
`define` and application spell out a one-line form and the condition for it, so they do not.

A form broken this way is no wider than one that is not, so an enclosing form cannot infer the break from a width.
`pretty.ts` propagates it outwards through `containsForcedBreak`, which is what carries `(define f (lambda (x) x))` onto the multi-line `define` shape.

## Two modes

Scamper offers a strict and a relaxed version of the formatting rules:

| | `strict` (the default) | `relaxed` |
|---|---|---|
| `lambda`, `if`, `let`, `cond`, `match` | break | break |
| a `let`'s binding list | stacks, one per line | stacks, one per line |
| a `cond`/`match` clause | `[guard` / ` consequent]`, always | one line while it fits |

```scheme
;; strict                        ;; relaxed
(match l                         (match l
  [null                            [null 0]
   0]                              [(cons _ tail) (+ 1 (f tail))])
  [(cons _ tail)
   (+ 1 (f tail))])
```

A binding list is the counterpart of a clause *list*, not of a clause.
Rule 4 draws each `[name exp]` whole and stacks them, so the list breaks and the binding does not; a lone binding shows no break, having nothing to put on a second line.

```scheme
(let ([a 1]                      ; the list stacks, however short
      [b (f 2)])                 ; each binding stays whole
  body)
```

`expToString` and its siblings stay flat in both modes.
They are the canonical one-line form: what goes into an error message, what the trace deduplicates on, and what the printer measures a value's width with.
What gets displayed is `layoutToString` over the same layout.

## The console

`scamper --trace` breaks a step by the same rules, with one addition.
The console writes each step behind a `--> ` marker, so the step is laid out as beginning after it.
`renderToString` takes the starting column, every planned column is absolute, and the marker's width reaches the printer through an optional `col` on `TextRenderer.render`.
An `if`'s branches therefore sit under its test rather than under the marker, and eighty columns still means the finished line.

```
--> (if (= 3 0)
        1                      <- column 8, under "(= 3 0)"
        (* 3 (fact (- 3 1))))
```

The web trace needs none of this: it draws from a plan rather than a string and gives each step its own container, which is also why it carries no marker.

## The anti-drift invariant

`indentRange(format(p)) === format(p)`: re-indenting formatted output must leave it unchanged, or the editor and the panes would disagree about the same code.
`test/apps/web/format-indent-agreement.test.ts` asserts it over seventeen programs, for both the printer and the reformat command, in both modes, with no exceptions.
The same test checks that the two produce byte-identical output.

`INDENT_UNIT` in `style.ts` is also CodeMirror's `indentUnit` facet, which is what keeps the two sides measuring in the same units.

## Implementation notes

+   **Comments live on the layout.**
    The AST has carried leading, trailing, and dangling comments since #304; `expToLayout` copies them onto the layout and `pretty.ts` emits them.
    A comment forces its enclosing form to break, since a line comment runs to the end of its line.
    A trailing comment is held back and written just before its line ends, wherever that turns out to be.
+   **A map literal's pairs are one `unit` each**, a layout that never breaks apart, so a wrapped map never ends a line with a key whose value is on the next.
+   **Blank lines between statements belong to `format.ts`, not the printer.**
    They are the one piece of spacing the style rules cannot derive, because they record how the author grouped the file.
    A run of one-line statements written together — a block of imports, a few short defines — stays as it was left.
    Everything else is separated by one blank line: a statement the printer spread over several lines, one with a comment above it, or one the author had already set apart.
    A wider gap collapses to one.
+   **A comment block keeps its own gaps** the same way (#333).
    `commentLines` (`src/scheme/ast.ts`) reads them off each `Comment`'s range and marks one with an empty entry in the layout's comment list.
    `pretty.ts` pays a line's indentation only when something is written on it, so such a line comes out bare; a padded blank line would be trailing whitespace, which the indenter strips and which would break the invariant above.
+   **Formatting adds vertical space in one case.**
    Two adjacent multi-line statements written with no blank line between them gain one, since `packs` keeps only a run of one-liners together.
+   **The grammar's clause rules are named.**
    `binding`, `condBranch`, `branch`, and `arglist` were lowercase, so Lezer emitted no nodes for them, which is why `language.ts`'s `binding:` indent entry never did anything.
    They are now `Binding`, `CondClause`, `MatchClause`, `Bindings`, `ArgList`, and `FieldList`.
    The accepted language is unchanged; `lezer-bridge.ts` descends through them instead of relying on positional flattening.


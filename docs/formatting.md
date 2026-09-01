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

The rules fall into two families, which is how DrRacket organises them too.

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

Rule 5 says "each guard and the corresponding consequent should be on separate lines", while the request's own worked example writes `[null 0]` on one line.
Both readings are available under **Edit > Relaxed Formatting**:

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

The setting is read in three places, so a file and a trace cannot disagree: the reformat command reads it, and the output and step panes read it through `FormatModeKey` (`src/scheme/ast-components/format-mode.ts`).
The editor's indenter ignores it, since Ctrl-I decides where a line starts and never where a break goes, so both modes indent identically.

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

## Behaviour worth knowing

+ **Comments live on the layout.**
  The AST has carried leading, trailing, and dangling comments since #304; `expToLayout` copies them onto the layout and `pretty.ts` emits them.
  A comment forces its enclosing form to break, since a line comment runs to the end of its line.
  A trailing comment is held back and written just before its line ends, wherever that turns out to be.
+ **A map literal's pairs are one `unit` each**, a layout that never breaks apart, so a wrapped map never ends a line with a key whose value is on the next.
+ **Blank lines between statements belong to `format.ts`, not the printer.**
  They are the one piece of spacing the style rules cannot derive, because they record how the author grouped the file.
  A run of one-line statements written together — a block of imports, a few short defines — stays as it was left.
  Everything else is separated by one blank line: a statement the printer spread over several lines, one with a comment above it, or one the author had already set apart.
  A wider gap collapses to one.
+ **A comment block keeps its own gaps** the same way (#333).
  `commentLines` (`src/scheme/ast.ts`) reads them off each `Comment`'s range and marks one with an empty entry in the layout's comment list.
  `pretty.ts` pays a line's indentation only when something is written on it, so such a line comes out bare; a padded blank line would be trailing whitespace, which the indenter strips and which would break the invariant above.
+ **Formatting adds vertical space in one case.**
  Two adjacent multi-line statements written with no blank line between them gain one, since `packs` keeps only a run of one-liners together.
+ **The grammar's clause rules are named.**
  `binding`, `condBranch`, `branch`, and `arglist` were lowercase, so Lezer emitted no nodes for them, which is why `language.ts`'s `binding:` indent entry never did anything.
  They are now `Binding`, `CondClause`, `MatchClause`, `Bindings`, `ArgList`, and `FieldList`.
  The accepted language is unchanged; `lezer-bridge.ts` descends through them instead of relying on positional flattening.

## Decisions

These are settled.
Revisiting one is a small change to `src/scheme/style.ts` and its tests.

+ **`match` follows `cond`**, with the scrutinee on the `match` line.
+ **Every form `FORM_STYLES` does not name is an application.**
  The set is closed: the grammar names each special form, and there is no `let*`, `letrec`, `when`, or `unless`.
  `and`, `or`, `struct`, `import`, and `export` are applications and hardly ever break.
  `display` is one too; it takes a single argument, so it breaks only when that argument is multi-line, aligning it at column 9.
  DrRacket has no special rule for `display`, which is the point of following DrRacket.
  `test/scheme/pretty.test.ts` pins it.
+ **`{...}` pairs lay out as units**, so a break falls between pairs and never between a key and its value; the pairs align under the first.
+ **`[...]` and `#(...)`** are applications over their elements, and over the inner form past the `#`.
+ **Source captions are not formatted.**
  `SourceCaption.vue` shows a statement's source exactly as the student wrote it: a caption cites their code rather than rendering it.
+ **Arguments are never packed greedily**, so there is no `fill`.
  All-on-one-line or all-on-separate-lines is what makes an argument list scannable.
  The cost is real — `(list 1 2 ... 30)` becomes thirty lines — but the alternative is a second break policy beside the one table, keyed on which heads are "data-like", which is the special-casing the one-table design exists to avoid.
  Reopen it with a student file that reads badly.
+ **No width or indent preference.**
  `PRINT_WIDTH` and `INDENT_UNIT` stay constants in `style.ts`; the setting people asked for is Edit > Relaxed Formatting.
  A preference would have to reach the indenter too, and the anti-drift invariant would want running at every width.
  `formatSource` and `planLayout` already take a `width`, so the plumbing survives if it is ever wanted.
+ **Numeric spelling is not preserved.**
  A `lit` renders from its runtime value, so `1.50` reformats to `1.5` and `1e3` to `1000`.
  No meaning changes — Scamper numbers are plain JS numbers, with no exact/inexact distinction — and every spelling re-reads.
  Preserving it would mean an optional `source` on `Lit`/`PLit` threaded from the reader, a field on the AST that only the printer reads and every later pass has to carry.
  Reopen it at the same bar as the fill rule above.

## Why not Prettier

The three deliverables need three different kinds of engine, and Prettier serves only one.

| | What it needs | Prettier |
|---|---|---|
| Enter | A synchronous, incremental, error-tolerant indent query for one position, on a document that is usually unparseable mid-typing | **No.** Prettier is async and throws on a parse failure. CodeMirror's indent hooks are synchronous and must work on broken input. This is Lezer and `indentNodeProp` work. |
| Ctrl-I | A pure re-indent of leading whitespace | **Not needed.** `indentRange(state, 0, doc.length)` gives it once Enter-indent exists. |
| Panes | Line-broken rendering into DOM, keeping `ValueRenderer` at `val` leaves, the `scamper-hl-*` classes per token, and the `trace-changed` wrapper `changedLayoutPath` locates | **No.** Prettier's printer emits a string, which loses all three. |

Since two of the three were greenfield either way, the implementation is one style spec (`style.ts`) feeding one column-aware printer (`pretty.ts`) with two backends, text and DOM, plus a Lezer indenter driven by the same table.

`src/prettier/` and the runtime Prettier dependency are gone.
Removing them took the IDE bundle from 399.82 kB to 305.94 kB, or 134.70 to 100.60 kB gzipped.
Prettier remains a devDependency, where it formats this repository's TypeScript.

_(Co-created with Claude Code)_

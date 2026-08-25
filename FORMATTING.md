# Formatting and indentation: target style and implementation plan

A scoping document for the DrRacket-style formatting request. It records the
target style, measures the gap that existed before the work, scopes the two
candidate implementations, and recommends one.

**Plan B is now implemented** -- all three stages, in full. What shipped is
summarised in [As built](#as-built); the sections after it are the reasoning
that led there, kept because the trade-offs still explain why the code is
shaped as it is. All five [open questions](#open-questions) have since been
answered, and the answers are what the code does.

## As built

| | Delivered by | Where |
|---|---|---|
| **(a)** Enter indents the next line | Per-form Lezer strategies; Enter was already wired to `insertNewlineAndIndent` | `src/app/web/codemirror/extensions/indentation.ts` |
| **(b)** Ctrl-I re-indents the buffer | `indentRange` over the whole document -- DrRacket semantics, line breaks untouched | same file |
| **(c)** Output and step panes | A column-aware printer producing a plan both backends render from | `src/scheme/pretty.ts`, `LayoutRenderer.vue` |
| Reformat (Ctrl-Shift-I) | The same printer, over a program parsed from the buffer | `src/scheme/format.ts` |
| The rules themselves | One table, read by all three | `src/scheme/style.ts` |

Also in: typing a closing bracket snaps the line into place (`indentOnInput`,
previously installed but inert), and Tab re-indents the selection rather than
adding an indent unit.

The anti-drift invariant of §7 is real and enforced:
`test/apps/web/format-indent-agreement.test.ts` asserts
`indentRange(format(p)) === format(p)` over seventeen programs, for both the
printer and the reformat command, with no exceptions -- the two now produce
byte-identical output, which the same test also checks.

Two things worth knowing:

- **The grammar's clause rules are now named.** `binding`, `condBranch`,
  `branch` and `arglist` were lowercase, so Lezer emitted no nodes for them --
  which is why `language.ts`'s `binding:` indent entry had never done anything.
  They are now `Binding`, `CondClause`, `MatchClause`, `Bindings`, `ArgList` and
  `FieldList`. The accepted language is unchanged; `lezer-bridge.ts` descends
  through them instead of relying on positional flattening.
- **`Layout` carries comments, and the Prettier printer is gone.** The AST has
  held leading/trailing/dangling comments since #304; `expToLayout` and its
  siblings now copy them onto the layout, and `pretty.ts` emits them. A comment
  forces its enclosing form to break, since a line comment runs to the end of
  its line; a trailing comment is held back and written just before its line
  ends, wherever that turns out to be. That removed the last reason to keep
  `src/prettier/`, and with it the runtime Prettier dependency: the IDE bundle
  drops from 399.10 kB to 304.83 kB, or 134.47 to 100.11 kB gzipped. Prettier
  stays a *devDependency*, which is a separate job -- it formats this
  repository's own TypeScript.
- **A map literal's pairs are one `unit` each**, a layout that never breaks
  apart, so a wrapped map never ends a line with a key whose value is on the
  next one. The panes used to; only the Prettier printer got this right.

## 1. What was asked

Three user-visible behaviours:

- **(a)** Pressing Enter indents the next line correctly for the enclosing form.
- **(b)** Ctrl-I re-indents the whole editor buffer.
- **(c)** The output pane and step pane format their contents by the same rules.

And seven style rules, reproduced in §2.

## 2. The target style

The seven rules collapse into two families, which is also how DrRacket itself is
organised. That dichotomy is the natural shape for a rule table.

### Family 1 — "body" forms: fixed head, body indented two spaces

```scheme
(lambda (x y)                    (define id
  body)                            expression)

(let ([name1 exp1]               (cond
      [name2 exp2]                 [guard1
      [name3 exp3])                 consequent1]
  body)                            [guard2
                                    consequent2])
```

`define` also has a one-line form, `(define id expression)`, used when the whole
statement fits in 80 columns and the expression is a single line.

`let` is the awkward member: its binding list opens on the `let` line, so the
second and subsequent bindings align at column 6 — one past `(let (`.

`cond` is the other: each clause sits at +2, and the consequent sits at +3, one
past the clause's own `[`.

### Family 2 — "aligned" forms: continuation lines under the first argument

```scheme
(fun arg1 arg2 arg3)             (if test
                                     consequent
(fun arg1                            alternate)
     arg2
     arg3)
```

Rules 3 (`if`) and 7 (every other application) are the *same rule*: continuation
lines align to the column of the first argument, which is `1 + len(head) + 1`
past the open paren. For `if` that is exactly 4. Arguments are all on one line or
all on separate lines — never packed greedily.

Both families break when the form exceeds 80 columns or when any subexpression
is itself multi-line.

## 3. Where things stood before

| Area | File | Size | Before |
|---|---|---|---|
| Formatter | `src/prettier/` (3 files) | ~355 lines | Prettier plugin, bound to `Mod-Shift-i`, 56 tests in `test/prettier/printer.test.ts` |
| Enter-indent | `src/app/web/codemirror/extensions/language.ts` | 54 lines | `continuedIndent({units: 1})` on 11 node types — a flat 2 spaces for everything |
| Step/trace pane | `Layout` in `src/scheme/ast.ts`; `layoutToString` (text) and `LayoutRenderer.vue` (DOM) | ~150 lines | **No line breaking at all.** Every form renders on one space-separated line |
| Output pane | `OutputPane.vue` + `SourceCaption.vue` | — | Renders *values*, plus the verbatim source text of the statement that produced them |

### Measured gap

Running the formatter as it was on the request's own examples:

| Rule | Target | Today's output | |
|---|---|---|---|
| 2 `define` | `(define id\n  expr)` | `(define id\n  expr)` | ✅ |
| 1 `lambda` | body at +2 | body at +2 when it breaks | ✅ modulo Q3 |
| 3 `if` | consequent/alternate at **+4** | `(if test\n  cons\n  alt)` — at +2 | ✗ width |
| 4 `let` | `(let ([b1]\n      [b2])\n  body)` | `(let\n  ([b1] [b2])\n  body)` | ✗ shape |
| 5 `cond` | `[guard\n consequent]`, +2 / +3 | `[guard consequent]` kept inline at +2 | ✗ never splits |
| 6 `match` | *(unspecified — see Q2)* | scrutinee inline, clauses at +2 | ? |
| 7 application | aligned under first argument | `(fun\n  arg1\n  arg2)` — at +2 | ✗ alignment |

So one rule is already right, one is close, four are wrong, and one is unknown.
The wrong ones are all *indentation-width* errors rather than structural ones —
which is why the printer rewrite in either plan is modest.

## 4. The structural finding

The three deliverables need three different **kinds** of engine, and Prettier can
serve only one of them.

| | What it actually needs | Prettier? |
|---|---|---|
| **(a)** Enter | A **synchronous**, incremental, **error-tolerant** indent query for one position, on a document that is usually *unparseable* mid-typing (unbalanced parens) | **No.** Prettier is async (`format` returns a Promise) and throws on a parse failure — today's call site swallows it with `.catch(() => {})`. CodeMirror's indent hooks are synchronous and must work on broken input. This is Lezer/`indentNodeProp` work. |
| **(b)** Ctrl-I | Either a pure re-indent (leading whitespace only — DrRacket's actual semantics) or a full reflow | **Yes**, for the reflow reading. For the re-indent reading, `indentRange(state, 0, doc.length)` — or the ready-made `indentSelection` command — gives it for free once (a) exists. About six lines; see §6. |
| **(c)** Panes | Line-broken rendering into **DOM**, keeping `ValueRenderer` at `val` leaves (images, lists, and pairs are components, not text), the `scamper-hl-*` classes per token, and the `trace-changed` wrapper that `changedLayoutPath` locates | **No.** Prettier's printer emits a string; a string loses all three. |

**Prettier addresses at most one of the three asks.** The other two are greenfield
under either plan. That single fact drives the recommendation.

## 5. Plan A — stay in the Prettier ecosystem

### A1. Rewrite `src/prettier/scheme/printer.ts` (~150 lines changed)

All seven rules *are* expressible in Prettier's doc algebra. The builder that does
the work is `align(n, doc)`, which is already exported (`prettier/doc.d.ts:113`).

- `if` → `group(['(if ', guard, align(4, [line, ifB, line, elseB]), ')'])`
- application → `align(2 + headWidth, …)`
- `let` → `['(let (', align(6, join(line, bindings)), ')', indent([hardline, body]), ')']`
- `cond` → clauses at `indent(2)`, each clause `['[', test, align(1, [hardline, body]), ']']`
- `lambda`, `define` → already correct

Two caveats, both real:

1. **Prettier's indentation is a virtual stack, not a column.** `align(n)` means
   "current indent + n", not "current output column + n". Those coincide only
   while every form's `align` equals the literal width of its head — an invariant
   that holds if you build every case carefully and breaks *silently* if one case
   is written differently. DrRacket, by contrast, measures the real column.
2. **Non-atomic heads cannot be aligned.** For `((compose f g) x y)` the width of
   the head is not known when the doc is built, so the rule degrades to `indent(2)`.
   Rare in student code, but a permanent divergence from the spec.

### A2. Rebind the command (~5 lines)

`Mod-Shift-i` → `Ctrl-i` in `extensions/prettier.ts`; add it to `ShortcutsHelp.vue`.
No conflict with `indentWithTab` (Tab) or browser defaults.

### A3. Write the Lezer indenter for (a) — *no reuse from A1* (~110 lines)

Replace the eleven `continuedIndent` entries in `language.ts` with per-form
strategies, using `TreeIndentContext.column(pos)` for the aligned family.

### A4. Write a line-breaking engine over `Layout` for (c) — *no reuse from A1 or A3* (~250 lines)

`layoutToString` and `LayoutRenderer.vue` both need line breaking, and the DOM one
cannot consume a string. The comment already in `ast.ts` anticipates this: the
`Layout` type "is the natural place to later hang line-breaking/indentation for
traces."

### Net

**Three independent implementations of one style spec**: the Prettier printer, the
Lezer indent props, and the `Layout` breaker.

### Risks

- **Drift between the three.** This has already bitten this codebase once: the
  text and web renderings of `Layout` were "previously hand-synced" (#318), and
  `test/regressions/let-trace-formatting.test.ts` exists because they diverged.
  A third rule engine invites the same failure with no mechanical guard.
- The virtual-indent invariant above, which no test naturally catches.
- Prettier stays in the IDE bundle: **97 kB raw / 35 kB gzipped**, measured by
  building `dist/` with the extension stubbed (`scamper-ide` chunk: 398.89 kB →
  302.12 kB, gzip 134.31 → 99.16 kB).

## 6. What CodeMirror already does

This applies to **both** plans — the indenter for (a) is the same work either way,
so none of it shifts the A-vs-B decision. What it does do is make Stage 1 of §9
much smaller and mostly *configuration of well-tested machinery*, which is why it
should ship first.

CodeMirror draws a sharp line, and it is exactly the line Plan B draws:

> **CodeMirror answers "how far in does this line start?"**
> **A printer answers "where do the line breaks go?"**

Those are the two halves of formatting. CodeMirror implements the first one
completely, and has **nothing whatsoever** for the second — no `printWidth`, no
reflow, no formatter of any kind in core. Which is why (c) is ours regardless.

### What we get for free

| Concern | Provided by |
|---|---|
| Enter inserts a break *and* indents | `insertNewlineAndIndent`, **already bound** in `defaultKeymap` |
| "What would the indent be if a break were here?" | `IndentContext({ simulateBreak, simulateDoubleBreak })` |
| Error tolerance on half-typed, unbalanced input | Lezer's incremental error-tolerant tree; a strategy just walks it |
| Re-indent a range, **cascading** — line *n* sees line *n−1*'s **new** indent | `indentRange` + its `overrideIndentation` option |
| Re-indent the selection | `indentSelection` command |
| Minimal `ChangeSet` — only lines whose indent actually moved | both of the above |
| Blank lines normalized to column 0 | both of the above |
| Tabs vs. spaces, column arithmetic | `indentString`, `IndentContext.column`, `countColumn`, the `indentUnit` facet (2 spaces by default — already our target) |
| Falling back to the enclosing form | `TreeIndentContext.continue()` |

So **(b) is a keymap entry pointing at a command that already exists** — about six
lines, not the ten estimated earlier. And **(a) needs no command and no keybinding
at all**: Enter is already wired to `insertNewlineAndIndent`, which already asks
the language for an indent. Getting (a) right is *entirely* a matter of replacing
the eleven `continuedIndent` entries with real per-form strategies.

That drops the indenter from ~150 lines to **~110** — we write the strategies, not
the traversal, the change computation, the whitespace generation, or the
error handling.

### Two wins available almost for free

1. **`indentOnInput()` is installed but inert.** It is in the extension list in
   `codemirror.ts`, but it bails immediately unless the language supplies an
   `indentOnInput` regex — and `ScamperLanguage.languageData` has only
   `commentTokens`. Adding `indentOnInput: /^\s*[)\]}]$/` makes a line snap to its
   correct indent the moment you type the closing bracket. **One line**, and it is
   DrRacket behaviour we are currently shipping the machinery for and not using.
2. **Tab is bound to `indentMore`, which is the wrong verb.** `indentMore` adds an
   indent unit. In DrRacket, Tab *re-indents the current line* — that is the muscle
   memory this request is really about. Rebinding Tab to `indentSelection` gives
   exactly that, in **one line**, and makes Ctrl-I simply "the whole-buffer version
   of Tab" rather than an unrelated command.

### One thing that does *not* work off the shelf

`delimitedIndent({ align: true })` looks like it should give us Family 2, and does
not. Its helper `bracketedAligned` aligns to just past the **opening bracket**, so
for `(f a b)` continuation lines land under `f` — the Common Lisp convention —
where the request wants them under `a`. We need a custom strategy:

```ts
const alignToFirstArg = (cx: TreeIndentContext) => {
  const arg = firstArgumentOf(cx.node)          // skip "(" and the head
  return arg && arg.from < lineEndOrBreak(cx)   // …and it's on the opening line
    ? cx.column(arg.from)                       // Family 2
    : cx.baseIndent + cx.unit                   // nothing to align to yet
}
```

About fifteen lines, and `bracketedAligned` is a readable model to copy — including
its handling of the case where the first argument is *past* the simulated break,
which is what happens when you press Enter directly after `(f`.

## 7. Plan B — one style spec, one engine, two backends

### B1. `src/scheme/style.ts` — the rule table, single source of truth (~40 lines)

```ts
export type FormStyle =
  | { kind: 'align' }                  // continuation at the first-argument column
  | { kind: 'body'; head: number }     // `head` args stay on line 1; body at +2
  | { kind: 'clauses'; head: number }  // clauses at +2, split inside each clause

export const FORM_STYLES: Record<string, FormStyle> = {
  lambda: { kind: 'body', head: 1 },
  define: { kind: 'body', head: 1 },
  let:    { kind: 'body', head: 1 },   // plus aligned binding list
  cond:   { kind: 'clauses', head: 0 },
  match:  { kind: 'clauses', head: 1 },
  // everything else, `if` included: { kind: 'align' }
}
```

### B2. `src/scheme/pretty.ts` — a column-aware pretty printer (~200 lines)

A standard Wadler/Lindig engine over `text | line | concat | nest | alignHere |
group`, with one deliberate addition: **`alignHere` means "indent subsequent lines
to the current output column."** That is what Lisp alignment actually is, and it
removes both of Plan A's caveats at once — non-atomic heads align correctly, and
there is no virtual-indent invariant to preserve by hand.

### B3. Two backends over the same doc (~120 lines)

- `renderToString(doc, 80)` → replaces `layoutToString`
- `renderToLines(doc, 80)` → `{ indent: number, items: Layout[] }[]`, so
  `LayoutRenderer.vue` keeps `ValueRenderer` at `val` leaves and the
  `changedLayoutPath` highlight still resolves

### B4. Tag `Layout` groups with their form (~20 lines)

`group` gains `form?: string` so the printer can consult `FORM_STYLES` without
string-matching the head token.

### B5. Repoint the Prettier plugin (~20 lines)

Keep the plugin shell; replace the printer body with `renderToString`. All 56
existing tests keep running against the same public `format()` API, so this is a
refactor with a regression suite already in place.

### B6. `scamper-indent.ts` — the Lezer indenter, driven by `style.ts` (~110 lines)

Then **(b) is six lines**: a keymap entry on `indentSelection` (§6), or
`view.dispatch({ changes: indentRange(state, 0, doc.length) })`.
That is DrRacket's Ctrl-I semantics exactly.

### B7. The anti-drift invariant (~30 lines)

```
for each sample program p:  indentRange(format(p)) === format(p)
```

**The pretty-printer's output must be a fixed point of the editor's indenter.**
One property test mechanically keeps (a), (b) and (c) in agreement forever.

This invariant is *not statable* under Plan A: its Prettier printer and its Lezer
indenter share no rules, and its trace-pane breaker is a third thing again.

### Net

**One rule table, two rule-consuming engines** — and the second is derived from
the table and cross-checked against the first.

### Risks

- We own the group-fits algorithm (~100 lines). It is well-understood and
  thoroughly documented in the literature, and Scamper is maintained by
  undergraduates, for whom a 200-line printer they can read is arguably a better
  artefact than a Prettier plugin whose behaviour emerges from doc-builder
  interactions.
- Two backends over one doc is more indirection than `layoutToString` has today.

## 8. Comparison

| | Plan A (Prettier) | Plan B (own printer) |
|---|---|---|
| Rule engines to maintain | **3** | **2**, from 1 shared table |
| Total new/changed code | ~515 lines | ~535 lines |
| Effort | ~4.5 days | ~5 days |
| Accuracy | Virtual indent ≈ column; non-atomic heads unsupported | Real columns; no exceptions |
| Adding a form | Edit 3 places, no cross-check | 1 table entry + 1 Lezer node name, cross-checked by B7 |
| Extension (e.g. per-user width, `let*`, user macros) | Per-engine | Table-level |
| Drift risk | High, unguarded — and this codebase has been bitten before | Guarded by the B7 fixed-point test |
| Bundle | Keeps 35 kB gzip of Prettier | Can drop it |
| Prior art leverage | Prettier's `fits` algorithm is battle-tested | Reimplemented, ~100 lines |

The costs are within noise of each other. **The difference is not what it costs to
build — it is what you own afterwards.**

## 9. Recommendation

**Plan B, staged.** Prettier can serve at most one of the three asks, so choosing
it means writing the style rules three times instead of once, for the same effort.

Ship in three independent stages, each of which is useful alone:

1. **Stage 1 — `style.ts` + the Lezer indenter + `indentSelection`.** Delivers
   **(a)** and **(b)**. CodeMirror hosts essentially all of it (§6), so this is
   mostly configuration of well-tested machinery: no new commands for (a), one
   keymap entry for (b), and the two free wins — a live `indentOnInput` regex and
   Tab re-indenting the line the way DrRacket's does. Touches nothing else in the
   codebase, and it is what a student notices every time they press Enter.
2. **Stage 2 — `pretty.ts` + the two backends.** Delivers **(c)**, and the reflow
   reading of (b) if that is what is wanted (Q1).
3. **Stage 3 — repoint the Prettier printer at `pretty.ts`,** then decide whether
   to keep Prettier in the web bundle at all.

*As built, stage 3 is complete.* It went half-way first -- the Prettier printer
was repointed at `style.ts`, so there was one rule table rather than two sets of
rules, but it still built Prettier docs. Modelling comments in `Layout` is what
finished it: `src/scheme/format.ts` parses the buffer, ornaments the AST with
its comments, and hands the result to `pretty.ts`, so the reformat command and
the panes are now the same printer. `src/prettier/` is deleted, the estimated
~35 kB gzip came off the bundle (34.4 kB, measured), and the two shapes the
invariant test had excused now agree exactly.

If Q1 answers "re-indent, not reflow", Stage 1 alone satisfies both (a) and (b),
and Stage 2 is needed only for the panes.

## Open questions

All five are answered. The answers are what the code does; revisiting one is a
small change to `src/scheme/style.ts` and its tests.

1. **Does Ctrl-I re-indent or reflow?** *Re-indent* — DrRacket's semantics:
   leading whitespace only, the author's line breaks left alone. Reflowing is a
   separate verb, on Ctrl-Shift-I.
2. **Rule 6 (`match`), truncated in the request.** *Like `cond`, with the
   scrutinee on the `match` line* — `(match e\n  [p1\n   b1]\n  …)`.
3. **Do rules 1, 3 and 5 force breaks, or only fix the indent when a break is
   needed?** *Only fix the indent.* Nothing forces a break; a form breaks on the
   80-column or multi-line trigger and not otherwise, so `(lambda (x) x)` and
   `(if a b c)` stay on one line.
4. **Which family do Scamper's non-Racket forms belong to?** *Rule 7 — every
   form `FORM_STYLES` does not name.* The set is closed: the grammar names each
   special form, and there is no `let*`, `letrec`, `when` or `unless` to worry
   about, so this is a decision over a fixed list.
   - `and`, `or`, `struct`, `import`, `export`: rule 7, and they hardly ever
     break, so there is no practical difference either way.
   - `display` was the one real choice, and it stays on rule 7 too. It takes
     exactly one argument, so it breaks only when that argument is multi-line,
     and rule 7 then aligns it at column 9. A body form at +2 was weighed: it
     keeps an argument whole where starting at column 2 lets it fit, but costs
     a line whenever the argument would have broken anyway, and DrRacket has no
     special rule for `display` — it is a plain application there, which is the
     point of following DrRacket at all. `test/scheme/pretty.test.ts` pins it.
   - `{...}`: pairs lay out as units, so a break falls between them and never
     between a key and its value; the pairs align under the first, per rule 7.
   - `[...]` and `#(...)`: rule 7 over the elements, and over the inner form
     past the `#`. Ordinary data-list behaviour.
5. **Should the output pane's source captions be formatted?** *No, they stay
   verbatim.* `SourceCaption.vue` shows the statement's source exactly as the
   student wrote it: a caption is a citation of their code, not our rendering
   of it.

_(Co-created with Claude Code)_

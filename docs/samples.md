# Samples

Complete Scamper programs, for demonstrations, screenshots, and test fixtures.
All three live in `samples/` (#405):

| File | What it shows |
| --- | --- |
| `samples/showcase.scm` | the language: literals, functions, pattern matching, recursion, docstrings |
| `samples/libs.scm` | the libraries, one section each: `image`, `canvas`, `data`, `music`, `html`, `reactive`, `lab`, `rex`, `test` |
| `samples/reading.html` | the transcript widget (#375), embedding two readings on one page |

They are development artifacts, not build inputs.
Nothing here ships to `dist/`, and the IDE does not list them.

## Running them

+ `samples/showcase.scm` is pure and runs anywhere: `npm run cli samples/showcase.scm`, or open it in the IDE.
+ `samples/libs.scm` needs a browser.
  The `canvas`, `html`, and `reactive` libraries all reach for `document`, so it runs in the IDE but not under the CLI.
  Open it with `npm run dev` to see the drawings, charts, and widgets.
+ `samples/reading.html` is served by `npm run dev` at [`/samples/reading.html`](http://localhost:5173/samples/reading.html).
  It needs the dev server rather than a bare `file://` open.

`samples/reading.html` loads the **built** bundle `dist/scamper-embed.js` — the one file a course site takes from a deployment — so `npm run build` must have run first, and a change to the widget appears here only after another build.
This is the difference from `src/app/web/embed/embed.html`, which loads the entry point from source: `embed.html` exercises the widget, `reading.html` exercises what is deployed.

## Contents

Every statement in the two `.scm` files produces a value, so each reads top-to-bottom as a transcript rather than as a program with a single answer at the end.

+ `samples/libs.scm` builds compositions but never calls `play-composition`, so opening it stays silent.
+ The `audio` library appears only as a comment, since it needs a real browser audio context.
+ The `file` library is omitted: its functions read and write a user's own files, and a sample has nothing sensible to open.

`samples/reading.html` reproduces two [CSC 151](https://osera.cs.grinnell.edu/csc151/) readings verbatim — [Recursion Over Lists](https://osera.cs.grinnell.edu/csc151/readings/recursion-over-lists.html) and [Interactivity, Events, and Reactivity](https://osera.cs.grinnell.edu/csc151/readings/interactivity-events-and-reactivity.html) — separated by a horizontal rule.
The first is entirely non-interactive and the second is interactive throughout, which makes the page a mixed one.
Runnable blocks are widgets; walkthrough excerpts stay plain listings, as in the readings themselves.

A third section at the end belongs to neither reading.
It exercises the widget features the readings do not use: a hidden preamble, `data-continues` chains, a plain `html` button, and `data-height`.
A reactive file chooser is absent: its callback does not fire inside a widget (#397).

Two known defects affect this page.
On a busy machine an interactive widget can render everything it will render and still never finish; because widgets run sequentially, the ones below it never start (#415).
Idle, the whole page runs in well under a second.

## Tests

`test/samples/` runs all three and requires that they report no errors: `test/samples/scm-samples.test.ts` for the two programs, `test/samples/reading-page.test.ts` for the page.
Nothing else in the suite covers them, since samples are not compiled, imported, or linted.
The assertion is that a sample runs, not that it produces particular output.

Run `npx vitest run test/samples` after editing a sample.
If a sample has to stop showing something the language no longer supports, note it in the pull request.

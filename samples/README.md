# Samples

Scamper in action, for whoever needs a real program rather than a snippet: a
demonstration, a screenshot, a page to point a browser at, or a fixture for a
test. Three of them (#405):

| File | What it shows |
| --- | --- |
| `showcase.scm` | the language: literals, functions, pattern matching, recursion, docstrings |
| `libs.scm` | the libraries, one section each: `image`, `canvas`, `data`, `music`, `html`, `reactive`, `lab`, `rex`, `test` |
| `reading.html` | the transcript widget (#375), embedding two readings on one page |

These are development artifacts, not part of any build. Nothing here ships to
`dist/`, and the IDE does not know they exist.

## Running them

+ `showcase.scm` is pure and runs anywhere: `npm run cli samples/showcase.scm`,
  or open it in the IDE.
+ `libs.scm` needs a browser. The `canvas`, `html` and `reactive` libraries all
  reach for `document`, so it runs in the IDE but not under the CLI. Open it
  with `npm run dev` to see the drawings, charts and widgets rendered.
+ `reading.html` is served by `npm run dev` at
  [`/samples/reading.html`](http://localhost:5173/samples/reading.html). It
  loads the embed entry point from source, the way `src/app/web/embed/embed.html`
  does, so it needs the dev server rather than a bare `file://` open.

## What is in them, and what is not

Every statement in the two `.scm` files produces a value, so each reads
top-to-bottom as a transcript rather than as a program with a single answer at
the end. Playback is left to the reader: `libs.scm` builds compositions but never
calls `play-composition`, so opening it stays quiet. The `audio` library is shown
only as a comment, since it needs a real browser audio context, and the `file`
library is left out entirely -- its functions read and write a user's own files,
and there is nothing sensible for a sample to open.

`reading.html` reproduces two of the [CSC 151](https://osera.cs.grinnell.edu/csc151/)
readings verbatim -- [Recursion Over
Lists](https://osera.cs.grinnell.edu/csc151/readings/recursion-over-lists.html)
and [Interactivity, Events, and
Reactivity](https://osera.cs.grinnell.edu/csc151/readings/interactivity-events-and-reactivity.html)
-- separated by a horizontal rule. The pairing is the point: the first reading is
entirely non-interactive and the second is interactive throughout, so the page is
a page of the mixed kind, which is the case worth checking. Each reading's
runnable blocks are widgets and its walkthrough excerpts stay plain listings,
which is how the readings themselves distinguish them.

Being copies, they will drift from the originals; that is accepted. They are a
sample of the widget, not a mirror of the course.

A third section at the end is marked as belonging to neither reading. It
exercises the widget features the readings happen not to use -- a hidden
preamble, `data-continues` chains, a plain `html` button, `data-height`. A
reactive *file chooser* is deliberately absent: its callback does not fire inside
a widget, the one known gap (#397).

One caveat worth knowing before demonstrating this page: on a busy machine an
interactive widget can render everything it is going to render and still never
finish, and because the widgets run one after another the ones below it then
never start (#415). Idle, the whole page runs in well under a second.

## The tests

`test/samples/` runs all three and insists they report no errors --
`scm-samples.test.ts` for the two programs, `reading-page.test.ts` for the page.
That is the whole point of keeping samples here rather than in a gist: nothing
else in the suite would notice one going stale, since they are not compiled,
imported, or linted. The bar is "it runs", not "it produces exactly this" -- a
sample pinned to its own output would need re-blessing on every rendering change.

Editing a sample means running `npx vitest run test/samples` before committing.
If a sample has to stop showing something the language no longer supports, that
is a finding worth a line in the pull request, not just a quiet edit.

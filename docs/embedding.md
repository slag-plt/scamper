# Embedding Scamper in a reading

A **transcript widget** turns a block of Scamper code in a web page into the
code plus what it produced, interleaved. It is meant for online readings — the
CSC 151 readings are the reason it exists (#375).

## The smallest page that works

```html
<script type="module"
        src="https://scamper.cs.grinnell.edu/<version>/scamper-embed.js"></script>

<div class="scamper-transcript">
;;; (factorial n) -> number?
;;;   n : number?
;;; Returns n!
(define factorial
  (lambda (n)
    (if (zero? n) 1 (* n (factorial (- n 1))))))

(factorial 5)
</div>
```

Every `.scamper-transcript` on the page is found and run, in the order it
appears. Each widget fills its container's width.

That script is the whole of it: `scamper-embed.js` carries its own styles and
refers to nothing outside itself, so a reading need not link a stylesheet, and
the file works as well copied into a reading's own site as loaded from a Scamper
deployment. A version is a directory there, so `<version>` above picks one — a
reading pinning the version it was written against will not change under its
author.

## Writing the code

Two forms. The one above puts the code in the element itself, which is the
simplest thing that works. The other puts it in a `<script>`:

```html
<div class="scamper-transcript">
  <script type="text/scamper">(factorial 5)</script>
</div>
```

Prefer the `<script>` form for anything containing `<` or `&`. A browser does
not parse a script's contents as markup, so `(< 1 2)` survives verbatim; in the
bare form it is markup and the browser will mangle it.

## Hiding setup

Code in a `text/scamper-preamble` script runs first and is not shown:

```html
<div class="scamper-transcript">
  <script type="text/scamper-preamble">(import image)</script>
  <script type="text/scamper">(rectangle 60 30 "solid" "teal")</script>
</div>
```

## Building on an earlier widget

By default each widget is its own program and sees only the standard library, so
a definition in one cannot leak into the next. `data-continues` says otherwise:

```html
<div class="scamper-transcript" id="defs">(define x 41)</div>

<!-- continues the widget immediately above -->
<div class="scamper-transcript" data-continues>(+ x 1)</div>

<!-- continues a named widget, wherever it is on the page -->
<div class="scamper-transcript" data-continues="defs">(* x 2)</div>
```

A chain can run as long as you like: each widget hands its final environment to
the next.

## Sizing

A widget grows to fit its contents. `data-height` fixes the height and adds a
scrollbar instead:

```html
<div class="scamper-transcript" data-height="20em">…</div>
```

## What is interactive

Buttons, `on-keydown!`, `animate-with`, the reactive library, music playback and
the file choosers all work, and each widget's handlers belong to that widget: one
widget's button sees its own definitions and reports its own errors, and starting
a later widget does not tear down an earlier one's animation.

Two gaps that used to be listed here are fixed: a file chooser's callback never
firing (#397), and a widget rendering everything and never completing, which
left the rest of the page blank (#415).

## Notes for whoever maintains this

+ The entry point is `src/app/web/embed/embed-entry.ts`; the scan and the run
  loop are `embed.ts`, and `embed-display.ts` is the output channel that
  captions each statement with its source.
+ The entry **awaits** `renderers.js` before running anything. `scamper.ts`
  starts that import fire-and-forget, which is fine for the IDE — someone has to
  press Run first — but a page of widgets starts immediately and loses the race,
  and every drawing renders as its `(rectangle …)` text instead of a picture.
+ Widgets run **sequentially**, because one may continue another and so cannot
  start until that one's environment exists.
+ Each widget is a `Scamper.executeEmbedded` call. Unlike `execute`, that does
  not supersede: the foreground run is left alone and each widget gets a run of
  its own (see `RunContext` in `src/scamper.ts`).
+ A widget's run deliberately outlives its fiber, so its handlers keep working
  after its program has finished — exactly as the IDE's do.
+ Ids are a counter rather than `crypto.randomUUID()`, which needs a secure
  context: a reading served over plain `http://` still has to run.
+ `dist/scamper-embed.js` is a **second** build of that entry point
  (`vite.config.embed.ts`, run after the site build by `scripts/build`): one
  chunk, with the dynamic imports and every stylesheet folded in. The site build
  emits the same entry as `assets/scamper-embed-<version>.js`, but as chunks
  shared with the IDE and with the CSS left to the page — which suits
  `embed.html`, sitting in the deployment beside them, and is unusable from a
  reading on another site.
+ `src/app/web/embed/embed.html` is a demonstration page and what the browser
  test drives. `npm run dev` serves it at `/embed.html`. It links the
  stylesheets and loads the entry point directly, so it exercises the widget
  rather than the bundle.
+ `samples/reading.html` is the same thing at full size: two real readings on
  one page, one non-interactive and one not, which is the mixed case a course
  site actually produces. `npm run dev` serves it at `/samples/reading.html`,
  and `test/samples/reading-page.test.ts` runs every widget on it. It differs
  from `embed.html` in what it loads: the built `dist/scamper-embed.js` rather
  than the source entry, and none of Scamper's stylesheets -- so it is the one
  place the deployed artifact's "add a single script tag" claim is actually
  put to the test. Build before serving it.

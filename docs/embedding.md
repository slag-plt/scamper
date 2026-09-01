# Embedding Scamper in a webpage

We provide a collection of widgets for embedding Scamper into a webpage, e.g., for displaying the output of code or a tracing example.

## Minimal example

```html
<script type="module"
        src="<host>/<version>/scamper-embed.js"></script>

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

Every `.scamper-transcript` on the page is found and run, in the order it appears. 
Each widget fills its container's width.

`scamper-embed.js` carries its own styles and refers to nothing outside itself, so a reading need not link a stylesheet

## Embedding Scamper code

There are two ways to embed Scamper code. The above example puts the code in the element itself. Alternatively, we can place code in a `<script>`:

```html
<div class="scamper-transcript">
  <script type="text/scamper">(factorial 5)</script>
</div>
```

`<script>` preserves `<` or `&` which are, otherwise, treated specially within HTML.

## Preamble code

If you wish to specify a hidden preamble to power a widget, use `text/scamper-preamble`.
This code is not made visible to the user:

```html
<div class="scamper-transcript">
  <script type="text/scamper-preamble">(import image)</script>
  <script type="text/scamper">(rectangle 60 30 "solid" "teal")</script>
</div>
```

## Building on an earlier widget

By default each widget is its own program and sees only the standard library, so
a definition in one cannot leak into the next.
We can specify that the current widget should use the previous widget as a (hidden) preamble via the `data-continues` attribute:

```html
<div class="scamper-transcript" id="defs">(define x 41)</div>

<!-- continues the widget immediately above -->
<div class="scamper-transcript" data-continues>(+ x 1)</div>

<!-- continues a named widget, wherever it is on the page -->
<div class="scamper-transcript" data-continues="defs">(* x 2)</div>
```

A chain can run as long as you like: each widget hands its final environment to the next.

## Sizing

A widget grows to fit its contents.
`data-height` fixes the height and adds a scrollbar instead:

```html
<div class="scamper-transcript" data-height="20em">…</div>
```

## Interactivity

Interactive elements such as buttons, `on-keydown!`, `animate-with`, the reactive library, music playback and the file choosers all work, and each widget's handlers belong to that widget.
One widget's button sees its own definitions and reports its own errors, and starting
a later widget does not tear down an earlier one's animation.

(_Note_: there are two bugs here to be still be resolved, #397 and #415.)

## Implementation notes

+   The entry point is `src/app/web/embed/embed-entry.ts`; the scan and the run loop are `embed.ts`, and `embed-display.ts` is the output channel that captions each statement with its source.
+   The entry **awaits** `renderers.js` before running anything so that renderers are guaranteed to be loaded before they are used.
+   Widgets run **sequentially**, because one may continue another and so cannot start until that one's environment exists.
+   Each widget is a `Scamper.executeEmbedded` call.
    Unlike `execute`, that does not supersede: the foreground run is left alone and each widget gets a run of its own (see `RunContext` in `src/scamper.ts`).
+   A widget's run deliberately outlives its fiber, so its handlers keep working after its program has finished.
+   `dist/scamper-embed.js` is a **second** build of that entry point
  (`vite.config.embed.ts`, run after the site build by `scripts/build`): one chunk, with the dynamic imports and every stylesheet folded in.
    The site build emits the same entry as `assets/scamper-embed-<version>.js`, but as chunks shared with the IDE and with the CSS left to the page — which suits `embed.html`, sitting in the deployment beside them, and is unusable from a reading on another site.
+   `src/app/web/embed/embed.html` is a demonstration page and what the browser test drives.
    `npm run dev` serves it at `/embed.html`.
    It links the stylesheets and loads the entry point directly, so it exercises the widget rather than the bundle.
+   `samples/reading.html` is the same thing at full size.
    `npm run dev` serves it at `/samples/reading.html`, and `test/samples/reading-page.test.ts` runs every widget on it.
    It differs from `embed.html` in what it loads: the built `dist/scamper-embed.js` rather than the source entry with none of Scamper's stylesheets.
    Make sure to build before using it.

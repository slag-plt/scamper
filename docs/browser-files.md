# The browser-files page

`files.html` is a standalone browser-storage manager: it lists everything
Scamper has put in this browser, hands any of it back as a download, and
deletes what is in the way.
It exists for the failure in issue #130 -- a student loads a file too large for
Scamper to open, and from then on the IDE will not come up.
Before this page the only escape was clearing the site's data, which destroys
every other file too.

## Where it is

`files.html`, beside the IDE's `index.html` -- so
`https://scamper.cs.grinnell.edu/<version>/files.html` on the Grinnell
deployment, which serves each release from its own directory
(`docs/server-architecture.md`), and <http://localhost:5173/files.html> in
development.
It is one of the entry points in `vite.config.ts`'s `htmlEntries`, so it builds
and deploys exactly as `docs.html` does.

**Nothing in the IDE links to it, deliberately.**
It is a rescue tool, and the operations it offers -- deleting a folder,
emptying storage -- are ones a student should have to go looking for rather
than wander into.
This document and `CLAUDE.md` are how it is discovered; the patch notes name
the URL too.

## What it does

+ Lists the root of the browser's private file storage (OPFS): name, what the
  file is, size, and when it last changed.
  Nothing is hidden, which is the point -- a leftover `hello.scm.crswap`, a
  `.hello.scm.history` blob, and a `.scamper.config` from an older build are
  all part of how an instance gets stuck, and each is labelled in plain
  language.
+ Downloads any single file, or everything at once as a zip.
  The zip really is everything, unlike the IDE's export, which keeps only the
  student's own files.
+ Renames and deletes, including a non-empty directory.
+ Resets the IDE's settings -- a separate button, because those live in
  `localStorage` (`src/app/web/ide-config.ts`) rather than in storage, so
  "which file was open" survives deleting the file itself.
  It touches no files and leaves the light/dark choice alone.

It shows only the root directory.
Nothing in Scamper creates a directory, so a breadcrumb stack would be
untested dead code; a directory can still be deleted whole.

## How it is built

The page has to load on a Scamper the IDE cannot open, so its import graph is
kept small on purpose: Vue, the shared `ThemeToggle`, the pure helpers in
`src/fs/fs.ts`, and its own OPFS layer.
No `src/scamper.ts`, no standard library, no CodeMirror, no `src/fs` backend.

`src/app/files/opfs-direct.ts` is that layer, and it deliberately does not
reuse `src/fs/opfs.ts`.
The header of the file records why in full; the load-bearing reason is that
`getFileList` builds a five-line preview by calling `file.text()` on every user
file, so on the storage this page exists to rescue, *listing the directory is
itself the hazard*.
This layer asks a handle for `.size` and `.lastModified` and never reads
contents.

Confirmations are `window.confirm` and `window.prompt` rather than the IDE's
modal system, for the same reason: fewer moving parts between the student and
their files.

If an IDE tab is already open the page says so, by asking
`navigator.locks.query()` about the `scamper-single-instance` lock.
It never takes that lock itself -- doing so would leave the student unable to
open the IDE afterwards.

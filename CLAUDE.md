# CLAUDE.md

This file provides guidance to LLM agents when working with code in this repository.

## Project Overview

**Scamper** is a mini-Scheme implementation designed for teaching multimedia programming on the web. It provides a complete web-based programming environment with IDE, documentation viewer, command-line runner, and an embeddable widget.

## Tech Stack
+ Host language: Typescript
+ Scripting languages: Python (large scripts) and Bash (simple scripts)
+ Package manager: Node
+ Build system: Vite
+ Build system and package manager: Node
+ Source language: R7RS Small Scheme with extensions drawn from Racket and Clojure
+ UI frameworks: Vue and Codemirror (text editor)
+ Scamper support libraries
    - Lezer (parsing)
    - Chart.js (data charts)
    - Papaparse (CSV parsing)
    - Webaudiofont (web audio management)
+ Linting: ESLint

## Development Commands

### The three arrangements

Scamper runs three ways, differing only in where a user's files live. The IDE
decides at startup by fetching `/config.json` — absent means browser storage,
present means the server it names — so one build serves all three and nothing
is compiled in.

1. **Static** (`npm run dev`, `npm run build`): front end alone, files in OPFS. No server. Most work needs only this.
2. **In-memory** (`npm run dev:memory`): both halves wired together, back end holding everything in memory with no accounts. For work on the API, `src/fs/`, or history without Docker.
3. **Full stack** (`scripts/server/server-up`): MariaDB + the API + Caddy serving the built front end and proxying `/api` to it, all one origin. The real deployment; its interface is the bash scripts, which are the *administrator's* tools rather than build steps.

### Building

+ `npm install`: installs dependencies
+ `npm run dev`: arrangement 1 — front end only, files in the browser
+ `npm run dev:memory`: arrangement 2 — front end *and* the `server/` back end, wired together. The back end runs in memory with no sign-in unless `DATABASE_URL` is set (`SCAMPER_SERVER_PORT` moves it off 3000)
+ `npm run dev:server`: starts the `server/` back end alone, watching for changes (`PORT` overrides its port)
+ `npm run build`: full production build (compilation + bundling) into `dist/`, via `scripts/build`. Two Vite builds: the site, and then `dist/scamper-embed.js` — the reading widget as a single self-contained file (`vite.config.embed.ts`), which is what a reading on another site includes. See `docs/embedding.md`
+ `npm run preview`: serves the built `dist/` locally, i.e. arrangement 1 as deployed
+ `npm run clean`: cleans the build
+ `npm run deploy`: deploys the *front end* to the production server (requires Unix and `compsci` host)
+ `npm run deploy:server-url -- <url>`: points every deployed version at the given file server by writing the site-root `config.json` (no argument clears it, putting everyone back on local storage)

**A server on a different origin from the front end is not supported**, deliberately: it would mean CORS, `SameSite=None` cookies, a CSRF check on the file routes to replace what `SameSite=Lax` gives for free, and exposure to browsers restricting third-party cookies. Arrangement 3 serves both from one origin instead.

### The file server

+ `scripts/server/server-up [--build]`: runs the whole app via `docker compose` — MariaDB, the API, and Caddy serving the built front end while proxying `/api` — applying migrations first, then waits until it answers. This is also how it is deployed; there is no deploy script for it. Needs a `.env` (see `.env.example`). `--build` is required after any change to `server/` *or* the front end, since the images hold copies of both. Flags pass through to `compose up`, which is what a deployment uses: `--pull always --no-build` runs the images CI published instead of building on the host
+ `scripts/server/server-sync`: the same thing under cron — pulls the repository and the images, and deploys only when one of them moved, so a release reaches the server on its own. Silent when there is nothing to do; a pinned `SCAMPER_TAG` turns it off and is also the rollback
+ `scripts/server/web-update`: rebuilds and swaps *only* the front-end container, leaving the API and database running. Use this for a front-end patch on a machine that can build; a host that pulls does `docker compose pull web && docker compose up -d --no-deps web` instead. `server-up --build` recreates everything, migrations included
+ `scripts/server/server-down [--wipe]`: stops it. The database survives; `--wipe` destroys it, after a typed confirmation
+ `scripts/server/server-dump [file]`: dumps the whole database to `dumps/scamper-<timestamp>.sql`
+ `scripts/server/user-{add,list,info,rename,chpwd,delete}`: account management, each running `server/src/admin.ts` inside the container. There is no sign-up, so `user-add` is the only way in. `BETTER_AUTH_URL` in `.env` must match the origin the browser is on, port included, or sign-in fails with `Invalid origin`
+ `npm run db:migrate --workspace @scamper/server`: creates BetterAuth's tables. Only needed when running the server *without* Docker — compose does it
+ `npm run account -- <command>`: the same account commands, for a database reachable from the host (i.e. no Docker). Against the compose stack its port is deliberately unpublished, so use the scripts above
+ `npm run start:server`: runs the back end without watching, as the container does
+ CI (`.github/workflows/node.js.yml`) builds `scamper-{web,server,migrate}` and pushes them to ghcr.io from every green main, tagged `latest` and by commit; a commit that changes `package.json`'s version is also tagged with it and moves `release`. `SCAMPER_TAG` in `.env` chooses which a host follows — `release` (the default) deploys on version bumps only, `latest` on every merge, a version pins. Compose still builds any service whose tag is missing locally, so development is unaffected
+ `SCAMPER_TRUSTED_ORIGINS` in `.env`: further origins allowed to sign in, comma-separated. Empty in a real deployment; locally it lets `npm run dev -- --mode server` on :5173 sign in to the compose stack without editing `BETTER_AUTH_URL`

### Validation

+ `npm run validate`: runs the full validation process (test, typecheck, lint)
+ `npm run test`: runs the full test suite
+ `npm run typecheck`: runs the typechecker 
+ `npm run typecheck:server`: runs the typechecker over the `server/` workspace
+ `npm run lint`: runs the linter. **It fails on a single warning** (`--max-warnings 0`): every rule here is a warning by design, since `eslint-plugin-only-warn` is what the config loads, so this is what makes any of them binding. A warning that is genuinely wanted -- a guard the types cannot see, a deliberate `any` at a boundary -- is disabled on the line with the reason beside it, never left to accumulate
+ `npm run lint:fix`: automatically fixes simple linter errors

### Releases

+ A release is a commit on main that changes `version` in `package.json`; that is what deploys to a server and what decides the patch notes a student is shown. Ordinary merges deploy nowhere. See `RELEASING.md`
+ Cut one with `npm version <patch|minor|major> --workspaces --include-workspace-root --no-git-tag-version`, which writes `package.json`, `server/package.json`, and `package-lock.json`. The `version` job checks that they agree, that the version rose, and that a minor or major release has an entry in `src/app/web/patch-notes.ts`

### Patch notes

**Every pull request that changes user-facing functionality adds exactly one
line to the patch notes** in `src/app/web/patch-notes.ts`, under the entry for
the next version. Notes are written as the work lands rather than gathered at
release time, so that entry usually names a release that has not happened yet;
create it if it is not there.

+ Work a student cannot see adds no line -- a refactor, a test, a CI or tooling change, contributor documentation. Say so in the pull request instead, so the omission reads as a decision rather than an oversight
+ One line per PR, and one sentence per line. It summarises the change; it is not a changelog of the commits in it
+ Write it for a student, in terms of what they will notice, not how it was built. `patch-notes.ts` is what the IDE shows them on their first load of a new version
+ Which version to file under follows `RELEASING.md`: `patch` for a fix, `minor` for behaviour that is new or changed, `major` for a breaking change. A `minor` line lands under the next minor, and so on
+ **Do not bump `package.json`.** Naming the release early is the point; the `version` job exits early while the version is unchanged, so the line simply pre-satisfies the check for whoever cuts that release. Nothing reaches a student until the bump, since `patchNotesSince` never returns an entry newer than the running version

## Architecture Overview

### Source Tree Layout

+ `scripts/` — Standalone Node/bash scripts for parser generation, deployment, and build validation that live outside the Vite pipeline.
+ `public/` — Static assets (CSS, fonts, images) copied as-is into every build output.
+ `src/` — All application and language-implementation source code.
  - `src/app/` — The end-user apps, one folder each:
    - `src/app/cli/` — Node-based command-line entry point for running Scamper programs outside the browser.
    - `src/app/docs/` — Vue app rendering the searchable API/library documentation site (`docs.html`).
    - `src/app/search/` — Only `search.html`, a redirect. Search was its own app until #403 folded it into the docs page; the URL stays so older links keep working.
    - `src/app/web/` — Browser-facing UI: the IDE and its Vue components.
      - `src/app/web/embed/` — The transcript widget a reading embeds (#375): a `.scamper-transcript` block becomes its code interleaved with its output. See `docs/embedding.md`.
      - The notebook view (#410) is the open file shown as its forms, each with what it printed underneath. It is a *view* of the document the editor holds, never a second copy: `notebook-cells.ts` splits the file by the parser's own statement ranges, `composables/use-notebook.ts` keeps those cells and the document in step and writes every edit through, and `notebook-display.ts` files a run's output under the cell that produced it. Nothing is stored in a `.scm` file to make one, so the file's own spacing survives. `view-prefs.ts` decides which of the two views is on screen.
  - `src/fs/` — File system abstraction (browser OPFS, Node on the CLI, the Scamper server when logged in) used to load and save Scamper source files. `src/fs/index.ts` pairs a file system with its history as one `Backend`, so the two can never be mismatched.
  - `src/history/` — A file's save history (#42), as an interface with two backings: `flat-file.ts` keeps snapshots in a `.{filename}.history` blob beside the file, `server.ts` keeps one row per snapshot in the database. `policy.ts` decides when a save is worth recording and is shared with `server/`.
  - `src/js/` — The JavaScript "native" package: one folder per library that Scamper's standard library binds to via `js-var`.
  - `src/lib/` — The Scamper-language standard library (`.scm` sources) plus the loader that compiles and registers them at startup.
  - `src/lpm/` — The Little Pattern Machine bytecode runtime: fibers, scheduler, stack frames, and the handlers that execute compiled programs.
  - `src/scheme/` — The Scheme language front end: reader, AST, macro expansion, scope checking, and codegen down to LPM bytecode.
+ `gradescope/` — The Gradescope autograder harness (#404): `setup.sh`, `run_autograder`, and an example `autograder.scm` that an instructor zips and uploads. Not a build script; see its README.
+ `server/` — The Scamper file server: an npm workspace with its own `package.json` and `tsconfig.json`, holding the back end that serves a user's files (issue #357). Kept in this repo rather than a separate one so the `FS` contract in `src/fs/fs.ts` has a single definition and both sides of a change land in one PR. ESLint enforces the boundary: `src/` may not import `server/src/`, and `server/` may import *types* from `src/` but *values* only from the two shared contracts, `src/fs/fs.ts` and `src/history/policy.ts`. The server's DOM-free `tsconfig.json` backstops it, turning any stray browser import into a typecheck error.
+ `samples/` — Scamper in action (#405): `showcase.scm` for the language, `libs.scm` for the libraries, and `reading.html` embedding two readings on one page. Development artifacts rather than build inputs — nothing here ships — and `test/samples/` runs all three so they cannot go stale. See its README.
+ `test/` — Vitest test suites

## Compilation Pipeline

`src/scamper.ts` and its singleton `Scamper` object is the entry point for all Scamper language services.

### Front-end

1. **Parsing** (`src/scheme/syntax.grammar` and `lezer-bridge.ts`): handled by Lezer
2. **AST definitions** (`src/scheme/ast.ts`)
3. **AST expansion** (`src/expansion.ts`)
4. **Scope checking** (`src/scope.ts`)

### Back-end

The back-end of Scamper is the Little Pattern Machine (LPM), a stack-based virtual machine. The machine manages
Scamper programs as a collection of fibers of execution.

1. **Bytecode definitions** (`src/lpm/lang.ts`): LPM values, runtime structures, and the bytecode language
2. **Fibers** (`src/lpm/fiber.ts`)
3. **Opcode execution** (`src/lpm/handlers`)
4. **Output system** (`src/lpm/output`): definitions of output and error channels with implementations for text and web-based rendering
5. **Rendering system** (`src/lpm/renderers`): definitions of specific renderers of LPM values

### Apps

+ `src/app/web`: the main web IDE
+ `src/app/cli`: the command-line driver for console-based execution of Scamper programs
+ `src/app/docs`: the standard library documentation pages 
+ `src/app/search`: the standard library search page

## Design Notes

+ Scamper is maintained by undergraduate research students, so favor simpler designs as long as they do not cause significant headaches.
+ Additionally, readable design is paramount; when writing code, if there are design choices to be made, favor consulting the user when possible.
+ As much as possible, the codebase is meant to be written in a pure, functional style. The exception is when performance is necessary, and then effects are intentionally scoped as local as possible.
+ Use standard Typescript/Javadoc docstrings for function level comments with the following conventions:
    - Use standard tags, e.g., @param and @returns, when possible.
    - If a parameter's use is obvious from its name, the @param tag can be elided.
    - The different parts of a docstring should be short and to the point, appropriate for viewing in a tooltip.
+ Text that lives in an artifact whether in code comments, docstrings, commit messages, etc., should be concise and to the point; a few sentences or bullets rather than paragraphs.
+ When leaving messages and comments in Git and Github (e.g., commits, issues, and pull requests), do not include a postamble marker that Claude created the message/comment or a link to the Claude code transcript. Instead. leave a message "_(Co-created with Claude Code)_" at the end of the message/comment.

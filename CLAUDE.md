# CLAUDE.md

This file provides guidance to LLM agents when working with code in this repository.

## Project Overview

**Scamper** is a mini-Scheme implementation designed for teaching multimedia programming on the web. It provides a complete web-based programming environment with IDE, documentation viewer, and various runner interfaces.

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

### Building

+ `npm install`: installs dependencies
+ `npm run dev`: starts development server
+ `npm run dev:server`: starts the `server/` back end, watching for changes (`PORT` overrides its port)
+ `npm run build`: full production build (compilation + bundling)
+ `npm run clean`: cleans the build
+ `npm run deploy`: deploys to the production server (requires Unix and `compsci` host)
+ `npm run deploy:server-url -- <url>`: points every deployed version at the given file server by writing the site-root `config.json` (no argument clears it, putting everyone back on local storage)

### Validation

+ `npm run validate`: runs the full validation process (test, typecheck, lint)
+ `npm run test`: runs the full test suite
+ `npm run typecheck`: runs the typechecker 
+ `npm run typecheck:server`: runs the typechecker over the `server/` workspace
+ `npm run lint`: runs the linter
+ `npm run lint:fix`: automatically fixes simple linter errors

## Architecture Overview

### Source Tree Layout

+ `scripts/` — Standalone Node/bash scripts for parser generation, deployment, and build validation that live outside the Vite pipeline.
+ `public/` — Static assets (CSS, fonts, images) copied as-is into every build output.
+ `src/` — All application and language-implementation source code.
  - `src/app/` — The four end-user apps, one folder each:
    - `src/app/cli/` — Node-based command-line entry point for running Scamper programs outside the browser.
    - `src/app/docs/` — Vue app rendering the searchable API/library documentation site (`docs.html`).
    - `src/app/search/` — Vue app powering the standalone documentation search page (`search.html`).
    - `src/app/web/` — Browser-facing UI: the IDE, runner, and embeddable-widget entry points and their Vue components.
  - `src/fs/` — File system abstraction (browser OPFS, Node on the CLI, the Scamper server when logged in) used to load and save Scamper source files. `src/fs/index.ts` pairs a file system with its history as one `Backend`, so the two can never be mismatched.
  - `src/history/` — A file's save history (#42), as an interface with two backings: `flat-file.ts` keeps snapshots in a `.{filename}.history` blob beside the file, `server.ts` keeps one row per snapshot in the database. `policy.ts` decides when a save is worth recording and is shared with `server/`.
  - `src/js/` — The JavaScript "native" package: one folder per library that Scamper's standard library binds to via `js-var`.
  - `src/lib/` — The Scamper-language standard library (`.scm` sources) plus the loader that compiles and registers them at startup.
  - `src/lpm/` — The Little Pattern Machine bytecode runtime: fibers, scheduler, stack frames, and the handlers that execute compiled programs.
  - `src/prettier/` — A Prettier plugin that parses and pretty-prints Scamper/Scheme source.
  - `src/scheme/` — The Scheme language front end: reader, AST, macro expansion, scope checking, and codegen down to LPM bytecode.
+ `server/` — The Scamper file server: an npm workspace with its own `package.json` and `tsconfig.json`, holding the back end that serves a user's files (issue #357). Kept in this repo rather than a separate one so the `FS` contract in `src/fs/fs.ts` has a single definition and both sides of a change land in one PR. ESLint enforces the boundary: `src/` may not import `server/src/`, and `server/` may import *types* from `src/` but *values* only from the two shared contracts, `src/fs/fs.ts` and `src/history/policy.ts`. The server's DOM-free `tsconfig.json` backstops it, turning any stray browser import into a typecheck error.
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

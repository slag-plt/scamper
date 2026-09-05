# Scamper Testing

## Structure

The test suite is rooted at `test/`, and its structure mirrors `src/`.
Paths in the tree below are relative to `test/`.

+ `scheme/` — the front end and compiler
    - `parsing/` — source to AST, via the generated Lezer parser
        - `core.test.ts` — parsing of each language feature
        - `corpus.test.ts` — parsing of realistic, multi-statement programs
        - `errors.test.ts` — error recovery and reporting on malformed input
        - `grammar-keyword-parity.test.ts` — every reserved word has a matching grammar keyword production, and the reverse
        - `generated-parser-freshness.test.ts` — the checked-in generated parser matches `src/scheme/syntax.grammar`
        - `test-utils.ts` — shared parsing helpers
    - `literals.test.ts` — literal tokens: numbers, strings, chars, identifiers
    - `ast.test.ts` — AST helpers such as `expToString`
    - `sugarer.test.ts` — desugaring of sugared forms
    - `expansion.test.ts` — expansion of each sugared form at the AST level
    - `scope.test.ts` — scope-checking behavior; the specs are skipped or expected to fail, since scope checking is not yet wired into the compile pipeline
    - `codegen.test.ts` — codegen of each language feature to LPM bytecode
    - `query.test.ts` — querying of runtime values
    - `raise.test.ts` — raising Scamper errors from a fiber
    - `tracing.test.ts` — execution tracing
    - `index.test.ts` — the front-end pipeline end to end: tokenize, parse, compile
    - `docstring/` — docstring comment parsing and doc tags
+ `lpm/` — the back end and runtime
    - `ops.test.ts` — each opcode in isolation
    - `machine.test.ts` — combinations of opcodes
    - `range.test.ts` — the range datatype
    - `scheduler.test.ts` — the scheduler
    - `util.test.ts` — LPM utility functions
    - `lang.test.ts` — core LPM constructs: values, modules, environments
    - `fiber.test.ts` — fiber execution
+ `libs/` — one `.test.ts` per module of `src/js/`
    - `prelude`, `rex`, `data`, `runtime`, `test`, `canvas`, `image`, `lab`, and `html` have real coverage
    - `audio.test.ts`, `music.test.ts`, and `reactive.test.ts` are `test.todo` stubs, pending a browser-API mocking strategy
    - `canvas.test.ts` and `image.test.ts` each have a `.browser.test.ts` sibling for functions that need a real browser; see [Browser-mode tests](#browser-mode-tests)
    - `generated-sources-freshness.test.ts` — the checked-in generated library sources match `src/lib/*.scm`
    - `docstring-arity.test.ts` — every documented binding's signature declares the arity its implementation actually has, since contract insertion makes the docstring the runtime arity (#496)
+ `apps/` — end-to-end tests for the applications
    - `web/` — the IDE end to end; `embed-widget.test.ts` covers the reading widget (#375): several programs on one page, chained environments, and each widget's callbacks staying its own
    - `cli/` — the command-line runner against fixture programs
    - `docs/` and `search/` are not yet covered
+ `fs/` — the file-system layer
    - `opfs.test.ts`, `node-fs.test.ts`, `server-fs.test.ts` — each backend against the shared `FS` contract, including the text/bytes split (#385)
    - `file-kind.test.ts` — the extension table deciding how a file is read and edited
    - `backends.test.ts` — the fs/history pairing invariant
    - `config.test.ts` — how a deployment names its server
+ `history/` — a file's save history (#42), one file per backing
+ `server/` — the file server: its route layer, accounts, and the MariaDB stores
+ `samples/` — runs the samples in `samples/` (#405) and requires that they report no errors.
  `scm-samples.test.ts` covers `samples/showcase.scm` and `samples/libs.scm`; `reading-page.test.ts` covers `samples/reading.html`.
  Nothing else in the suite would notice a sample going stale, since samples are not compiled, imported, or linted.
  The split into two files is required: `runProgram` gives each program its own `Scheduler`, while a page runs on the Scamper singleton's.
+ `regressions/` — regression tests for fixed issues

## Browser-mode tests

A `*.browser.test.ts` file runs under real headless Chromium via Vitest's browser mode and Playwright (`test/vitest.browser.config.ts`).
This is needed by three features utilized by Scamper:

+ A real Canvas2D and font-metrics implementation — `test/libs/canvas.browser.test.ts` and `test/libs/image.browser.test.ts`, for pixel rendering, `getImageData` round-trips, and `measureText`.
+ Real layout, for a component's geometry.
+ Real OPFS — `test/fs/opfs.browser.test.ts`, and the #429 regression, which removes `createWritable` to exercise the fallback worker a Safari user gets.

Operational notes:

+ These files run one at a time (`fileParallelism: false`).
  One browser means one origin and therefore one shared OPFS, so a file that clears storage would empty another's fixtures mid-run.
+ They are excluded from `npm test` and `npm run validate` (see `test.exclude` in `vite.config.ts`), because a missing Playwright browser binary fails browser-mode startup outright.
  CI runs them as their own job.
  They *are* typechecked by `npm run typecheck:test`, which covers the whole of `test/`, so a browser spec that has drifted from `src/` still fails `npm run validate` even though nothing runs it there.
+ One-time setup: `npm run playwright:install` downloads a headless Chromium binary, cached outside the repository.
  Then run `npm run test:browser` or `npm run coverage:browser`.
+ On a bare Linux box that binary needs system libraries that are not installed alongside it.
  The symptom is a startup failure naming a missing shared object, such as `error while loading shared libraries: libatk-1.0.so.0`.
  Install them once with `npx playwright install --with-deps chromium`, which requires root and is what CI's `browser-tests` job runs.

## Style

+ Use the harnesses rather than setting up an execution environment per test.
+ Run programs through `runProgram` (`test/harness.ts`), never a hand-written `while (!fiber.isDone()) fiber.step()` loop.
  `runProgram` drives a real `Scheduler`, which is the only thing that services blocking primitives (`with-file`, the `file` library, `with-image-from-url`) and file imports, so tests exercise the same path as the IDE and CLI.
  Variants: `runProgramValues` for values rather than rendered text, `runProgramTraced` and `reductionTrace` for reduction traces, and `runProgramWithHTML`.
+ Step a fiber directly only in LPM-level tests: those covering the fiber/VM contract itself (`test/lpm/fiber.test.ts`, `machine.test.ts`, `ops.test.ts`), or those that must observe the machine *between* steps, such as frame depth and per-step raise or sugar.
  Use `stepFiberToCompletion` or `stepFiberWith` (`test/util.ts`) to make the intent explicit.
  Neither handles blocking primitives, file imports, or error recovery.
+ Library and language-feature tests cover standard cases, corner cases where the function's contract makes them meaningful, and failure cases.
+ Keep test descriptions short rather than full sentences.

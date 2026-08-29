# Scamper Test Plan

## Structure

Scamper has a comprehensive test suite whose structure nearly mirrors the structure found in src/.

+ scheme/ contains tests for the front-end/compiler
    - parsing/ tests parsing (via the generated Lezer parser) of source to AST
        - core.test.ts tests parsing of each language feature
        - corpus.test.ts tests parsing of realistic, multi-statement programs
        - errors.test.ts tests error recovery/reporting on malformed input
        - grammar-keyword-parity.test.ts checks every reserved word has a matching grammar keyword production and vice versa
        - generated-parser-freshness.test.ts checks the checked-in generated parser matches syntax.grammar
        - test-utils.ts shared parsing helpers
    - literals.test.ts tests parsing of literal tokens (numbers, strings, chars, identifiers)
    - ast.test.ts tests AST helper functions (e.g. expToString)
    - sugarer.test.ts tests desugaring of sugared forms
    - expansion.test.ts tests expansion of each sugared form at the AST level
    - scope.test.ts documents scope-checking behavior; specs are skipped/expected-to-fail since scope-checking isn't wired into the compile pipeline yet
    - codegen.test.ts tests codegen of each language feature to LPM bytecode
    - query.test.ts tests querying of runtime values
    - raise.test.ts tests raising Scamper errors from a fiber
    - tracing.test.ts tests execution tracing
    - index.test.ts tests the front-end pipeline end-to-end (tokenize, parse, compile)
    - docstring/ tests docstring comment parsing and doc tags
+ lpm/ contains tests for the back-end/runtime
    - ops.test.ts tests execution of each opcode in isolation
    - machine.test.ts tests execution of interesting combinations of opcodes
    - range.test.ts tests functionality of the range datatype
    - scheduler.test.ts tests the functionality of the scheduler
    - util.test.ts tests LPM utility functions
    - lang.test.ts tests core LPM language constructs (values, modules, environments)
    - fiber.test.ts tests fiber execution
+ libs/ contains one test.ts file per module of src/js/
    - prelude.test.ts, rex.test.ts, data.test.ts, runtime.test.ts, test.test.ts, canvas.test.ts, image.test.ts, lab.test.ts, html.test.ts have real coverage
    - audio.test.ts, music.test.ts, reactive.test.ts are test.todo-only stubs pending a browser-API mocking strategy
    - canvas.test.ts and image.test.ts each have a sibling canvas.browser.test.ts/image.browser.test.ts covering functions that need a real browser to test meaningfully -- see "Browser-mode tests" below
    - generated-sources-freshness.test.ts checks the checked-in generated library sources match src/lib/*.scm
+ apps/ contains end-to-end tests for Scamper applications
    - web/ tests the IDE end-to-end, and embed-widget.test.ts the reading widget (#375) -- several programs on one page, chained environments, and that each widget's callbacks stay its own
    - cli/ tests the command-line runner end-to-end against fixture programs
    - docs/ and search/ are not yet covered
+ fs/ contains tests for the file-system layer
    - opfs.test.ts, node-fs.test.ts, server-fs.test.ts test each backend against the shared FS contract, including the text/bytes split (#385)
    - file-kind.test.ts tests the extension table deciding how a file is read and edited
    - backends.test.ts tests the fs/history pairing invariant; config.test.ts tests how a deployment names its server
+ history/ contains tests for a file's save history (#42), one per backing
+ server/ contains tests for the file server: its route layer, accounts, and the MariaDB stores
+ samples/ runs the samples in samples/ (#405) and insists they report no errors -- scm-samples.test.ts for showcase.scm and libs.scm, reading-page.test.ts for the embedded-reading page. Nothing else would notice a sample going stale, since they are not compiled, imported, or linted. Deliberately two files: `runProgram` gives each program its own Scheduler while a page runs on the Scamper singleton's, and a harness program that registers a DOM handler wedges a singleton run later in the same environment
+ regressions/ contains regression tests for fixed issues

## Browser-mode tests

+ A `*.browser.test.ts` file needs something jsdom does not have, and runs under real headless Chromium via Vitest's browser mode + Playwright (test/vitest.browser.config.ts). Three things ask for one: a real Canvas2D/font-metrics implementation (test/libs/canvas.browser.test.ts, test/libs/image.browser.test.ts -- real pixel rendering, getImageData round-trips, measureText), real layout (a component's geometry), and real OPFS (test/fs/opfs.browser.test.ts, and the #429 regression, which takes `createWritable` away to exercise the worker a Safari user falls back to)
+ These files run one at a time (`fileParallelism: false`): one browser means one origin, so they share an OPFS, and a file that clears storage would empty another's fixtures halfway through it
+ Excluded from `npm test`/`npm run validate` (see vite.config.ts's test.exclude) since a missing Playwright browser binary fails vitest's browser-mode startup outright -- this is a deliberately separate, opt-in suite; CI runs it as its own job
+ One-time setup: `npm run playwright:install` downloads a headless Chromium binary (cached outside the repo); then run with `npm run test:browser` / `npm run coverage:browser`
+ On a bare Linux box the binary needs system libraries that are not installed with it. The symptom is a startup failure naming a missing shared object -- `error while loading shared libraries: libatk-1.0.so.0` -- which reads like a broken test run but is an environment gap: nothing in the suite is at fault and no amount of editing it will help. Install them once with `npx playwright install --with-deps chromium` (needs root; it is what CI's browser-tests job runs). Headlessness is not the problem -- CI runs headless too

## Style

+ To make tests readable, we use test harnesses to avoid the redundant work of setting up an appropriate execution environment for each test
+ Run programs through `runProgram` (test/harness.ts), never a hand-written `while (!fiber.isDone()) fiber.step()` loop. It drives a real `Scheduler`, which is the only thing that services blocking primitives (`with-file`, the `file` library, `with-image-from-url`) and file imports, so tests exercise the same path the IDE and CLI do. Variants: `runProgramValues` (values rather than rendered text), `runProgramTraced` / `reductionTrace` (reduction traces), `runProgramWithHTML`
+ Stepping a fiber directly is for LPM-level tests only -- those about the fiber/VM contract itself (lpm/{fiber,machine,ops}.test.ts) or that must observe the machine *between* steps (frame depth, per-step raise/sugar). Use `stepFiberToCompletion` / `stepFiberWith` (test/util.ts) so the intent is explicit; neither handles blocking primitives, file imports, or error recovery
+ Library and language-feature tests aim for standard, corner (where meaningful per the function's contract), and failure cases
+ Test descriptions are short and to the point rather than full phrases of sentences
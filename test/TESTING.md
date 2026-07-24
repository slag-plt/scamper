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
    - web/ tests the IDE end-to-end
    - cli/ tests the command-line runner end-to-end against fixture programs
    - docs/ and search/ are not yet covered
+ prettier/ contains tests for prettier (pretty-printing functionality)
+ regressions/ contains regression tests for fixed issues

## Browser-mode tests

+ A few libs/ specs need a real Canvas2D/font-metrics implementation to test meaningfully (real pixel rendering, getImageData round-trips, measureText); these live in test/libs/canvas.browser.test.ts and test/libs/image.browser.test.ts and run under real headless Chromium via Vitest's browser mode + Playwright (test/vitest.browser.config.ts), not jsdom
+ Excluded from `npm test`/`npm run validate` (see vite.config.ts's test.exclude) since a missing Playwright browser binary fails vitest's browser-mode startup outright -- this is a deliberately separate, opt-in suite; CI runs it as its own job
+ One-time setup: `npm run playwright:install` downloads a headless Chromium binary (cached outside the repo); then run with `npm run test:browser` / `npm run coverage:browser`

## Style

+ To make tests readable, we use test harnesses to avoid the redundant work of setting up an appropriate execution environment for each test
+ Library and language-feature tests aim for standard, corner (where meaningful per the function's contract), and failure cases
+ Test descriptions are short and to the point rather than full phrases of sentences
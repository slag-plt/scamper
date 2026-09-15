# Bug sweep — 11 issues, one branch, one PR

**Branch:** `bug-sweep` (12 commits, local only, nothing pushed)
**Status:** all 11 implemented and verified. `npm run validate` clean:
`3621 passed | 36 skipped | 62 todo`, typecheck/typecheck:server/typecheck:test/test/lint all PASS.
Baseline on `main` was 3526 passing, so the sweep adds 95 tests and removes none.

Each issue was investigated in its own git worktree by a separate agent, which
reproduced the defect, wrote a regression test that failed first, implemented,
and ran full validation before reporting. This file consolidates those plans and
records the decisions that need a maintainer's eye.

---

## Decisions I want confirmed before this is pushed

### D1. #609 — round per-procedure (implemented) or at construction?
Implemented the **narrow** fix: `Math.round` inside `rgb-greyscale` and
`rgb-average`. The **broad** alternative is one line in `color_rgb`
(`src/js/image/color.ts:67-70`), alongside the clamp already there.

| | narrow (implemented) | broad |
|---|---|---|
| `(rgb-greyscale (rgb 32 32 32))` | `(rgba 32 32 32 255)` | same |
| `(rgb 32.7 10.2 5.9)` | unchanged, still fractional | `(rgba 33 10 6 255)` |

For broad: `rgb-component?` is *documented* as "an integer between 0 and 255"
and `rgb` declares all four params that way, so the narrow fix leaves that
documentation false; `(rgb (/ 255 2) 0 0)` still produces a fraction.
Against: it silently rewrites a number a student typed literally.
Existing-test churn is identical either way, so switching later is free.

### D2. #596 — confirm the breadth of hiding void
Your instruction was "make void's output be a hidden div rather than printing
void". Implemented literally, which has two reaches wider than `ignore`:
1. **Every** void-valued expression now shows nothing in the web output --
   `(vector-set! v 0 5)`, `(ref-set! r 1)`, `(hash-set! ...)` used to print `void`.
2. A void **nested in an aggregate** is hidden too: `(list void 2)` draws as
   `(list  2)` in the web, while the CLI still shows `(list void 2)`.
   Scoping to top-level only would mean special-casing void in every output-pane
   component instead of one renderer branch.

### D3. #606 — a one-parameter signature gets no position
`(car 5)` still says `expected pair or nonempty-list, received number`, with no
"as the first argument". With one argument there is nothing to disambiguate, so
the position is noise on the error students hit most. This is the one place the
agent went beyond the issue; say if you want it uniform instead.

### D4. Optional add-on — 6 more docstring prose mismatches
#594's regression test is general: it sweeps every `src/lib/*.scm` docstring and
checks each backticked name in the prose resolves to something real. It found
six more sites of the identical bug, currently **allowlisted** rather than fixed
(to keep #594 scoped to what it was filed for). All are one-word fixes in files
no other change touches:
+ `image.scm isosceles-triangle` -- prose says base `base`, the parameter is `width`
  (same wording in `solid-` and `outlined-` variants: 3 sites)
+ `image.scm find-colors` -- prose `color`, parameter is `color-name`
+ `music.scm instrument` -- prose names `comp`, which does not exist; the only param is `prog`
+ `music.scm make-note-handlers` -- prose `note-handler`, the binding is `note-handlers`

Say the word and I fold them in and drop the allowlist.

---

## The one real conflict, and how it was resolved

**#591 and #592 both rewrote the same block of `applyFn`** (js-function arm,
`src/lpm/handlers/op-handlers.ts`) and both rewrote the same `test/lpm/ops.test.ts`
fixture. They agree on *range* -- both route every catch branch through the
origin-keyed `siteRange` and both drop the suspend-path guard. They differ on
exactly one expression:

```
#591:  currFrame.name.startsWith('##') ? fn.name : currFrame.name
#592:  currFrame.origin === 'builtin' && !currFrame.name.startsWith('##')
         ? currFrame.name : fn.name
```

#592's is strictly narrower and **subsumes** #591's. They agree on an anonymous
library frame (both -> `fn.name`) and on a contract wrapper (both -> frame name),
and diverge only on a frame the student *named*, where #591 yields `f` -- which is
precisely #592's own defect -- and #592 yields `point-x`.

**Resolution, verified empirically (86/86 pass):** take #592's implementation, take
#591's regression suite, and take #591's `ops.test.ts` fixture, which is
parameterized over origin and pins *both* the `builtin`->`callRange` and
`user`->`bodyRange` halves where #592's hardcoded only `builtin`. #591's clearer
"two different questions" comment prose is merged onto #592's expression.

## The one cross-cutting interaction

**#606 changes the contract message format** ("expected a list" -> "expected a list
as the second argument"), which #589's and #590's new regression tests were
written against. Sequenced #606 last and retargeted the 8 affected expectations.
Each message strictly *gained* the argument position and lost nothing, so nothing
was loosened. Verified: 42/42 pass.

---

## Per-issue summary

### #599 — gradescope harness timeouts (not user-facing)
Three budgets stacked: `SCAMPER_TIMEOUT=30`, `spawnSync` 60s, vitest 60s. The
outer two were equal, so on a real hang the test and child died together and the
child's better diagnostic never applied.
+ `test/regressions/gradescope-harness.test.ts` -- spawnSync 60000 -> 35_000, describe 60_000 -> 40_000.
  35s not 30s deliberately: at equality `spawnSync` would kill `run_autograder` in the
  same instant `timeout 30` fires, before it can write the zero-with-a-reason results file.
+ NEW `test/regressions/gradescope-harness-timeout.test.ts` -- reads the three numbers out of
  the suite's source and asserts they nest strictly.
Measured: the harness takes ~0.8s, about 40x under budget.

### #594 — rex docstrings name a parameter their signatures don't
+ `src/lib/rex.scm` -- `rs` -> `xs` in `rex-concat` and `rex-any-of` descriptions.
+ NEW `test/regressions/docstring-prose-parameter-names.test.ts` -- general sweep (see D4).

### #590 — four `any` parameters outside data.scm
+ `src/lib/prelude.scm` -- `any-of`/`all-of`: `f1 : any` -> `procedure?` (only these two
  declarations touched, so #596's work in the same file merges clean).
+ `src/lib/reactive.scm` -- `on-timer`: `interval : any` -> `integer?` (non-negativity stays prose).
+ `src/lib/html.scm` -- `tag-set-children!`: `elt`/`c` : `any` -> `element?`. Its `c` prose
  claimed "an HTML element or string"; the native has never accepted a string, so that was false.
+ `test/libs/contract-samples.ts` -- added an `element?` sample, retired a now-redundant ARGS override.
+ NEW `test/regressions/prose-named-param-contracts.test.ts` (15 cases).
Corrections to the issue: the claimed "range inside prelude.scm:376" no longer holds
(#588 moved it); `tag-set-children!` is milder than framed (the native has its own guard).

### #589 — data.scm's `any` parameters
**14 sites, not the 15 the issue claims** -- `dataset?`/`plot?` carry no refinement line
and legitimately take any value; the issue's own enumeration sums to 14.
+ `src/lib/data.scm` -- one line per site.
  **The three `plot-*` rest parameters take the element predicate `dataset?`, not `list?`**:
  `contract.ts` checks a rest param per-argument via `all-satisfy?`, so `list?` would have
  required every argument to be a list and broken working code. Those three were the quiet
  half of the bug -- `(plot-linear 5)` silently rendered an *empty chart*.
+ NEW `test/regressions/data-any-parameter-contracts.test.ts` (27 cases).

### #596 — ignore is documented `-> void?` but returned an HTMLElement
+ `src/js/prelude/index.ts` -- `prelude_ignore` returns void, does nothing; dropped `requireBrowser`.
+ `src/lpm/renderers/html.ts` -- new `mkHiddenElement()`; `case 'undefined'` returns it.
+ NEW `src/lpm/renderers/vue/components/VoidRenderer.vue`.
+ `src/lpm/renderers/vue/simple-renderers.ts` -- `undefinedStrategy` selects it.
+ `src/lib/prelude.scm` **untouched** -- the docstring was already right.
+ 4 existing tests updated; #530's regression test repointed at the element's new home rather
  than deleted, filename kept so #530 stays findable.
+ NEW `test/regressions/ignore-returns-void.test.ts`.
Side effect: `ignore` now works on the CLI and in Gradescope, where it previously errored.

### #609 — RGB components are not integers
+ `src/js/image/color.ts` -- `Math.round` in `color_rgbGreyscale`; a `midpoint` helper in
  `color_rgbAverage` applied to all four components. Rounding not truncation: the reported
  value is 31.999999999999996 and the expected answer is 32; truncation gives 31.
+ Found a third symptom the reporter didn't mention: `drawing-color` on a composite inherits
  the fraction through `rgb-average` (127.5 -> 128).
+ 3 existing expectations updated (all had deliberately pinned the fractional values).
+ NEW `test/regressions/rgb-components-are-integers.test.ts` (7 cases).
See D1.

### #595 — DOM instanceof lint rule (not user-facing)
+ `eslint.config.mjs` -- class list lifted to a `domClass` const; selector now matches
  `[right.property.name=...]` too, and accepts a `typeof` guard on either side.
+ Fixes (a) the missed `window.HTMLElement` and (c) the yoda-spelling false positive.
+ **Deliberately does not fix (b)**, with the reasoning recorded in the rule's comment: the
  escape is not the callback -- `typeof foo !== 'undefined' && v instanceof HTMLElement` with
  no callback escapes identically. The hole is guard/test name disagreement, which only a
  custom rule can compare.
+ Allowlist untouched, per the issue.
+ `test/tsconfig.json` -- include `eslint.config.mjs` (exact precedent already present).
+ NEW `test/regressions/dom-instanceof-lint-rule.test.ts` -- drives ESLint over the rule
  *imported from the config*, so it pins what the build actually runs.

### #606 — clearer type errors
+ `src/lpm/util.ts` -- `typeOf` returns `floating point number` for a non-integer. Central, so
  all 19 call sites benefit. Integers keep plain `number`. Stale "(for debugging purposes)"
  docstring corrected.
+ `src/scheme/contract.ts` -- `positionWords`/`describePosition`; `mkErrorMsg` takes a position;
  `mkCheckChain` passes `params.length + i` for optionals so `(substring "hello" 0 1.5)` says *third*.
+ Phrasing matched to the existing convention (`src/lpm/util.ts:101` "The second argument to cons
  should be a list"), not invented.
+ **81 assertions across 12 suites updated**, applied via a positional diff-driven patcher rather
  than find-and-replace, because several blocks contain identical context strings beside changed
  ones. Every one strictly gained information.
+ NEW `test/regressions/contract-argument-position.test.ts` (6 cases).
See D3.

### #592 — struct accessor blames the enclosing call
+ `src/lpm/handlers/op-handlers.ts` -- see the conflict section above.
+ `src/js/runtime/index.ts` -- `runtime_mkGetFn`/`mkCtorFn`/`mkPredFn` wrap their result in
  `L.nameFn` with the Scamper spelling, as `Module.registerValue` does, so they can name themselves.
+ Giving accessors a real contract was tried and rejected with specifics: `contractProgram` runs
  only when `insertContracts` is set (library only, deliberately) and runs *before* `expandProgram`,
  so `(struct point (x y))` has not yet become accessor defines. Doing it anyway would push a frame
  per accessor call on a hot path and spill accessor internals into the trace view.
+ Also fixes the constructor, and a student's own `(error ...)` inside a named lambda.
+ 6 existing assertions updated; `ops.test.ts` fixture rebuilt (see conflict section).
+ NEW `test/regressions/struct-accessor-call-range.test.ts` (5 cases).

### #591 — applyFn conflates "has a site" with "has a name"
+ Lands via #592's implementation (see conflict section); contributes its regression suite and
  the richer `ops.test.ts` fixture.
+ NEW `test/regressions/anonymous-library-lambda-call-range.test.ts` (7 cases).
+ The sharpest finding: `map` was already correct and `vector-map` was not, purely because `map`
  recurses through a *named* helper while `vector-map` drives an inner anonymous lambda -- not a
  distinction a student can see.

### #577 — a cancelled import still reports into the stopped run
+ `src/lpm/scheduler.ts` -- new private `abandonIfCancelled(task)`, called at the top of
  `loadFile`'s fulfilment and rejection handlers and block-on's rejection handler.
  Differs from `resumeOrComplete`'s peek: that ends *every* suspension whatever the outcome;
  this takes the entry *only* on the cancelled path, so a live task still settles through the
  single chokepoint. The `@returns` doc states the coupling.
+ A `prettier --write` pass that had reformatted ~6 unrelated regions was reverted, since it
  would have collided with the `applyFn` work for no benefit.
+ No existing test needed updating.
+ NEW `test/regressions/cancelled-import-output.test.ts` (4 cases). Deterministic: the test holds
  the promise's resolve/reject, so no timing race exists to lose. Ran 8x targeted, 4x full suite.
+ Narrows #578's window from one side and corrects its framing (see the triage list).

---

## Remaining steps once approved

1. Push `bug-sweep` and open one PR referencing all 11 issues.
2. Spawn an independent code-review agent over the assembled diff; apply what it finds.
3. Report back with the PR URL and the triage list of ~30 new findings.

## Not in this sweep

Deferred by your scoping decision, reasons recorded on each issue:
#494 (quadratic trace), #557 (Range file identity), #553 (contract bypass),
#578 (module fiber cancel), #568 / #569 (tracing).
Excluded by the skill's own rule (investigation/blocked): #593, #436, #373, #351.

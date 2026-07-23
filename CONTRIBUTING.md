# Contributing

Hi future contributors / Sam's MAP students!

This guide gets you from only having experience through Grinnell coursework to being able to make a change to Scamper in about a week. If you can get through this faster, great! If not, don't worry: software development is hard, not to mention that you're also doing research at the same time.

## What you'll be able to do by the end

- [ ] Clone Scamper, install it, run `npm run dev`, and see your local instance in the browser.
- [ ] Trace at a high level what happens when you press the IDE's "Run" button --> input string in, output rendered out. You don't need to understand the runtime internals, just the broad shape of the pipeline.
- [ ] Add a trivial button to the Scamper IDE (i.e. one that pops an alert) through a Vue component in `src/app/web/components/`, and watch the hot-reload pick it up.
- [ ] Add a new function to a library in `src/lib/` and call it from a `.scm` file in the IDE.
- [ ] Run `npm run build`, `npm test`, and ensure `npx eslint <files-you-touched>` is clean before opening a PR.
- [ ] Know which file to open when Sam says "go look at how the prelude does X."

If those bullets all feel doable, please still skim the rest of the guide!

## What I'm assuming you know

I'm assuming you finished the intro sequence (CSC-151, CSC-161, CSC-207), so you're comfortable with multiple paradigms and basic data structures. You also used Scamper as a CSC-151 student, so the language itself isn't new (but contributing to Scamper is!). You have touched Git before. You probably haven't done web development, so you likely haven't seen a web build tool, a frontend framework, a real TypeScript codebase, a linter, or a Vue component before.

Hopefully you understand all these by the end!

## Suggested schedule

This is just how I would structure this guide.

Treat each "day" as ~4–5 focused hours. If something clicks faster, skip ahead. If something doesn't, reach out to your peers!

| Day | Focus | Deliverable |
| --- | --- | --- |
| 1 | Environment + web fundamentals | Scamper running locally via `npm run dev`; understand how a website gets served and rendered |
| 2 | JavaScript + HTML basics | Can generally read JS and the HTML files scattered through the project like `index.html` |
| 3 | npm, Vite, the build pipeline | Can explain what `package.json` does, each `scripts` entry, and how to install new packages; `npm test` and `npm run build` pass |
| 4 | TypeScript | Can parse the TypeScript syntax in `src/scamper-vue.ts` and `src/lib/test.ts`; recognize narrowing and generics |
| 5 | Vue 3 + linter/formatter setup | Editor lints and formats on save; can point to `<template>`/`<script>`/`<style>` blocks in an SFC like `IdeHeader.vue` |
| 6 | Mini-project (build) | Hand-built Vue 3 + TypeScript + Vite app |
| 7 | Mini-project finish + codebase tour | Mini-project passes `npx eslint .`; finished the codebase tour; ready to start your research work! |

## Day 1 — environment + how the web works

- [ ] Install Node 22 (matches CI in `.github/workflows/node.js.yml`) and an IDE like VS Code.
- [ ] `git clone` the repo, then follow `README.md` (`npm install`, `npm run dev`). Open the URL Vite prints. You should see Scamper.
- [ ] Read [How the Web works](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/How_the_Web_works) and [How browsers load websites](https://developer.mozilla.org/en-US/docs/Learn_web_development/Getting_started/Web_standards/How_browsers_load_websites).
  > Hopefully, you should understand how a website truly gets served and rendered on a browser.

## Day 2 — JavaScript and HTML

- [ ] Skim MDN's [Dynamic scripting with JavaScript](https://developer.mozilla.org/en-US/docs/Learn_web_development/Core/Scripting) module
  > Focus on the pages covering variables, conditionals, loops, functions, events, and objects. You don't need to do the challenges.
  > The goal is to be able to generally read JS and not to become an expert. (This is because we don't really use JavaScript: foreshadowing Day 4 TypeScript!)
- [ ] Skim MDN's [Introduction to HTML](https://developer.mozilla.org/en-US/docs/Learn_web_development/Core/Structuring_content/Basic_HTML_syntax)
  > Just enough to read the various HTML files scattered throughout the project like `index.html` (since again, we don't really use HTML either, we use Vue!).
  > CSS is intentionally skipped. You won't need it for most contributions.

## Day 3 — npm, Vite, and the build pipeline

- [ ] Read MDN's [Client-side tooling overview](https://developer.mozilla.org/en-US/docs/Learn_web_development/Extensions/Client-side_tools/Overview): focus on **Overview of modern tooling** and **The modern tooling ecosystem**.
- [ ] Read MDN's [Package management basics](https://developer.mozilla.org/en-US/docs/Learn_web_development/Extensions/Client-side_tools/Package_management) through the **Making your own commands** section.
- [ ] Read the [Vite "Why Vite"](https://vitejs.dev/guide/why.html) page (the rest of the guide is reference, come back to it!).
  > Vite is the *transformation* tool from the Overview reading. This page explains what problem it solves, what `npm run dev` and `npm run build` are actually doing under the hood, and why Scamper's `vite.config.ts` has one entry per HTML file.
- [ ] Open Scamper's `package.json`. Identify which packages are runtime deps vs. dev deps, then write yourself a one-line description of each `scripts` entry. Verify by running `npm run build`.
  > You should be able to understand what `package.json` does, as well as how to install new packages.

## Day 4 — TypeScript

- [ ] Read [TypeScript for JavaScript Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html) and [Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html).
  > These two chapters cover ~90% of what you'll see in `src/`.
- [ ] Skim [Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html) and [Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html).
  > You don't need to write these from scratch yet, just be able to recognize them!
- [ ] Read two real files in the codebase end-to-end. Don't worry about understanding *what* the code does yet, just try your best to parse the TypeScript syntax (you'll come back to these later!):
  - `src/scamper-vue.ts`: the top-level orchestrator. Shows a typed class with optional fields, an async method, and how the language pipeline + LPM runtime + libraries get wired together.
  - `src/lib/test.ts`: a real library file. Shows type aliases, interfaces extending interfaces, discriminated unions, and switch-narrowing on the discriminant.

## Day 5 — Vue 3, the linter, and your editor

- [ ] Do the [official Vue 3 interactive tutorial](https://vuejs.org/tutorial/) (Composition API track, what Scamper uses; see `src/app/web/components/IdeApp.vue`).
  > This is hands-on and takes ~2 hours. It's much more useful than reading the guide cover-to-cover.
- [ ] Skim [Vue's "Single-File Components"](https://vuejs.org/guide/scaling-up/sfc.html) and look at `src/app/web/components/IdeHeader.vue`.
  > You should be able to point to the `<template>`, `<script>`, and `<style>` blocks and explain each.
- [ ] Install some editor extensions (assuming you're using VS Code): **Vue (Volar)**, **ESLint**, **Prettier**, and **Vitest Explorer**. Turn on "format on save."
  > **Why we have a strict linter and formatter:** remember the "safety net" bucket from the Day 3 Overview reading? ESLint and Prettier are Scamper's safety net. Consistent formatting with Prettier removes style issues from code, and a strict ESLint catches whole categories of bugs (unhandled promises, unsafe `any`, unused variables) at edit time instead of at runtime in a student's browser during class (or worse: months later when spaghetti code starts piling up!).

  > Concretely, Scamper runs ESLint in `strict-type-checked` + `stylistic-type-checked` mode (see `eslint.config.mjs`), which means `any`, unsafe casts, and a lot of stylistic things are intentionally seen as bad things!
- [ ] Make a scratch file in the repo root (`scratch.ts`) containing one line: `export const x = 1`. Run `npx eslint scratch.ts`: it should report nothing. Now change it to `export const x: any = 1` and re-run; watch ESLint complain. Delete the file when you're done.
  > **Heads up on the existing warnings.** If you run `npx eslint .` over the whole repo, you'll see ~1000 pre-existing warnings. None of those are your fault, and fixing them is not a part of your research work (I think). Your job is just to not make it worse: any file *you* touch in a PR should leave with zero new ESLint warnings.

  > **No disable comments!!!** When ESLint or TypeScript complains about your code, fix the underlying issue: don't paper over it with `// eslint-disable-line`, `/* eslint-disable */`, `// @ts-ignore`, `// @ts-expect-error`, or `as any`. The whole point of the safety net is defeated if you cut holes in it, and if you use them, I will be mad!!! Don't do this!!!

## Days 6–7 — mini-project and codebase tour

Build a tiny Vue 3 + TypeScript + Vite app from scratch. Suggested scope: a simple logger: a `<textarea>` for input, and a button that logs the input to a scrolling output pane.

**Constraints:**

- No `npm create vue@latest` or any other scaffolding tool. Set up `package.json`, `vite.config.ts`, `tsconfig.json`, and `index.html` by hand. You'll Google a lot. That's the exercise.
- No AI tools during this project: no ChatGPT, no GitHub Copilot, no Cursor agent, no Google AI Overview. See the No-AI policy for why.

**You're done when:** the app runs via `npm run dev`, builds via `npm run build`, has at least one `.vue` component you wrote, and passes `npx eslint .` (copy [typescript-eslint's ESLint config example](https://typescript-eslint.io/getting-started#step-2-configuration) for this).

Once that's working, take the codebase tour:

1. `README.md`: how to run.
2. `library-development.md`: how libraries plug into the runtime. This is the doc you'll re-read most often.
3. `src/scamper-vue.ts`: re-read this and trace one call from input string to output.
4. `src/app/web/components/WebEmbedWidget.vue`: what we (will) use in future CSC-151 readings!
5. `src/lib/index.ts` and one library file (e.g. `src/lib/test.ts`): see what a library actually looks like.
6. One test file in `test/`: see how Vitest is used.

### Hopefully this was an accelerated course in web development!

## No-AI policy (during onboarding only)

For the week this guide covers, please don't use generative AI tools (ChatGPT, Copilot, Cursor's agent, Google's AI Overview, etc.) to write your code or read the docs for you. The goal is to build the mental model of how a TypeScript/Vue/Vite project fits together so that after onboarding, when you *do* use AI tools, you can tell when they're hallucinating. Once you're done onboarding, use whatever makes you productive (and whatever Sam lets you use).

## Optional deeper dive — runtime internals

Eventually you may want to understand Scamper's **LPM** runtime and the scheduling tricks it uses to cooperate with JavaScript's single-threaded event loop. If so:

- Read up on JavaScript [async/await and the event loop](https://developer.mozilla.org/en-US/docs/Learn_web_development/Extensions/Async_JS).
- Then read `src/lpm/` end-to-end.

Having taken CSC-213 would make this task much easier.
But, you can be a productive contributor on libraries and UI without any of this.

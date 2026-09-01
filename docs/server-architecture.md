# The Scamper server's architecture

Why the server is shaped as it is, and the rules a change to it has to respect.
Read once before working on either half of the file system; consult it again when changing the boundary between them.

`docs/server.md` covers running the server and its API; `docs/server-deployment.md` covers putting it on a host.

## Cross-origin

There is none.
Scamper is deployed with the static site and this server on **one origin**, so `/api/v1` is a path on the host the IDE is served from, which is the arrangement `npm run dev:memory` reproduces with a proxy.
No reply carries CORS headers, `OPTIONS` is a 405, and session cookies stay `SameSite=Lax` and same-origin.

In production the `web` container is what makes that true: Caddy serves the built front end and proxies `/api` here, so both halves answer on one hostname.

An `ALLOWED_ORIGIN` setting for a split-origin deployment used to exist and has been removed: it was configuration nothing set, on a path nothing exercised, and a credentialed cross-origin reply is a poor thing to leave untested.
Serving this from a second origin means restoring it deliberately, alongside `sameSite: 'none'` on the session cookie and `trustedOrigins` in `server/src/auth.ts`.

**`SameSite=Lax` is the only CSRF protection the file routes have.**
Nothing checks `Origin` on `/api/v1/*`, because a cross-site request carries no cookie.
Under `sameSite: 'none'`, any page a student visits could `PUT` or `DELETE` their files with their session attached, so an Origin allowlist would have to be added in the same change.
Two subdomains of one registrable domain are same-site and avoid this; only a genuinely different site needs it.

## Why the server lives in this repository

`server/` is an npm workspace of the main repository rather than a separate `scamper-server` repository:

- **One definition of the contract.**
  `src/fs/fs.ts` is six methods plus `FileEntry`, and the server exists to implement exactly that interface per-user.
  Adding a method should be one PR touching the interface, the OPFS implementation, the server implementation, and the route together.
- **Node code already lives here.**
  `src/app/cli/` and `src/fs/node.ts` are Node-targeted, so a server is not a foreign artifact in this tree.
- **One onboarding path.**
  `CONTRIBUTING.md` takes a student from zero web experience to a first change in a week; a second repository would double the clone/install/CI/PR surface that guide has to cover.

Revisit this if the server is ever operated by campus IT rather than the research group, or if student contributors should not hold commit access to production authentication code.

## The client/server boundary

Because this is a workspace, npm hoists its dependencies into the root `node_modules`, so nothing physically stops a Vue component from importing a server-only package.
ESLint enforces the split:

- `src/` may not import from `server/src/` at all.
- `server/` may import **types** from anywhere in `src/` (`import type`).
  Type-only imports are erased at compile time, so they add no runtime coupling and cannot drag browser code into the server.
- `server/` may import **values** only from the two shared contracts: `src/fs/fs.ts` (the `FS` interface, `FileEntry`, and what counts as a user's own file) and `src/history/policy.ts` (when a save is worth recording).
  Sharing them keeps the backends agreeing on what "hidden" means and how long the merge window is, rather than each carrying its own copy of the answer.

The rule is written as a list of forbidden directories rather than "all of `src/` except those two", because these globs follow .gitignore semantics: a pattern matches a path segment anywhere plus everything beneath it, and negation does not re-admit a descendant.
Add a line when `src/` grows a top-level directory.

A second guard covers what lint cannot express: `server/tsconfig.json` omits the `DOM` lib, so importing a browser module such as `src/fs/opfs.ts` fails `npm run typecheck:server` with `Cannot find name 'navigator'`.
That is an error rather than a warning, so it blocks `npm run validate`.

The ESLint rules above are *warnings*, since the repository uses `eslint-plugin-only-warn` (#154).
`npm run lint` runs with `--max-warnings 0`, which is what makes them binding.

## Constraints

**Routes must stay versioned and backward-compatible.**
`scripts/deploy` rsyncs each front-end release into its own directory (`scamper.cs.grinnell.edu/3.5.0/`) and `scripts/update-latest` only moves a redirect, so every past release stays reachable at its URL indefinitely.
One server therefore serves many client versions at once.
A monorepo does not make both sides change atomically, since already-deployed clients never get the update.
Ship a breaking change as `/api/v2` beside `/api/v1`.

**Recording is decided twice.**
The client settles the common case from its cached head without sending a request, since autosave fires every few seconds while a student types and almost none of those firings deserve an entry.
When the client cannot rule a save out, the server re-applies the same predicate — the same module, `src/history/policy.ts` — against what it holds, and its answer wins.

**The server stamps snapshot times, not the client.**
A history now spans a student's machines, and a laptop running ten minutes fast would otherwise sort its snapshots above ones taken later elsewhere.

**`fileExists` is a hot path**, as `src/fs/opfs.ts` documents: module resolution, import steps, and the `file-exists?` primitive a student can call in a loop.
One request per call would turn that loop into a network round-trip per iteration.
`src/fs/server.ts` caches file names instead, refreshing them on each listing and updating them on its own writes, so a warm `fileExists` makes no request.

## Where the client half lives

- `src/fs/config.ts` reads the site-root `/config.json` that names the server.
  Any failure means "no server, stay on local storage" — the common case, since a `npm run dev` checkout has no config at all.
- `src/fs/server.ts` is `ServerFileSystem`, the `FS` implementation that talks to these routes.
- `src/history/server.ts` is `ServerHistory`, the `History` implementation that talks to the history routes.
  `src/history/flat-file.ts` is the OPFS/CLI one.
- `src/app/web/ide-config.ts` holds the IDE's own settings — which file was open, which patch notes have been seen — and is **not** an `FS` client.
  It is per-machine state about a browsing session, so it lives in `localStorage`.
  As a file it would follow the user between machines, so opening a laptop would inherit what the lab computer had open, and it would cost a write to the server on every tab hide.
  Not to be confused with `src/fs/config.ts`, which reads the deployment's `/config.json`.
- `src/fs/index.ts` exposes `setBackend()`, which is the login/logout seam.
  It takes a file system and a history together so a server file system can never end up paired with a flat-file history -- that combination would write `.{filename}.history` blobs into the server's file storage, which is exactly the layout the database replaces.
  `src/app/web/server-session.ts` chooses which one at startup, and `SignInModal.vue` is the sign-in dialog.

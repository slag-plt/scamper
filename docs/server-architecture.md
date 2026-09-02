# The Scamper server's architecture

How the two halves of the file system fit together, and the rules a change to either has to respect.

`docs/server.md` covers running the server and its API; `docs/server-deployment.md` covers putting it on a host.

## Cross-origin

There is none.
Scamper is deployed with the static site and this server on **one origin**, so `/api/v1` is a path on the host the IDE is served from, which is the arrangement `npm run dev:memory` reproduces with a proxy.
No reply carries CORS headers, `OPTIONS` is a 405, and session cookies stay `SameSite=Lax` and same-origin.

In production the `web` container is what makes that true: Caddy serves the built front end and proxies `/api` here, so both halves answer on one hostname.

A split-origin deployment is unsupported.
Serving this from a second origin requires an allowed-origin setting, `sameSite: 'none'` on the session cookie, and `trustedOrigins` in `server/src/auth.ts`.

**`SameSite=Lax` is the only CSRF protection the file routes have.**
Nothing checks `Origin` on `/api/v1/*`, because a cross-site request carries no cookie.
Under `sameSite: 'none'`, any page a student visits could `PUT` or `DELETE` their files with their session attached, so an Origin allowlist has to be added in the same change.
Two subdomains of one registrable domain are same-site and avoid this; only a genuinely different site needs it.

## The client/server boundary

`server/` is an npm workspace of this repository, so npm hoists its dependencies into the root `node_modules` and nothing physically stops a Vue component from importing a server-only package.
ESLint enforces the split:

- `src/` may not import from `server/src/` at all.
- `server/` may import **types** from anywhere in `src/` (`import type`).
  Type-only imports are erased at compile time, so they add no runtime coupling.
- `server/` may import **values** only from the two shared contracts: `src/fs/fs.ts` (the `FS` interface, `FileEntry`, and what counts as a user's own file) and `src/history/policy.ts` (when a save is worth recording).

The rule is a list of forbidden directories rather than "all of `src/` except those two": these globs follow .gitignore semantics, where a pattern matches a path segment anywhere plus everything beneath it and negation does not re-admit a descendant.
Add a line when `src/` grows a top-level directory.

A second guard covers what lint cannot express: `server/tsconfig.json` omits the `DOM` lib, so importing a browser module such as `src/fs/opfs.ts` fails `npm run typecheck:server` with `Cannot find name 'navigator'`.
That is an error rather than a warning, so it blocks `npm run validate`.

The ESLint rules above are *warnings*, since the repository uses `eslint-plugin-only-warn` (#154).
`npm run lint` runs with `--max-warnings 0`, which is what makes them binding.

## Constraints

**Routes must stay versioned and backward-compatible.**
`scripts/deploy` rsyncs each front-end release into its own directory (`scamper.cs.grinnell.edu/3.5.0/`) and `scripts/update-latest` only moves a redirect, so every past release stays reachable at its URL indefinitely.
One server therefore serves many client versions at once, and already-deployed clients never update.
Ship a breaking change as `/api/v2` beside `/api/v1`.

**Recording is decided twice.**
The client settles the common case from its cached head without sending a request.
When it cannot rule a save out, the server re-applies the same predicate — the same module, `src/history/policy.ts` — against what it holds, and its answer wins.

**The server stamps snapshot times, not the client.**
A history spans a student's machines, and a laptop running ten minutes fast would otherwise sort its snapshots above ones taken later elsewhere.

**`fileExists` is a hot path**, as `src/fs/opfs.ts` documents: module resolution, import steps, and the `file-exists?` primitive a student can call in a loop.
`src/fs/server.ts` caches file names, refreshing them on each listing and updating them on its own writes, so a warm `fileExists` makes no request.

## Where the client half lives

- `src/fs/config.ts` reads the site-root `/config.json` that names the server.
  Any failure means "no server, stay on local storage", which is the common case: a `npm run dev` checkout has no config at all.
- `src/fs/server.ts` is `ServerFileSystem`, the `FS` implementation that talks to these routes.
- `src/history/server.ts` is `ServerHistory`, the `History` implementation that talks to the history routes.
  `src/history/flat-file.ts` is the OPFS/CLI one.
- `src/app/web/ide-config.ts` holds the IDE's own settings — which file was open, which patch notes have been seen — and is **not** an `FS` client.
  It is per-machine state about a browsing session and lives in `localStorage`.
  Not to be confused with `src/fs/config.ts`, which reads the deployment's `/config.json`.
- `src/fs/index.ts` exposes `setBackend()`, which is the login/logout seam.
  It takes a file system and a history together, so a server file system can never end up paired with a flat-file history writing `.{filename}.history` blobs into the server's file storage.
  `src/app/web/server-session.ts` chooses which one at startup, and `SignInModal.vue` is the sign-in dialog.

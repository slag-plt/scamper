# Scamper server

The back end that stores a user's files, so they survive browser-storage loss
and follow the user between machines (issue #357).

Run it with `npm run dev:server` from the repository root. `PORT` overrides the
default of 3000.

> **Storage is currently a stub.** `src/store.ts` keeps files in memory, in one
> shared namespace, with no authentication — enough for the client seam in
> `src/fs/server.ts` to talk to something real. MariaDB-backed per-user storage
> behind BetterAuth replaces it; the routes are the part meant to survive that
> swap. Do not deploy this as-is.

## The API

Every route mirrors one method of the `FS` interface, so the two stay in step.
A filename is a single percent-encoded path segment.

| Method   | Path                    | Meaning                                  |
| -------- | ----------------------- | ---------------------------------------- |
| `GET`    | `/api/v1/health`        | liveness, and which API version this is  |
| `GET`    | `/api/v1/fs/files`      | `{ files: FileEntry[] }`, previews included |
| `GET`    | `/api/v1/fs/files/{name}` | `{ contents }`, or 404                 |
| `PUT`    | `/api/v1/fs/files/{name}` | save `{ contents }`, creating if needed |
| `DELETE` | `/api/v1/fs/files/{name}` | delete, or 404                         |
| `POST`   | `/api/v1/fs/rename`     | `{ from, to }`, overwriting `to`         |

The listing carries each file's preview because computing them client-side
costs one request per file. `rename` is one route rather than a copy-then-delete
pair so an interruption cannot leave a user with two copies or none.

## Cross-origin

`ALLOWED_ORIGIN` names the single origin permitted to call the server with
credentials. Leave it unset when the server and the static site share an origin,
which sends no CORS headers at all. It is one origin rather than a list because
`Access-Control-Allow-Origin` cannot be `*` once the client sends cookies.

## Why this lives in the Scamper repo

`server/` is an npm workspace of the main repository rather than a separate
`scamper-server` repo. The reasons:

- **One definition of the contract.** `src/fs/fs.ts` is six methods plus
  `FileEntry`, and the server exists to implement exactly that interface
  per-user. Adding a method should be one PR touching the interface, the OPFS
  implementation, the server implementation, and the route together.
- **Node code already lives here.** `src/app/cli/` and `src/fs/node.ts` are
  Node-targeted, so a server is not a foreign artifact in this tree.
- **One onboarding path.** `CONTRIBUTING.md` takes a student from zero web
  experience to a first change in a week; a second repository would double the
  clone/install/CI/PR surface that guide has to cover.

This would be worth revisiting if the server were ever operated by campus IT
rather than the research group, or if student contributors should not hold
commit access to production authentication code.

## The client/server boundary

Because this is a workspace, npm hoists its dependencies into the root
`node_modules` — nothing *physically* stops a Vue component from importing a
server-only package. ESLint is what keeps the split real:

- `src/` may not import from `server/` at all.
- `server/` may import **types** from anywhere in `src/` (`import type`).
  Type-only imports are erased at compile time, so they add no runtime coupling
  and cannot drag browser code into the server.
- `server/` may import **values** only from `src/fs/fs.ts`, the contract both
  halves implement. Sharing it is what keeps this backend and OPFS agreeing on
  questions like what "hidden" means, rather than each carrying its own copy of
  the answer.

A second guard covers what lint cannot express: `tsconfig.json` here omits the
`DOM` lib, so importing a browser module — `src/fs/opfs.ts`, say — fails
`npm run typecheck:server` with `Cannot find name 'navigator'`. That is an
error, not a warning, so it blocks `npm run validate` outright.

Note these are ESLint *warnings*, since the repo uses `eslint-plugin-only-warn`
(see issue #154), so CI will not fail on a violation. Watch for them in review.

## Two constraints that are easy to get wrong

**Routes must stay versioned and backward-compatible.** `scripts/deploy` rsyncs
each front-end release into its own directory
(`scamper.cs.grinnell.edu/3.5.0/`) and `scripts/update-latest` only moves a
redirect, so every past release stays reachable at its URL indefinitely. One
server therefore serves many client versions at once. Being in a monorepo does
*not* mean both sides change atomically — already-deployed clients never get the
update. Ship a breaking change as `/api/v2` beside `/api/v1`.

**`fileExists` is a hot path.** `src/fs/opfs.ts` documents it as such: module
resolution, import steps, and the `file-exists?` primitive a student can call in
a loop. A naive port that makes one request per call turns a student's loop into
a network round-trip per iteration. `src/fs/server.ts` therefore caches file
names, refreshing them on each listing and updating them on its own writes, so a
warm `fileExists` makes no request at all.

## Where the client half lives

- `src/fs/config.ts` reads the site-root `/config.json` that names the server.
  Any failure means "no server, stay on local storage" — the common case, since
  a `npm run dev` checkout has no config at all.
- `src/fs/server.ts` is `ServerFileSystem`, the `FS` implementation that talks
  to these routes.
- `src/fs/index.ts` already exposes `setFS()`, which is the login/logout seam.
  Nothing switches automatically yet: a configured server only means one is
  available to log in to, and the login UI is still to come.

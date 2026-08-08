# Scamper server

The back end that stores a user's files, so they survive browser-storage loss
and follow the user between machines (issue #357).

Run it with `npm run dev:server` from the repository root. `PORT` overrides the
default of 3000.

Usually you want both halves at once:

```console
npm run dev:full
```

That starts this server and a front end wired to it, and the IDE then keeps
files here instead of in the browser. See **Running the two halves together**
below for how they are connected, and why it is done that way.

## Configuration

| Variable              | Meaning                                                        |
| --------------------- | -------------------------------------------------------------- |
| `DATABASE_URL`        | `mysql://user:pass@host:3306/scamper` — where files are kept    |
| `BETTER_AUTH_SECRET`  | signs sessions; `openssl rand -base64 32`                       |
| `BETTER_AUTH_URL`     | the origin Scamper is served from                               |
| `PORT`                | defaults to 3000                                                |
| `ALLOWED_ORIGIN`      | only for a split-origin deployment (see below)                  |
| `SCAMPER_STUB`        | `1` to run in memory with no sign-in — development only         |

**A server with no `DATABASE_URL` refuses to start.** It could instead fall back
to the in-memory store, but that store has no sign-in and one shared namespace,
so a deployment that lost its configuration would quietly serve every student
the same pile of files. Failing to start is the safer of the two, and
`SCAMPER_STUB=1` is how a front-end contributor asks for the in-memory one on
purpose (`npm run dev:full` sets it).

## Setting up a database

BetterAuth owns the `user`, `session`, `account`, and `verification` tables and
its CLI creates them; ours reference `user`, so it goes first:

```console
export DATABASE_URL='mysql://root:secret@127.0.0.1:3306/scamper'
export BETTER_AUTH_SECRET="$(openssl rand -base64 32)"
export BETTER_AUTH_URL='http://localhost:5173'
npm run db:migrate --workspace @scamper/server
```

`files`, `histories`, and `snapshots` are ours, and `src/db.ts` applies
`schema.sql` at every start. Every statement there is `IF NOT EXISTS`, so that
is a no-op once done, and a fresh checkout needs no separate step.

A MariaDB to point it at, if you have Docker:

```console
docker run -d --name scamper-mariadb -e MARIADB_ROOT_PASSWORD=secret \
  -e MARIADB_DATABASE=scamper -p 3306:3306 mariadb:11
```

## Sign-in

Email and password, via BetterAuth mounted at `/api/auth/*`. That method needs
no third-party registration, so a contributor can create an account against a
local database and exercise the whole flow offline. Adding an identity provider
later — campus Google, say — is configuration in `src/auth.ts` plus a button in
the login form; nothing downstream cares, because everything downstream keys off
`session.user.id`.

Every route but `/api/v1/health` needs a session and answers **401** without
one. That check lives in `src/api.ts` rather than in the HTTP layer, so the rule
is stated where the routes are and a test can pin it.

> **Not yet production-ready.** Nothing verifies an email address, so anyone who
> can reach the server can create an account. That gate — an allowlist, campus
> SSO, or mail-backed verification — has to be decided before this is exposed.

## The API

Every route mirrors one method of the `FS` interface, so the two stay in step.
A filename is a single percent-encoded path segment. Everything below is scoped
to the signed-in user, and answers 401 without a session (except `health`).

| Method   | Path                              | Meaning                                     |
| -------- | --------------------------------- | ------------------------------------------- |
| `GET`    | `/api/v1/health`                  | liveness, and which API version this is     |
| `GET`    | `/api/v1/fs/files`                | `{ files: FileEntry[] }`, previews included |
| `GET`    | `/api/v1/fs/files/{name}`         | `{ contents }`, or 404                      |
| `PUT`    | `/api/v1/fs/files/{name}`         | save `{ contents }`, creating if needed     |
| `DELETE` | `/api/v1/fs/files/{name}`         | delete, or 404                              |
| `POST`   | `/api/v1/fs/rename`               | `{ from, to }`, overwriting `to`            |
| `GET`    | `/api/v1/history/files`           | files with a history, and deletion marks    |
| `GET`    | `/api/v1/history/files/{name}`    | snapshot **times**, newest first            |
| `GET`    | `/api/v1/history/files/{name}/{id}` | one version's `{ contents }`, or 404      |
| `POST`   | `/api/v1/history/files/{name}`    | record `{ contents, force }`                |
| `DELETE` | `/api/v1/history/files/{name}`    | mark deleted, keeping the snapshots         |
| `POST`   | `/api/v1/history/rename`          | `{ from, to }`                              |
| *        | `/api/auth/*`                     | BetterAuth: sign-up, sign-in, sign-out, session |

The listing carries each file's preview because computing them client-side
costs one request per file. `rename` is one route rather than a copy-then-delete
pair so an interruption cannot leave a user with two copies or none.

The history routes deliberately do **not** mirror the file routes. Listing and
indexing answer with times and deletion marks only; contents come one version at
a time from `files/{name}/{id}`. A history holds up to fifty copies of a file, so
shipping them all to draw a column of timestamps would undo the reason snapshots
are stored as rows. See `schema.sql` for the queries these stand in for.

## Running the two halves together

`npm run dev:full` starts this server and a front end pointed at it. It is
`scripts/dev-full.mjs`, and it is exactly these two commands, so run them in
separate terminals instead if you prefer:

```console
npm run dev:server
npm run dev -- --mode server
```

`--mode server` does two things (see `vite.config.ts`):

1. **Proxies `/api` to this server**, so the browser only ever talks to the Vite
   origin.
2. **Serves a `/config.json`** naming `/api/v1`, which is how the client learns
   there is a server at all. A plain `npm run dev` has no such file, gets a 404,
   and stays on local storage — unchanged from before any of this existed.

The proxy is the part worth understanding. The obvious alternative — point the
client straight at `localhost:3000` — makes local development *cross-origin*
while production is *same-origin*, and everything cookie-shaped then differs
between the two: `SameSite` has to be `none` (which requires HTTPS, painful
locally), CORS has to be configured and can silently work in dev but not
production or vice versa, and the credentialed-request path you test is not the
one you ship. Proxying makes a dev checkout single-origin exactly as production
is, so there is one behaviour to reason about.

`SCAMPER_SERVER_PORT` moves this server (and the proxy that follows it) off
3000.

By default `dev:full` sets `SCAMPER_STUB=1`, so the back end runs in memory with
no sign-in — which is what someone working on the front end wants. Set
`DATABASE_URL` (and the two `BETTER_AUTH_*` variables) in the environment you run
it from and it uses the database and real sessions instead.

`src/app/web/dev-backend.ts` switches the IDE onto the server when
`/config.json` is present, and is compiled out of any build that is not a
`--mode server` dev server. That guard is why the stub cannot escape into a
release: were a production build to switch on the mere presence of a config, a
single `npm run deploy:server-url` would put every student into the same pile of
files. Signing in is what will move a user's files — the login UI is still to
come, so in database mode the IDE currently gets 401s until one exists.

## Cross-origin

Scamper is deployed with the static site and this server on **one origin**, so
`/api/v1` is a path on the same host the IDE is served from. That is the
arrangement `npm run dev:full` reproduces, and it means no CORS headers are sent
at all, and session cookies can stay `SameSite=Lax` and same-origin.

`ALLOWED_ORIGIN` exists for a split-origin deployment, and names the single
origin permitted to call the server with credentials. It is one origin rather
than a list because `Access-Control-Allow-Origin` cannot be `*` once the client
sends cookies. Leave it unset for the same-origin deployment above.

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

- `src/` may not import from `server/src/` at all.
- `server/` may import **types** from anywhere in `src/` (`import type`).
  Type-only imports are erased at compile time, so they add no runtime coupling
  and cannot drag browser code into the server.
- `server/` may import **values** only from the two shared contracts:
  `src/fs/fs.ts` (the `FS` interface, `FileEntry`, and what counts as a user's
  own file) and `src/history/policy.ts` (when a save is worth recording).
  Sharing them is what keeps the backends agreeing on questions like what
  "hidden" means or how long the merge window is, rather than each carrying its
  own copy of the answer.

The rule is written as a list of forbidden directories rather than "all of `src/`
except those two", because these globs follow .gitignore semantics: a pattern
matches a path segment anywhere plus everything beneath it, and negation does not
re-admit a descendant. Add a line when `src/` grows a top-level directory.

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

**Recording is decided twice, on purpose.** The client settles the common case
from its cached head and never sends a request: autosave fires every few seconds
while a student types, and almost none of those firings deserve an entry. When
the client cannot rule a save out, the server re-applies the same predicate --
literally the same module, `src/history/policy.ts` -- against what it actually
holds, and its answer wins.

**The server stamps snapshot times, not the client.** A history now spans a
student's machines, and a laptop running ten minutes fast would otherwise sort
its snapshots above ones taken later elsewhere.

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
- `src/history/server.ts` is `ServerHistory`, the `History` implementation that
  talks to the history routes. `src/history/flat-file.ts` is the OPFS/CLI one.
- `src/app/web/ide-config.ts` is the IDE's *own* settings — which file was open,
  which patch notes have been seen — and is deliberately **not** an `FS` client.
  It is per-machine state about a browsing session, so it lives in
  `localStorage`: as a file it would follow the user between machines (opening a
  laptop would inherit what the lab computer had open) and cost a write to the
  server on every tab hide. Not to be confused with `src/fs/config.ts`, which is
  the deployment's `/config.json`.
- `src/fs/index.ts` exposes `setBackend()`, which is the login/logout seam. It
  takes a file system and a history together so a server file system can never
  end up paired with a flat-file history -- that combination would write
  `.{filename}.history` blobs into the server's file storage, which is exactly
  the layout the database replaces. Nothing switches automatically yet: a
  configured server only means one is available to log in to, and the login UI is
  still to come.

# Scamper server

The back end that stores a user's files, so they survive browser-storage loss
and follow the user between machines (issue #357).

Run it with `npm run dev:server` from the repository root. `PORT` overrides the
default of 3000.

Usually you want both halves at once:

```console
npm run dev:memory
```

That starts this server and a front end wired to it, and the IDE then keeps
files here instead of in the browser. See **Running the two halves together**
below for how they are connected, and why it is done that way.

## Configuration

| Variable                   | Meaning                                                        |
| -------------------------- | -------------------------------------------------------------- |
| `DATABASE_URL`             | `mysql://user:pass@host:3306/scamper` — where files are kept    |
| `BETTER_AUTH_SECRET`       | signs sessions; `openssl rand -base64 32`                       |
| `BETTER_AUTH_URL`          | the origin Scamper is served from, **port included**            |
| `SCAMPER_TRUSTED_ORIGINS`  | further origins allowed to sign in, comma-separated; empty in a real deployment |
| `PORT`                     | defaults to 3000                                                |
| `SCAMPER_STUB`             | `1` to run in memory with no sign-in — development only         |

`BETTER_AUTH_URL` is the one that bites. It is the list of origins a session may
be created from, so if it does not match the browser's address bar exactly —
port and all — **sign-in alone** fails with `Invalid origin` while everything
else works, which makes it look like a password problem. `server-up` prints the
current value for exactly this reason. `SCAMPER_TRUSTED_ORIGINS` adds more, so
one stack can serve both its own origin and Vite's dev origin without edits.

There is nothing to configure for sign-in beyond the secret: accounts are made
by hand (see below), and there is no identity provider or mail transport.

**A server with no `DATABASE_URL` refuses to start.** It could instead fall back
to the in-memory store, but that store has no sign-in and one shared namespace,
so a deployment that lost its configuration would quietly serve every student
the same pile of files. Failing to start is the safer of the two, and
`SCAMPER_STUB=1` is how a front-end contributor asks for the in-memory one on
purpose (`npm run dev:memory` sets it).

## Running it, with its database

`docker-compose.yml` in the repository root is how this server is meant to run,
in development and in production alike:

```console
cp .env.example .env         # fill in the passwords and the secret
scripts/server/server-up
```

That brings up MariaDB, waits for it to be genuinely ready, creates BetterAuth's
tables, starts the server, starts Caddy in front of it, and waits until the
whole chain answers. It is also the upgrade:

```console
git pull && scripts/server/server-up --build
```

`--build` matters after any change to `server/` *or* the front end: the images
hold *copies* of both, so editing the source changes nothing until they are
rebuilt.

### Four containers, one origin

| Service   | What it is                                                     |
| --------- | -------------------------------------------------------------- |
| `db`      | MariaDB. Its port is deliberately unpublished                  |
| `migrate` | BetterAuth's CLI; runs to completion at every start, then exits |
| `server`  | this API, on loopback only                                      |
| `web`     | Caddy: serves the built front end, proxies `/api` to `server`   |

`web` is the only one a browser talks to, and that is the design: the app and
its API share an origin, so the session cookie is first-party. No CORS, no
`SameSite=None`, no CSRF check to write, and nothing that breaks when browsers
tighten third-party cookie policy. Splitting the two across hosts costs all of
that at once — which is why the front end is built into an image here
(`Dockerfile.web`) rather than deployed somewhere else and pointed at this API.

`web` also answers `/config.json` with `{"serverUrl": "/api/v1"}` — see
`Caddyfile`. That is how the IDE learns there is a server; a static deployment
has no such file and stays on browser storage.

### Patching the front end alone

The front end is **baked into the `web` image**, so a change to it needs a
rebuild. `server-up --build` does that, but it recreates *every* container —
migrations re-run and the API restarts — which is far more than a stylesheet fix
deserves. So:

```console
scripts/server/web-update
```

That rebuilds only the front-end image and swaps only that container
(`--no-deps`), leaving the API's uptime, the database, and everyone's session
untouched. About ten seconds, of which Caddy is down for one.

If even that is too much, `docker-compose.override.yml.example` switches `web`
to serving a directory on the host:

```console
cp docker-compose.override.yml.example docker-compose.override.yml
scripts/server/server-up          # once, to apply the mount
```

From then on, putting new files in `dist/` — `npm run build`, or an rsync from
elsewhere — is live immediately, with no container touched at all. `web-update`
notices the mount and rebuilds `dist/` instead of an image.

The trade is real: what is being served no longer corresponds to any image, so
`git log` stops describing what students are running, and a half-written `dist/`
is live the moment it lands. Prefer `web-update` unless you specifically want
the live directory.

Note that `docker-compose.override.yml` is a file Compose loads **only when no
`-f` is passed**. The scripts in `scripts/server/` name their compose files
explicitly, so they list the override too when it exists — otherwise any of them
would quietly recreate `web` without the mount.

```console
scripts/server/server-down          # stop; the database is kept
scripts/server/server-down --wipe   # stop and destroy every account and file
scripts/server/server-dump          # dumps/scamper-<timestamp>.sql
```

Each is `docker compose` underneath (`up -d`, `down`, `down -v`, and
`mariadb-dump` through `exec`) with the guard rails the bare commands lack: a
`.env` check, a wait for health, and a typed confirmation before anything
irreversible.

**`down` does not delete data.** The database lives in the named volume
`scamper-db`, which survives it; only `--wipe` (`down -v`) removes it, and there
is no undo. Take a `server-dump` first.

There is deliberately no deployment script. A script would have to encode how
the server is started, restarted after a crash, and pointed at its database --
which is what the compose file already says, in a form that runs.

### Migrations

Two sets of tables, in this order:

1. **BetterAuth's** (`user`, `session`, `account`, `verification`). It owns them
   and its CLI creates them. The `migrate` service runs that CLI to completion
   before the server starts, so `up` does it for you. It is additive and skips
   what exists, so it is a no-op on every start after the first.
2. **Ours** (`files`, `histories`, `snapshots`, in `schema.sql`), which reference
   `user`. `src/db.ts` applies them at every start; every statement is
   `IF NOT EXISTS`.

The CLI is a separate build stage because it drags in Prisma, Drizzle, and a
native SQLite binding for databases we do not use. Worth carrying in a container
that runs for two seconds at deploy time; not in the one serving requests.

### About the `npm audit` alerts on that CLI

`npm audit` reports a **critical** advisory against `better-auth` and a **high**
one against `drizzle-orm`, both reachable from `@better-auth/cli`. They are
known, and the answer is to leave them alone. As of 2026-08-11:

- **The server is not affected.** It runs `better-auth@1.6.26`; the advisory
  covers `<= 1.6.21`. What is flagged is a *second, older copy* --
  `better-auth@1.4.21`, nested inside `@better-auth/cli@1.4.21` -- which drags
  `drizzle-orm@0.41.0` along with it.
- **Nothing we run reaches the vulnerable code.** Every one of those advisories
  is in an OAuth, OIDC, magic-link, SCIM, or organization flow. This server has
  no identity provider and no plugins: email and password, and accounts made by
  hand.
- **It is not in the serving image.** The CLI is a devDependency, so
  `npm ci --omit=dev` leaves it out; it exists only in the `migrate` stage, in a
  container that runs once at deploy time and exits.
- **Accounts do not use it.** `admin.ts` goes through BetterAuth's runtime
  internals -- the 1.6.26 copy -- not the CLI. Creating and resetting accounts
  never touches the flagged tree.

There is no newer *stable* `@better-auth/cli` to upgrade to: 1.4.21 is latest,
and the only thing past it is a 1.5.0 beta. **Do not pin that beta to silence
the alert** -- it would put a pre-release in the deployment path to fix
something that cannot be reached.

Revisit when a stable CLI past 1.4.21 ships, which is the clean fix and costs
nothing. Revisit sooner if this server ever gains a social provider or one of
the plugins named above, because that is the assumption the whole analysis rests
on.

The alternative, if the alerts become intolerable, is to drop the CLI and write
BetterAuth's four tables into `schema.sql` beside our three. That removes the
dependency entirely, at the cost of having to notice when BetterAuth changes its
own schema -- which is exactly what the CLI is doing for us.

### Without Docker

The server is a plain Node process, so it runs directly too — set the variables
above and `npm run start:server`, having run
`npm run db:migrate --workspace @scamper/server` once against your database.

## Accounts

Email and password, via BetterAuth mounted at `/api/auth/*`. Two constraints
shape everything about it:

- **No identity provider.** Institutional SSO is out on compliance grounds, so
  Scamper holds the credential itself.
- **No mail server.** Every flow that would normally send mail is therefore
  unavailable: no self-service sign-up, no address verification, no reset links.

So **sign-up is off** and an administrator makes each account, passing the
password to its owner directly. Against the compose stack — which is how this
normally runs — use the scripts in `scripts/server/`:

```console
scripts/server/user-add ada@example.edu "Ada Lovelace"   # prints a password
scripts/server/user-list
scripts/server/user-info ada@example.edu    # who they are, and how much they have
scripts/server/user-rename ada@example.edu "Ada Lovelace"   # the shown name
scripts/server/user-chpwd ada@example.edu   # new password, ends all sessions
scripts/server/user-delete ada@example.edu  # and their files, irreversibly
```

Each runs `server/src/admin.ts` inside the container, because that is the only
place the database is reachable from — compose does not publish its port. Where
the database *is* reachable (no Docker), the same commands are:

```console
npm run account -- create ada@example.edu "Ada Lovelace"
npm run account -- chpwd  ada@example.edu
npm run account -- list
```

The password is generated when not given, from an alphabet with no `0`/`O` or
`1`/`l`/`I` — these get read aloud and written down by someone who did not
choose them. **It is shown once.** Passwords are stored hashed, so a lost one is
replaced rather than recovered, which is what `reset` is for; it also ends that
person's sessions, since the usual reason to reset is that someone else may know
the old one.

This runs against the database, not the running server. There is deliberately no
privileged HTTP route: no account can be made over the network at all.

`accounts.ts` writes through BetterAuth's own internals — its hasher, its
adapter — so the rows are exactly the ones a sign-up would have written, and
signing in cannot tell the difference.

`/api/v1/auth/methods` reports what the server offers, so the login form can say
so rather than assume.

Every route but `/api/v1/health` needs a session and answers **401** without
one. That check lives in `src/api.ts` rather than in the HTTP layer, so the rule
is stated where the routes are and a test can pin it.

One exception, and it matters: when the **database** is unreachable, those
routes answer **503** rather than 401. Reading a session is itself a query, so a
database outage makes every request look unauthenticated — and a bare 401 would
tell a student their session had ended and offer them a sign-in that could not
work either. `health` reports the database for the same reason, so the IDE's
heartbeat sees the outage and goes into its offline state instead.

## The API

Every route mirrors one method of the `FS` interface, so the two stay in step.
A filename is a single percent-encoded path segment. Everything below is scoped
to the signed-in user, and answers 401 without a session (except `health`).

| Method   | Path                              | Meaning                                     |
| -------- | --------------------------------- | ------------------------------------------- |
| `GET`    | `/api/v1/health`                  | liveness (**503** if the database is away)  |
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

`npm run dev:memory` starts this server and a front end pointed at it. It is
`scripts/dev-memory.mjs`, and it is exactly these two commands, so run them in
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

By default `dev:memory` sets `SCAMPER_STUB=1`, so the back end runs in memory with
no sign-in — which is what someone working on the front end wants. Everything is
lost when it stops, and everyone shares one namespace.

### Developing against a real database

Let compose run the back end, and run only the front end yourself — worth it
when you are iterating on the front end, since the compose copy is a build and
has no hot reload:

```console
cp .env.example .env
scripts/server/server-up
scripts/server/user-add you@example.com "Your Name"
npm run dev -- --mode server
```

Then sign in at :5173 with what that printed. The third step is not optional:
there is no sign-up, so without an account the IDE has nothing to sign in with.

For this to work, :5173 has to be an origin the server accepts a session from —
`.env.example` puts it in `SCAMPER_TRUSTED_ORIGINS` for that reason, alongside
`BETTER_AUTH_URL` pointing at the stack's own :8080. Without it, sign-in fails
with `Invalid origin` while every other request succeeds.

Use `npm run dev` here rather than `dev:memory`: compose is already running the
back end, and `dev:memory` would start a second one on the same port.

The alternative, if you would rather run the server from your terminal, is to
publish a database for it and set `DATABASE_URL` plus the two `BETTER_AUTH_*`
variables in the environment `dev:memory` runs in.

### What decides the file system

`src/app/web/server-session.ts` picks it before the app mounts: no
`/config.json` means local storage, a server the user is not signed in to means
local storage plus an offer to sign in, and a signed-in user means their files
on the server. A server reporting no sign-in methods is the stub above, which
has no accounts, so it is used directly.

Signing in or out reloads rather than swapping the file system mid-session.

## Deploying

Everything runs on one host, from one command. The host needs Docker with
Compose v2 and git, and **nothing else** — no Node, no build tooling: the images
build the front end themselves.

```console
git clone <this repo> scamper && cd scamper
cp .env.example .env          # then fill it in -- see below
scripts/server/server-up --build
scripts/server/user-add ada@example.edu "Ada Lovelace"
```

That is the whole deployment. It brings up MariaDB, waits for it, runs
BetterAuth's migrations to completion, starts the API, starts Caddy in front of
it, and waits until the whole chain answers — then prints the URL.

### Filling in `.env`

Four values matter, and one of them is the usual source of trouble:

| Variable | What it must be |
| --- | --- |
| `MARIADB_PASSWORD`, `MARIADB_ROOT_PASSWORD` | letters and digits only — they go into a connection URL as-is |
| `BETTER_AUTH_SECRET` | `openssl rand -base64 32` |
| `BETTER_AUTH_URL` | **the origin a browser will show**, scheme and port included |
| `WEB_PORT` | the port the platform forwards public traffic to |

`BETTER_AUTH_URL` is the one. It is the list of origins a session may be created
from, so if it does not match the address bar exactly, **sign-in alone** fails
with `Invalid origin` while everything else works — which reads as a password
problem and is not. If the platform terminates TLS and forwards to the
container, the browser sees `https://`, so this must say `https://` too, even
though Caddy itself is serving plain HTTP inside.

`WEB_PORT` is whatever the host forwards public traffic to, usually `80`.
`SERVER_PORT` stays on loopback and needs nothing.

### TLS

The `Caddyfile` binds `:80` and does not request a certificate, which is right
when the platform in front terminates TLS — the common case on a managed host.

To have Caddy get its own certificate instead, replace `:80` in the `Caddyfile`
with the hostname and publish 443 as well. It will handle Let's Encrypt itself,
provided the name resolves to the host and both ports are reachable.

### Upgrades

```console
git pull && scripts/server/server-up --build     # everything
scripts/server/web-update                        # front end only
```

`--build` is not optional after a code change: the images hold *copies* of
`server/` and of the built front end. `web-update` is the one to reach for when
only the front end changed — it swaps that container alone, leaving the API's
uptime, the database, and everyone's session untouched.

Take a dump first if the change touches storage:

```console
scripts/server/server-dump          # dumps/scamper-<timestamp>.sql
```

### If the host cannot build the images

The front-end build (`npm ci` plus Vite) is the memory-hungriest step here, and
a small managed environment can run out. The way around it is to build
elsewhere and ship `dist/`:

```console
cp docker-compose.override.yml.example docker-compose.override.yml
scripts/server/server-up            # once, to apply the mount
rsync -a dist/ host:scamper/dist/   # from a machine that can build
```

Caddy then serves that directory directly. The cost is that what is running no
longer corresponds to any image — see **Patching the front end alone** above.

### The static deployment is separate, and still works

`npm run deploy` still rsyncs a build to a plain web server, where the absence
of a `/config.json` keeps the IDE on browser storage: no accounts, no server,
nothing to run. That deployment and this one are independent, and a site can
offer both — the static one as the no-account Scamper, this one for students
with accounts.

**Do not use `npm run deploy:server-url` to point a static deployment at this
container's API.** It would make the front end and the API different origins,
which is precisely what this arrangement exists to avoid: it needs CORS,
`SameSite=None` cookies, a CSRF check the file routes do not have, and it breaks
as browsers restrict third-party cookies. That script is for a deployment where
one web server serves both the static files and `/api` on a single origin.
## Cross-origin

There is none, on purpose. Scamper is deployed with the static site and this
server on **one origin**, so `/api/v1` is a path on the host the IDE is served
from — the arrangement `npm run dev:memory` reproduces with a proxy. No reply
carries CORS headers, `OPTIONS` is a 405, and session cookies stay
`SameSite=Lax` and same-origin.

In production the `web` container is what makes that true: Caddy serves the
built front end and proxies `/api` here, so both halves answer on one hostname.

An `ALLOWED_ORIGIN` setting used to exist for a split-origin deployment. It is
gone: it was configuration nothing set, on a path nothing exercised, in the one
area where an untested path is worth least — a credentialed cross-origin reply
is exactly the thing to get wrong quietly. Serving this from a second origin
would mean putting it back deliberately, alongside `sameSite: 'none'` on the
session cookie and `trustedOrigins` in `src/auth.ts`.

And one more thing that is easy to miss: **`SameSite=Lax` is the only CSRF
protection the file routes have.** Nothing checks `Origin` on `/api/v1/*` —
it does not have to, because a cross-site request carries no cookie. Set
`sameSite: 'none'` and any page a student visits could `PUT` or `DELETE` their
files with their session attached, so an Origin allowlist would have to be added
in the same change. Two subdomains of one registrable domain are *same-site*
and avoid all of this; only a genuinely different site needs it.

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
  the layout the database replaces. `src/app/web/server-session.ts` chooses
  which one at startup, and `SignInModal.vue` is the sign-in dialog.

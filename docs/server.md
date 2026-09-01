# Scamper server

The back end that stores a user's files, so they survive browser-storage loss
and follow the user between machines (issue #357). The code is in `server/`, an
npm workspace of this repository; paths below are relative to the repository
root.

Run it with `npm run dev:server` from the repository root. `PORT` overrides the
default of 3000.

Usually you want both halves at once:

```console
npm run dev:memory
```

That starts this server and a front end wired to it; the IDE then keeps files
here instead of in the browser. See [Running the two halves
together](#running-the-two-halves-together) for how the two are connected.

## Configuration

| Variable                   | Meaning                                                        |
| -------------------------- | -------------------------------------------------------------- |
| `DATABASE_URL`             | `mysql://user:pass@host:3306/scamper` — where files are kept    |
| `BETTER_AUTH_SECRET`       | signs sessions; `openssl rand -base64 32`                       |
| `BETTER_AUTH_URL`          | the origin Scamper is served from, **port included**            |
| `SCAMPER_TRUSTED_ORIGINS`  | further origins allowed to sign in, comma-separated; empty in a real deployment |
| `PORT`                     | defaults to 3000                                                |
| `SCAMPER_STUB`             | `1` to run in memory with no sign-in — development only         |

`BETTER_AUTH_URL` is the common source of trouble. It is the list of origins a
session may be created from, so if it does not match the browser's address bar
exactly, port included, **sign-in alone** fails with `Invalid origin` while
everything else works. `server-up` prints the current value for this reason.
`SCAMPER_TRUSTED_ORIGINS` adds further origins, so one stack can serve both its
own origin and Vite's dev origin without edits.

Sign-in needs no configuration beyond the secret. Accounts are made by hand, and
there is no identity provider or mail transport.

**A server with no `DATABASE_URL` refuses to start.** Falling back to the
in-memory store would give a misconfigured deployment no sign-in and one shared
namespace, serving every student the same files. `SCAMPER_STUB=1` requests the
in-memory store explicitly; `npm run dev:memory` sets it.

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

`--build` is required after any change to `server/` *or* the front end: the
images hold copies of both, so editing the source changes nothing until they are
rebuilt. A deployment builds nothing and runs what CI published; see
[Deploying](#deploying).

### Four containers, one origin

| Service   | What it is                                                     |
| --------- | -------------------------------------------------------------- |
| `db`      | MariaDB. Its port is deliberately unpublished                  |
| `migrate` | BetterAuth's CLI; runs to completion at every start, then exits |
| `server`  | this API, on loopback only                                      |
| `web`     | Caddy: serves the built front end, proxies `/api` to `server`   |

`web` is the only container a browser talks to. The app and its API therefore
share an origin and the session cookie is first-party: no CORS, no
`SameSite=None`, no CSRF check to write, and nothing that breaks as browsers
tighten third-party cookie policy. Splitting the two across hosts costs all of
that, which is why the front end is built into an image here (`Dockerfile.web`)
rather than deployed elsewhere and pointed at this API.

`web` also answers `/config.json` with `{"serverUrl": "/api/v1"}`; see the root
`Caddyfile`. That is how the IDE learns there is a server. A static deployment
has no such file and stays on browser storage.

### Patching the front end alone

The front end is baked into the `web` image, so a change to it needs a rebuild.
`server-up --build` recreates every container, re-running migrations and
restarting the API. To rebuild and swap only the front end:

```console
scripts/server/web-update
```

That rebuilds only the front-end image and swaps only that container
(`--no-deps`), leaving the API's uptime, the database, and everyone's session
untouched. About ten seconds, of which Caddy is down for one.

`web-update` *builds*, so it belongs on a machine that can. On a host running
published images the same swap is two commands, and it pulls rather than builds:

```console
docker compose pull web
docker compose up -d --no-deps web
```

If even that is too much, `docker-compose.override.yml.example` switches `web`
to serving a directory on the host:

```console
cp docker-compose.override.yml.example docker-compose.override.yml
scripts/server/server-up          # once, to apply the mount
```

From then on, putting new files in `dist/` — `npm run build`, or an rsync from
elsewhere — is live immediately, with no container touched at all. `web-update`
notices the mount and rebuilds `dist/` instead of an image.

The cost is that what is served no longer corresponds to any image: `git log`
stops describing what students are running, and a half-written `dist/` is live
the moment it lands. Prefer `web-update` unless the live directory is wanted.

Compose loads `docker-compose.override.yml` **only when no `-f` is passed**. The
scripts in `scripts/server/` name their compose files explicitly, so they list
the override too when it exists; otherwise they would recreate `web` without the
mount.

```console
scripts/server/server-down          # stop; the database is kept
scripts/server/server-down --wipe   # stop and destroy every account and file
scripts/server/server-dump          # dumps/scamper-<timestamp>.sql
```

Each wraps `docker compose` (`up -d`, `down`, `down -v`, and `mariadb-dump`
through `exec`) with guard rails the bare commands lack: a `.env` check, a wait
for health, and a typed confirmation before anything irreversible.

**`down` does not delete data.** The database lives in the named volume
`scamper-db`, which survives it. Only `--wipe` (`down -v`) removes it, and there
is no undo; take a `server-dump` first.

There is no deployment script. How the server is started, restarted after a
crash, and pointed at its database is already stated by the compose file.

### Migrations

Two sets of tables, in this order:

1. **BetterAuth's** (`user`, `session`, `account`, `verification`). It owns them
   and its CLI creates them. The `migrate` service runs that CLI to completion
   before the server starts, so `up` does it for you. It is additive and skips
   what exists, so it is a no-op on every start after the first.
2. **Ours** (`files`, `histories`, `snapshots`, in `server/schema.sql`), which reference
   `user`. `server/src/db.ts` applies them at every start; every statement is
   `IF NOT EXISTS`.

The CLI is a separate build stage because it pulls in Prisma, Drizzle, and a
native SQLite binding for databases this server does not use. That is acceptable
in a container running for two seconds at deploy time, but not in the one
serving requests.

### The `npm audit` alerts on that CLI

`npm audit` reports a **critical** advisory against `better-auth` and a **high**
one against `drizzle-orm`, both reachable from `@better-auth/cli`. Both are
known and neither is actionable. As of 2026-08-11:

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
- **Accounts do not use it.** `server/src/admin.ts` goes through BetterAuth's runtime
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
BetterAuth's four tables into `server/schema.sql` beside our three. That removes the
dependency entirely, at the cost of having to notice when BetterAuth changes its
own schema -- which is exactly what the CLI is doing for us.

### Without Docker

The server is a plain Node process, so it runs directly too — set the variables
above and `npm run start:server`, having run
`npm run db:migrate --workspace @scamper/server` once against your database.

## Accounts

Email and password, via BetterAuth mounted at `/api/auth/*`. Two constraints
shape it:

- **No identity provider.** Institutional SSO is out on compliance grounds, so
  Scamper holds the credential itself.
- **No mail server.** Every flow that would normally send mail is therefore
  unavailable: no self-service sign-up, no address verification, no reset links.

**Sign-up is off.** An administrator creates each account and passes the
password to its owner directly. Against the compose stack, use the scripts in
`scripts/server/`:

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

A password is generated when not given, from an alphabet omitting `0`/`O` and
`1`/`l`/`I`, since these are read aloud and written down by someone who did not
choose them. **It is shown once.** Passwords are stored hashed, so a lost one is
replaced rather than recovered; `reset` does that, and also ends that person's
sessions.

These commands run against the database, not the running server. There is no
privileged HTTP route, so no account can be created over the network.

`server/src/accounts.ts` writes through BetterAuth's own internals — its hasher, its
adapter — so the rows are exactly the ones a sign-up would have written, and
signing in cannot tell the difference.

`/api/v1/auth/methods` reports what the server offers, so the login form can say
so rather than assume.

Every route but `/api/v1/health` requires a session and answers **401** without
one. The check lives in `server/src/api.ts` rather than the HTTP layer, so the
rule is stated where the routes are and a test can pin it.

One exception: when the database is unreachable, those routes answer **503**
rather than 401. Reading a session is itself a query, so a database outage makes
every request look unauthenticated, and a bare 401 would tell a student their
session had ended and offer a sign-in that could not work either. `health`
reports the database for the same reason, so the IDE's heartbeat sees the outage
and enters its offline state.

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

The listing carries each file's preview, since computing previews client-side
costs one request per file. `rename` is one route rather than a copy-then-delete
pair, so an interruption cannot leave a user with two copies or none.

The history routes do **not** mirror the file routes. Listing and indexing answer
with times and deletion marks only; contents come one version at a time from
`files/{name}/{id}`. A history holds up to fifty copies of a file, so shipping
them all to draw a column of timestamps would defeat storing snapshots as rows.
See `server/schema.sql` for the queries these stand in for.

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

The proxy is what keeps development and production alike. Pointing the client
straight at `localhost:3000` would make local development cross-origin while
production is same-origin, and every cookie-related behaviour would then differ:
`SameSite` would have to be `none`, which requires HTTPS; CORS would have to be
configured, and could work in development but not production or the reverse; and
the credentialed-request path under test would not be the one shipped. Proxying
makes a development checkout single-origin exactly as production is.

`SCAMPER_SERVER_PORT` moves this server (and the proxy that follows it) off
3000.

By default `dev:memory` sets `SCAMPER_STUB=1`, so the back end runs in memory
with no sign-in. Everything is lost when it stops, and everyone shares one
namespace.

### Developing against a real database

Let compose run the back end and run the front end directly, since the compose
copy is a build and has no hot reload:

```console
cp .env.example .env
scripts/server/server-up
scripts/server/user-add you@example.com "Your Name"
npm run dev -- --mode server
```

Then sign in at :5173 with the printed password. The third step is required:
there is no sign-up, so without an account the IDE has nothing to sign in with.

:5173 must be an origin the server accepts a session from. `.env.example` puts it
in `SCAMPER_TRUSTED_ORIGINS`, alongside `BETTER_AUTH_URL` pointing at the stack's
own :8080. Without it, sign-in fails with `Invalid origin` while every other
request succeeds.

Use `npm run dev` rather than `dev:memory` here: compose already runs the back
end, and `dev:memory` would start a second one on the same port.

To run the server from a terminal instead, publish a database for it and set
`DATABASE_URL` and the two `BETTER_AUTH_*` variables in the environment
`dev:memory` runs in.

### What decides the file system

`src/app/web/server-session.ts` picks it before the app mounts: no
`/config.json` means local storage, a server the user is not signed in to means
local storage plus an offer to sign in, and a signed-in user means their files
on the server. A server reporting no sign-in methods is the stub above, which
has no accounts, so it is used directly.

Signing in or out reloads rather than swapping the file system mid-session.

## Deploying

Everything runs on one host, from one command. The host needs Docker with
Compose v2 and git, and nothing else: no Node, no build tooling, and not enough
memory to build, since it runs the images CI published.

```console
git clone <this repo> scamper && cd scamper
cp .env.example .env          # then fill it in -- see below
scripts/server/server-up --pull always --no-build
scripts/server/user-add ada@example.edu "Ada Lovelace"
```

That is the whole deployment. It pulls the three images, brings up MariaDB,
waits for it, runs BetterAuth's migrations to completion, starts the API, starts
Caddy in front of it, waits until the chain answers, and prints the URL.

On a host that has never run a release, put `latest` in `SCAMPER_TAG` for the
first deployment and move it to `release` once one exists.

`server-up` passes flags through to `compose up`. `--pull always` takes the
published image rather than the host's existing copy. `--no-build` turns a
missing image into an error instead of a build; Compose builds any service
carrying a `build:` section whenever its tag is absent, and the front-end build
is what a small host runs out of memory doing. A host with memory to spare can
omit both and run `server-up --build` from the checkout.

### Where the images come from

`.github/workflows/node.js.yml` builds three images and pushes them to ghcr.io
on every push to main whose tests pass:

| Image | What it is |
| --- | --- |
| `ghcr.io/slag-plt/scamper-web` | Caddy and the built front end |
| `ghcr.io/slag-plt/scamper-server` | this API |
| `ghcr.io/slag-plt/scamper-migrate` | BetterAuth's migration CLI |

Every build is tagged `latest` and with its own commit. A build whose commit
changed the version in `package.json` — a release — is tagged with that version
as well, and moves `release`. `SCAMPER_TAG` in `.env` chooses which of them a
host follows, and so how often it deploys at all:

| `SCAMPER_TAG` | What the host runs | When it deploys |
| --- | --- | --- |
| `release` | the last version bump | when the version changes |
| `latest` | the head of main | every merge |
| `3.6.0`, or a commit | that build and no other | never, until the line changes |

`release` is the default, because of the version number students see.
`APP_VERSION` comes from `package.json`, and the IDE shows the patch notes
between the version a student last opened and the one they are opening now. A
host on `latest` would serve 3.6.0's breaking changes days before calling itself
3.6.0 and saying what changed, breaking a program mid-assignment with no
announcement attached. On `release` all three agree.

`latest` suits a staging host, or a server with no students on it.

To see what a floating tag currently points at:

```console
docker image inspect ghcr.io/slag-plt/scamper-web:release \
  -f '{{index .Config.Labels "org.opencontainers.image.version"}}'
```

The packages are public, so a host pulls without credentials. If they are ever
made private, the host needs one `docker login ghcr.io` with a token carrying
`read:packages`.

`latest` is a shared name. On a development machine, `docker compose pull`
overwrites whatever `server-up --build` last built, and a later `server-up` then
starts main's image rather than the working tree without saying so.
`SCAMPER_TAG=dev` in a local `.env` keeps the two apart.

Runners and the usual host are x86, so the published images are `linux/amd64`
only. On an ARM Mac they pull and run emulated; `server-up --build` is the better
local option.

### Filling in `.env`

Five values matter:

| Variable | What it must be |
| --- | --- |
| `MARIADB_PASSWORD`, `MARIADB_ROOT_PASSWORD` | letters and digits only — they go into a connection URL as-is |
| `BETTER_AUTH_SECRET` | `openssl rand -base64 32` |
| `BETTER_AUTH_URL` | **the origin a browser will show**, scheme and port included |
| `WEB_PORT` | the port the platform forwards public traffic to |
| `SCAMPER_TAG` | which images to follow — `release`, `latest`, or a version to pin |

`BETTER_AUTH_URL` is again the common source of trouble. It is the list of
origins a session may be created from, so if it does not match the address bar
exactly, **sign-in alone** fails with `Invalid origin` while everything else
works. If the platform terminates TLS and forwards to the container, the browser
sees `https://`, so this must say `https://` too, even though Caddy serves plain
HTTP inside.

`WEB_PORT` is whatever the host forwards public traffic to, usually `80`.
`SERVER_PORT` stays on loopback and needs nothing.

### TLS

The root `Caddyfile` binds `:80` and does not request a certificate, which is
correct when the platform in front terminates TLS, the common case on a managed
host.

To have Caddy obtain its own certificate instead, replace `:80` in the
`Caddyfile` with the hostname and publish 443 as well. Caddy handles Let's
Encrypt itself, provided the name resolves to the host and both ports are
reachable.

### Upgrades

```console
git pull                                            # what lives on the host
scripts/server/server-up --pull always --no-build   # everything
docker compose pull web                             # front end only
docker compose up -d --no-deps web
```

`git pull` updates the files the host reads for itself: the compose file, the
`Caddyfile`, the scripts. The code is not among them; it arrives in the images,
so the second line is the upgrade.

The last two are the pulled-image equivalent of `web-update`. They swap Caddy
alone and leave the API's uptime, the database, and every session untouched. Use
`web-update` itself only where the host can build.

On a pinned host, put the new version in `SCAMPER_TAG` and run the same
`server-up`. Rolling back is putting the old version back.

Take a dump first if the change touches storage:

```console
scripts/server/server-dump          # dumps/scamper-<timestamp>.sql
```

### Keeping up with main

`scripts/server/server-sync` runs the upgrade above under cron, so a release
reaches the server without anyone logging in:

```console
crontab -e
```

```crontab
PATH=/usr/local/bin:/usr/bin:/bin
*/5 * * * * /root/scamper/scripts/server/server-sync >> /var/log/scamper-sync.log 2>&1
```

It pulls what the host reads for itself, pulls the images, and deploys **only if
one of them moved**, comparing the image IDs behind the tags across the pull
since the tag itself reads the same either way. On the default `release`, merges
accumulate and a version bump is what ships. A run with nothing to do exits
silently, so the log holds one entry per deployment rather than one every five
minutes. When there is something to do, it dumps the database first, keeps the
last twenty dumps, and hands over to `server-up --no-build`.

A release whose images CI has not finished building is not an error: the pull
fails, the run reports it and stops, and the next run five minutes later finds
them.

Note that the host follows **main** for the files it reads itself and the
**release** for the code. A `Caddyfile` or compose change therefore lands as soon
as it is merged, against whatever release is running. That suits files about the
host rather than the program, but a change that only makes sense alongside the
code it ships with should go out with that release. Once releases carry a git
tag, the tighter version is to check out the release's commit rather than main's
head.

Compose decides the rest: a container whose image ID has not changed is left
alone, so a run that finds nothing restarts nothing.

Four things to get right on the host:

- **An HTTPS remote.** Cron has no ssh-agent: `git remote set-url origin
  https://github.com/slag-plt/scamper.git`. The repository and the packages are
  public, so nothing here needs credentials.
- **`PATH` in the crontab.** Cron's is short, and `docker` is usually not on it.
- **A checkout with no local edits.** `git pull --ff-only` refuses to run over
  them. `.env` and `docker-compose.override.yml` are gitignored and so are safe;
  a hand-patched `Caddyfile` is not, and the failure is loud in the log.
- **`SCAMPER_TAG=release`.** Pinning a version instead is the off switch — the
  pull then fetches the same digest every time and nothing is ever deployed.
  That, and putting the *previous* version there, is the rollback when a release
  turns out to be bad: no commands, no revert commit, live within five minutes.

Nothing pushes from CI. A deploy key in GitHub's secrets amounts to a shell on
the server, whereas polling needs nothing inbound: no key, no open port, no
webhook. The cost is up to five minutes of latency. If that becomes a problem,
add an Actions job that ssh's in against a key restricted to
`command="…/server-sync"`, so a leaked secret can only deploy.

### If the host cannot build the images

It does not have to; that is what the published images are for, and the
front-end build (`npm ci` plus Vite) is the most memory-hungry step in this
repository. This section covers running something CI has not published: an
unmerged branch, or a change not going to main.

Build it elsewhere, under the name the compose file expects, and ship it:

```console
docker buildx build --platform linux/amd64 \
  -f Dockerfile.web --target runtime \
  -t ghcr.io/slag-plt/scamper-web:local --load .
docker save ghcr.io/slag-plt/scamper-web:local | gzip > web.tar.gz
scp web.tar.gz host:                       # then, on the host:
#   gunzip -c web.tar.gz | docker load
#   SCAMPER_TAG=local in .env, and server-up --no-build
```

`--platform` is not optional from an ARM Mac: an arm64 image will not start on
an x86 host, and the failure is an exec-format error at run time rather than
anything at build time.

The other way is to leave the front end out of an image entirely and serve a
directory on the host:

```console
cp docker-compose.override.yml.example docker-compose.override.yml
scripts/server/server-up --pull always --no-build   # once, to apply the mount
rsync -a --delete --delay-updates dist/ host:scamper/dist/
```

Caddy then serves that directory directly, and putting new files there is the
whole update. `--delay-updates` is required: Caddy reads each file per request,
so a half-transferred `dist/` is live while it transfers. The cost is that what
is running no longer corresponds to any image; see [Patching the front end
alone](#patching-the-front-end-alone).

### The static deployment

`npm run deploy` rsyncs a build to a plain web server, where the absence of a
`/config.json` keeps the IDE on browser storage: no accounts and no server. That
deployment and this one are independent, and a site can offer both — the static
one as the no-account Scamper, this one for students with accounts.

**Do not use `npm run deploy:server-url` to point a static deployment at this
container's API.** That would put the front end and the API on different origins,
which this arrangement exists to avoid: it requires CORS, `SameSite=None`
cookies, and a CSRF check the file routes do not have, and it breaks as browsers
restrict third-party cookies. That script is for a deployment where one web
server serves both the static files and `/api` on a single origin.

## Cross-origin

There is none. Scamper is deployed with the static site and this server on **one
origin**, so `/api/v1` is a path on the host the IDE is served from, which is the
arrangement `npm run dev:memory` reproduces with a proxy. No reply carries CORS
headers, `OPTIONS` is a 405, and session cookies stay `SameSite=Lax` and
same-origin.

In production the `web` container is what makes that true: Caddy serves the
built front end and proxies `/api` here, so both halves answer on one hostname.

An `ALLOWED_ORIGIN` setting for a split-origin deployment used to exist and has
been removed: it was configuration nothing set, on a path nothing exercised, and
a credentialed cross-origin reply is a poor thing to leave untested. Serving this
from a second origin means restoring it deliberately, alongside `sameSite:
'none'` on the session cookie and `trustedOrigins` in `server/src/auth.ts`.

**`SameSite=Lax` is the only CSRF protection the file routes have.** Nothing
checks `Origin` on `/api/v1/*`, because a cross-site request carries no cookie.
Under `sameSite: 'none'`, any page a student visits could `PUT` or `DELETE` their
files with their session attached, so an Origin allowlist would have to be added
in the same change. Two subdomains of one registrable domain are same-site and
avoid this; only a genuinely different site needs it.

## Why the server lives in this repository

`server/` is an npm workspace of the main repository rather than a separate
`scamper-server` repository:

- **One definition of the contract.** `src/fs/fs.ts` is six methods plus
  `FileEntry`, and the server exists to implement exactly that interface
  per-user. Adding a method should be one PR touching the interface, the OPFS
  implementation, the server implementation, and the route together.
- **Node code already lives here.** `src/app/cli/` and `src/fs/node.ts` are
  Node-targeted, so a server is not a foreign artifact in this tree.
- **One onboarding path.** `CONTRIBUTING.md` takes a student from zero web
  experience to a first change in a week; a second repository would double the
  clone/install/CI/PR surface that guide has to cover.

Revisit this if the server is ever operated by campus IT rather than the research
group, or if student contributors should not hold commit access to production
authentication code.

## The client/server boundary

Because this is a workspace, npm hoists its dependencies into the root
`node_modules`, so nothing physically stops a Vue component from importing a
server-only package. ESLint enforces the split:

- `src/` may not import from `server/src/` at all.
- `server/` may import **types** from anywhere in `src/` (`import type`).
  Type-only imports are erased at compile time, so they add no runtime coupling
  and cannot drag browser code into the server.
- `server/` may import **values** only from the two shared contracts:
  `src/fs/fs.ts` (the `FS` interface, `FileEntry`, and what counts as a user's
  own file) and `src/history/policy.ts` (when a save is worth recording).
  Sharing them keeps the backends agreeing on what "hidden" means and how long
  the merge window is, rather than each carrying its own copy of the answer.

The rule is written as a list of forbidden directories rather than "all of `src/`
except those two", because these globs follow .gitignore semantics: a pattern
matches a path segment anywhere plus everything beneath it, and negation does not
re-admit a descendant. Add a line when `src/` grows a top-level directory.

A second guard covers what lint cannot express: `server/tsconfig.json` omits the
`DOM` lib, so importing a browser module such as `src/fs/opfs.ts` fails `npm run
typecheck:server` with `Cannot find name 'navigator'`. That is an error rather
than a warning, so it blocks `npm run validate`.

The ESLint rules above are *warnings*, since the repository uses
`eslint-plugin-only-warn` (#154). `npm run lint` runs with `--max-warnings 0`,
which is what makes them binding.

## Constraints

**Routes must stay versioned and backward-compatible.** `scripts/deploy` rsyncs
each front-end release into its own directory (`scamper.cs.grinnell.edu/3.5.0/`)
and `scripts/update-latest` only moves a redirect, so every past release stays
reachable at its URL indefinitely. One server therefore serves many client
versions at once. A monorepo does not make both sides change atomically, since
already-deployed clients never get the update. Ship a breaking change as
`/api/v2` beside `/api/v1`.

**Recording is decided twice.** The client settles the common case from its
cached head without sending a request, since autosave fires every few seconds
while a student types and almost none of those firings deserve an entry. When the
client cannot rule a save out, the server re-applies the same predicate — the
same module, `src/history/policy.ts` — against what it holds, and its answer
wins.

**The server stamps snapshot times, not the client.** A history now spans a
student's machines, and a laptop running ten minutes fast would otherwise sort
its snapshots above ones taken later elsewhere.

**`fileExists` is a hot path**, as `src/fs/opfs.ts` documents: module resolution,
import steps, and the `file-exists?` primitive a student can call in a loop. One
request per call would turn that loop into a network round-trip per iteration.
`src/fs/server.ts` caches file names instead, refreshing them on each listing and
updating them on its own writes, so a warm `fileExists` makes no request.

## Where the client half lives

- `src/fs/config.ts` reads the site-root `/config.json` that names the server.
  Any failure means "no server, stay on local storage" — the common case, since
  a `npm run dev` checkout has no config at all.
- `src/fs/server.ts` is `ServerFileSystem`, the `FS` implementation that talks
  to these routes.
- `src/history/server.ts` is `ServerHistory`, the `History` implementation that
  talks to the history routes. `src/history/flat-file.ts` is the OPFS/CLI one.
- `src/app/web/ide-config.ts` holds the IDE's own settings — which file was open,
  which patch notes have been seen — and is **not** an `FS` client. It is
  per-machine state about a browsing session, so it lives in `localStorage`. As a
  file it would follow the user between machines, so opening a laptop would
  inherit what the lab computer had open, and it would cost a write to the server
  on every tab hide. Not to be confused with `src/fs/config.ts`, which reads the
  deployment's `/config.json`.
- `src/fs/index.ts` exposes `setBackend()`, which is the login/logout seam. It
  takes a file system and a history together so a server file system can never
  end up paired with a flat-file history -- that combination would write
  `.{filename}.history` blobs into the server's file storage, which is exactly
  the layout the database replaces. `src/app/web/server-session.ts` chooses
  which one at startup, and `SignInModal.vue` is the sign-in dialog.

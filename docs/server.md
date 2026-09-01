# Scamper server

The back end is a simple database that stores users' files.
The code is in `server/`, an npm workspace of this repository; paths below are relative to the repository root.

The server's documentation is spread out between three files:

+ `docs/server.md` (this file): configuration, execution, accounts, and the API.
+ `docs/server-deployment.md`: hosting and updates.
+ `docs/server-architecture.md`: architectural dec.isions and rationale

You can run an in-memory version of the server with  `npm run dev:server` from the repository root.
`PORT` overrides the default of 3000.

Commonly, you want to run both the front-end and back-end.
This is accomplished via `npm run dev:memory`.
That command starts this server (with an in-memory database) and a front-end wired to it.
See [Running the two halves together](#running-the-two-halves-together) for how the two are connected.

## Configuration

| Variable                   | Meaning                                                        |
| -------------------------- | -------------------------------------------------------------- |
| `DATABASE_URL`             | `mysql://user:pass@host:3306/scamper` — where files are kept    |
| `BETTER_AUTH_SECRET`       | signs sessions; `openssl rand -base64 32`                       |
| `BETTER_AUTH_URL`          | the origin Scamper is served from, **port included**            |
| `SCAMPER_TRUSTED_ORIGINS`  | further origins allowed to sign in, comma-separated; empty in a real deployment |
| `PORT`                     | defaults to 3000                                                |
| `SCAMPER_STUB`             | `1` to run in memory with no sign-in — development only         |


`BETTER_AUTH_URL` is the list of origins a session may be created from, so if it does not match the browser's address bar exactly, port included, **sign-in alone** fails with `Invalid origin` while everything else works.
`server-up` prints the current value for this reason.
`SCAMPER_TRUSTED_ORIGINS` adds further origins, so one stack can serve both its own origin and Vite's dev origin without edits.

Sign-in needs no configuration beyond the secret.
Accounts are made by hand via scripts found in the `/scripts` directory, and there is no identity provider or mail transport.

**A server with no `DATABASE_URL` refuses to start.**
`SCAMPER_STUB=1` requests the in-memory store explicitly; `npm run dev:memory` sets it.

## Running the server with a database

`docker-compose.yml` in the repository root is how this server is meant to run, in development and in production alike:

```console
cp .env.example .env         # fill in the passwords and the secret
scripts/server/server-up
```

This brings up MariaDB, waits for it to be genuinely ready, creates BetterAuth's tables, starts the server, starts Caddy in front of it, and waits until the whole chain answers.
Upgrading is accomplished via `server-up`:

```console
git pull && scripts/server/server-up --build
```

`--build` is required after any change to `server/` *or* the front end: the images hold copies of both, so editing the source changes nothing until they are rebuilt.
A deployment builds nothing and runs what CI published; see `docs/server-deployment.md`.

### Containers

| Service   | What it is                                                     |
| --------- | -------------------------------------------------------------- |
| `db`      | MariaDB. Its port is deliberately unpublished                  |
| `migrate` | BetterAuth's CLI; runs to completion at every start, then exits |
| `server`  | this API, on loopback only                                      |
| `web`     | Caddy: serves the built front end, proxies `/api` to `server`   |

`web` is the only container a browser talks to.
The app and its API share an origin, so the session cookie is first-party: no CORS, no `SameSite=None`, and no CSRF check to write.
The front end is built into an image here (`Dockerfile.web`) rather than deployed elsewhere and pointed at this API; see `docs/server-architecture.md`.

`web` also answers `/config.json` with `{"serverUrl": "/api/v1"}`; see the root `Caddyfile`.
That is how the IDE learns there is a server.
A static deployment has no such file and stays on browser storage.

### Migrating data

When migrating data, there are two sets of tables to consider, in this order:

1. **BetterAuth's** (`user`, `session`, `account`, `verification`).
   It owns them and its CLI creates them.
   The `migrate` service runs that CLI to completion before the server starts, so `up` does it for you.
   It is additive and skips what exists, so it is a no-op on every start after the first.
2. **Scamper's data** (`files`, `histories`, `snapshots`, in `server/schema.sql`), which reference `user`.
   `server/src/db.ts` applies them at every start; every statement is `IF NOT EXISTS`.

The CLI is a separate build stage: it pulls in Prisma, Drizzle, and a native SQLite binding for databases this server does not use, and is kept out of the image that serves requests.

### Implementation notes

`npm audit` reports a **critical** advisory against `better-auth` and a **high** one against `drizzle-orm`, both reachable from `@better-auth/cli`.
Both are known and neither is actionable.
As of 2026-08-11:

- **The server is not affected.**
  It runs `better-auth@1.6.26`; the advisory covers `<= 1.6.21`.
  What is flagged is a *second, older copy* -- `better-auth@1.4.21`, nested inside `@better-auth/cli@1.4.21` -- which drags `drizzle-orm@0.41.0` along with it.
- **Nothing we run reaches the vulnerable code.**
  Every one of those advisories is in an OAuth, OIDC, magic-link, SCIM, or organization flow.
  This server has no identity provider and no plugins: email and password, and accounts made by hand.
- **It is not in the serving image.**
  The CLI is a devDependency, so `npm ci --omit=dev` leaves it out; it exists only in the `migrate` stage, in a container that runs once at deploy time and exits.
- **Accounts do not use it.**
  `server/src/admin.ts` goes through BetterAuth's runtime internals -- the 1.6.26 copy -- not the CLI.
  Creating and resetting accounts never touches the flagged tree.

There is no newer *stable* `@better-auth/cli`: 1.4.21 is latest, and the only thing past it is a 1.5.0 beta.
**Do not pin that beta to silence the alert.**
Upgrade when a stable CLI past 1.4.21 ships, and re-check this if the server gains a social provider or one of the plugins named above.

### Running without Docker

The server is a plain Node process, so it runs directly too — set the variables above and `npm run start:server`, having run `npm run db:migrate --workspace @scamper/server` once against your database.

## Accounts

Accounts are simple email/password pairs, managed via BetterAuth mounted at `/api/auth/*`.
Scamper does not have a mail server to reduce complexity, so the server administrator must add accounts manually.
The administrator creates each account and passes the password to its owner directly.

Against the compose stack, use the scripts in `scripts/server/`:

```console
scripts/server/user-add ada@example.edu "Ada Lovelace"   # prints a password
scripts/server/user-list
scripts/server/user-info ada@example.edu    # who they are, and how much they have
scripts/server/user-rename ada@example.edu "Ada Lovelace"   # the shown name
scripts/server/user-chpwd ada@example.edu   # new password, ends all sessions
scripts/server/user-delete ada@example.edu  # and their files, irreversibly
```

Each runs `server/src/admin.ts` inside the container, because that is the only place the database is reachable from—compose does not publish its port.
Where the database *is* reachable (no Docker), the same commands are:

```console
npm run account -- create ada@example.edu "Ada Lovelace"
npm run account -- chpwd  ada@example.edu
npm run account -- list
```

A password is generated when not given, from an alphabet omitting `0`/`O` and `1`/`l`/`I`.
**It is shown once.**
Passwords are stored hashed, so a lost one is replaced rather than recovered; `reset` does that, and also ends that person's sessions.

These commands run against the database, not the running server.
There is no privileged HTTP route, so no account can be created over the network.

`server/src/accounts.ts` writes through BetterAuth's own internals — its hasher, its adapter — so the rows are exactly the ones a sign-up would have written, and signing in cannot tell the difference.

`/api/v1/auth/methods` reports what the server offers, so the login form can say so rather than assume.

Every route but `/api/v1/health` requires a session and answers **401** without one.
The check lives in `server/src/api.ts` rather than the HTTP layer, so the rule is stated where the routes are and a test can pin it.

One exception: when the database is unreachable, those routes answer **503** rather than 401.
Reading a session is itself a query, so a database outage would otherwise make every request look unauthenticated.
`health` reports the database too, so the IDE's heartbeat sees the outage and enters its offline state.

## The API

Every route mirrors one method of the `FS` interface, so the two stay in step.
A filename is a single percent-encoded path segment.
Everything below is scoped to the signed-in user, and answers 401 without a session (except `health`).

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

The listing carries each file's preview.
`rename` is one route rather than a copy-then-delete pair, so an interruption cannot leave a user with two copies or none.

The history routes do **not** mirror the file routes.
Listing and indexing answer with times and deletion marks only; contents come one version at a time from `files/{name}/{id}`.
A history holds up to fifty copies of a file, so a listing never carries contents.
See `server/schema.sql` for the queries these stand in for.

## Running the two halves together

`npm run dev:memory` starts this server and a front end pointed at it.
It is `scripts/dev-memory.mjs`, and it is exactly these two commands, so run them in separate terminals instead if you prefer:

```console
npm run dev:server
npm run dev -- --mode server
```

`--mode server` does two things (see `vite.config.ts`):

1. **Proxies `/api` to this server**, so the browser only ever talks to the Vite origin.
2. **Serves a `/config.json`** naming `/api/v1`, which is how the client learns there is a server at all.
   A plain `npm run dev` has no such file, gets a 404, and stays on local storage.

The proxy makes a development checkout single-origin exactly as production is, so cookie behaviour is the same in both.

`SCAMPER_SERVER_PORT` moves this server (and the proxy that follows it) off 3000.

By default `dev:memory` sets `SCAMPER_STUB=1`, so the back end runs in memory with no sign-in.
Everything is lost when it stops, and everyone shares one namespace.

### Developing against a real database

Let compose run the back end and run the front end directly, since the compose copy is a build and has no hot reload:

```console
cp .env.example .env
scripts/server/server-up
scripts/server/user-add you@example.com "Your Name"
npm run dev -- --mode server
```

Then sign in at :5173 with the printed password.
The third step is required: there is no sign-up, so without an account the IDE has nothing to sign in with.

:5173 must be an origin the server accepts a session from.
`.env.example` puts it in `SCAMPER_TRUSTED_ORIGINS`, alongside `BETTER_AUTH_URL` pointing at the stack's own :8080.
Without it, sign-in fails with `Invalid origin` while every other request succeeds.

Use `npm run dev` rather than `dev:memory` here: compose already runs the back end, and `dev:memory` would start a second one on the same port.

To run the server from a terminal instead, publish a database for it and set `DATABASE_URL` and the two `BETTER_AUTH_*` variables in the environment `dev:memory` runs in.

### Determining the file system

`src/app/web/server-session.ts` picks it before the app mounts: no `/config.json` means local storage, a server the user is not signed in to means local storage plus an offer to sign in, and a signed-in user means their files on the server.
A server reporting no sign-in methods is the stub above, which has no accounts, so it is used directly.

Signing in or out reloads rather than swapping the file system mid-session.

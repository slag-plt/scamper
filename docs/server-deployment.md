# Deploying the Scamper server

Putting the server on a host, and keeping it current. The reader here
administers a machine and need not touch the code: everything below runs the
images CI publishes.

`docs/server.md` covers configuring and running the server itself, and is worth
having open alongside this — in particular its Configuration table, since `.env`
is where the two meet.

## The first deployment

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

## Where the images come from

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

## Filling in `.env`

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

## TLS

The root `Caddyfile` binds `:80` and does not request a certificate, which is
correct when the platform in front terminates TLS, the common case on a managed
host.

To have Caddy obtain its own certificate instead, replace `:80` in the
`Caddyfile` with the hostname and publish 443 as well. Caddy handles Let's
Encrypt itself, provided the name resolves to the host and both ports are
reachable.

## Upgrades

```console
git pull                                            # what lives on the host
scripts/server/server-up --pull always --no-build   # everything
docker compose pull web                             # front end only
docker compose up -d --no-deps web
```

`git pull` updates the files the host reads for itself: the compose file, the
`Caddyfile`, the scripts. The code is not among them; it arrives in the images,
so the second line is the upgrade.

The last two swap Caddy alone, leaving the API's uptime, the database, and
every session untouched. See [Updating the front end
alone](#updating-the-front-end-alone).

On a pinned host, put the new version in `SCAMPER_TAG` and run the same
`server-up`. Rolling back is putting the old version back.

Take a dump first if the change touches storage:

```console
scripts/server/server-dump          # dumps/scamper-<timestamp>.sql
```

## Updating the front end alone

The front end is baked into the `web` image, so a change to it needs a rebuild.
`server-up --build` recreates every container, re-running migrations and
restarting the API. To rebuild and swap only the front end:

```console
scripts/server/web-update
```

That rebuilds only the front-end image and swaps only that container
(`--no-deps`), leaving the API's uptime, the database, and everyone's session
untouched. About ten seconds, of which Caddy is down for one.

`web-update` builds, so it belongs on a machine that can. A host running
published images pulls instead, with the two commands from
[Upgrades](#upgrades):

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

## Keeping up with main

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

## If the host cannot build the images

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
alone](#updating-the-front-end-alone).

## The static deployment

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

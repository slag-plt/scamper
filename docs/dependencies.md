# Dependencies and security alerts

Dependabot alerts on this repository fall into three families, and none of them
has yet been a vulnerability a student or a server was exposed to.
This is the standing explanation, so that triage is a lookup rather than an
investigation each time.

+   **The build toolchain** — `vite`, `vitest`, `undici`, and the transitives
    underneath them. These are ordinary patch bumps and are simply taken; see
    *Taking a toolchain bump* below.
+   **`better-auth` itself**, the running server's auth. Every advisory against
    it so far needs a plugin or a database this deployment does not use, and
    the pin on the 1.6 line means they cannot be upgraded away. Each one is
    checked against its preconditions and dismissed; see *The `better-auth`
    advisories*.
+   **The `@better-auth/cli` subtree**, a deploy-time tool. Described below.

## Check the scope first

Every alert carries a scope, and it decides how much the alert matters here:

```console
gh api repos/slag-plt/scamper/dependabot/alerts --paginate \
  -q '.[] | select(.state=="open") | [(.number|tostring), .security_advisory.severity, .dependency.package.name, .dependency.scope] | join(" | ")'
```

+   `runtime` — a package that reaches a built artefact. These matter; treat one
    as real until shown otherwise.
+   `development` — a build or deploy-time dependency. On this repository these
    are either the build toolchain, which is upgraded, or the
    `@better-auth/cli` subtree described below, which is not.

Scope is a starting point, not the answer. `brace-expansion`, `source-map-js`
and `zod` have all been reported as `runtime` while reaching nothing a browser
loads, and the `better-auth` advisories are `runtime` and genuinely about the
server's auth -- but turn out to need plugins it does not install. Read the
advisory's own "Am I affected?" conditions before either dismissing or acting.

## The `@better-auth/cli` subtree

`@better-auth/cli` is a `devDependency` of `@scamper/server`.
It exists for one command, `better-auth migrate`, which creates BetterAuth's
tables; `server/Dockerfile`'s `migrate` stage runs it and nothing else.

It drags a large tree behind it — `@mrleebo/prisma-ast` → `chevrotain` →
`lodash`, plus its own bundled copies of `better-auth` and `drizzle-orm` —
and that tree is where the alerts come from.

Three things make them harmless here, and it is worth checking each still holds
rather than assuming:

+   **It is not in the image that serves requests.** `server/Dockerfile`
    installs that one with `npm ci --omit=dev`, so none of the subtree is
    present. Only the `migrate` stage carries it, and that container runs for
    about two seconds at deploy and answers no network traffic.
+   **The flagged `better-auth` is not the one the server runs.** The CLI
    bundles an old copy. The server depends on `better-auth` directly, and it
    is that version which handles sign-in. Compare the advisory's range against
    the resolved *direct* dependency, not against whatever `npm ls` shows first:

    ```console
    npm ls better-auth --all      # the CLI's copy and the server's are different
    ```
+   **`drizzle-orm` is not a dependency of the server at all.** It appears only
    underneath the CLI. Nothing in `server/src` imports it.

## Why it is not simply upgraded

There is no stable version to move to.
`@better-auth/cli`'s `latest` tag has at times been *older* than the version the
lockfile resolves, with the next line published only as `1.5.0-beta`.
Forcing newer transitives through `overrides` would put `npm run db:migrate` at
risk — the only way to create BetterAuth's tables — in exchange for quieting
alerts about code that never runs in production.

If the CLI ever publishes a current stable release, taking it is the fix, and it
should retire most of this page.

## Why `better-auth` is held on the 1.6 line

`better-auth` is pinned with `~` rather than `^`, in both `package.json` and
`server/package.json`, so it takes patches and refuses the next minor.
The pin is load-bearing: the server cannot move ahead of the CLI above.

1.7 adds a required `issuer` column to the `account` table, with a unique index
on `(issuer, accountId)`, and credential sign-in filters on
`issuer = 'local:credential'` — as do `updatePassword` and
`findCredentialAccount`, which `accounts.ts` reaches through `internalAdapter`.
But `npm run db:migrate` derives its migration from the copy of `better-auth`
bundled inside `@better-auth/cli`, which is 1.4 and has no `issuer` at all.
A 1.7 server would therefore run against a schema its own migration tool cannot
produce, and every account row written under 1.6 would carry no issuer, so
every existing user would be locked out.

Waiting is not free, and the claim that used to stand here -- that every
`better-auth` advisory is fixed at 1.6.22 or below -- stopped being true. As of
November 2026 there are twelve open alerts against the `better-auth` family
whose first patched version is 1.7.0 or 1.7.7, three of them critical. What
makes them safe to carry is the configuration, not the version; see below.

Taking 1.7 is its own piece of work, and needs all of:

+   the `issuer` column and its unique index, added by something other than the
    CLI;
+   a backfill, `issuer = 'local:credential'` for every `providerId =
    'credential'` row, run before the new server starts;
+   `server/src/accounts.ts` updated for `createUser`'s second argument (the
    provisioning source) and `createAccount`'s new `issuer` field.

Note also that the front end imports `better-auth/client`
(`src/app/web/auth-client.ts`), so the root `package.json` declares
`better-auth` too. Keep the two ranges identical — they resolve to one hoisted
copy, and letting them drift installs two.

## The `better-auth` advisories

Every one so far is about a plugin or a database this deployment does not have.
`server/src/auth.ts` configures `emailAndPassword` and nothing else: there is
no `plugins` array and no `socialProviders`, and the database is MariaDB
through Kysely's `MysqlDialect`. Each advisory's own "Am I affected?" lists
conditions that must **all** hold, and the first one fails here:

| Advisory | Needs | Why not here |
| --- | --- | --- |
| `GHSA-965c-763c-88jm` (critical) — OAuth state usable as a Magic Link | the Magic Link plugin **and** a social or Generic OAuth provider | neither is installed; there is no `plugins` array and no `socialProviders` |
| `GHSA-r4xp-prcw-77qf` (high) — OAuth Proxy sign-in as another user | the OAuth Proxy plugin, cookie-backed state, implicit linking | the plugin is not installed, and state defaults to `database` with a database configured |
| `GHSA-q84f-53jg-9ppm` (high) — device approval hides the client | the opt-in `deviceAuthorization()` plugin | not installed; the advisory says outright that this is the whole condition |
| `GHSA-44jh-23m7-hpcf` (low) — rate limits exceeded under concurrency | database-backed rate limiting on **PostgreSQL** | the database is MariaDB, and rate-limit records default to memory; `@better-auth/drizzle-adapter` is not a dependency at all |

So these are dismissed individually, with the failing precondition as the
reason, rather than upgraded. **What would change that:** adding any plugin,
adding a social or Generic OAuth provider, moving to PostgreSQL, or enabling
database-backed rate limiting. Any of those turns one of the rows above into a
live vulnerability that the 1.6 pin cannot fix, and taking 1.7 — with the
`issuer` work above — becomes urgent rather than optional. Re-read this table
when `server/src/auth.ts` gains an option, not only when an alert arrives.

## Taking a toolchain bump

Dependabot opens a PR per package, and they all edit `package-lock.json`, so
merging them in sequence means a rebase per PR. One branch that takes them
together is less work and gives one validated state:

```console
npm update vite undici zod source-map-js brace-expansion
npm run validate && npm run build && npm run test:browser
```

`npm run validate` does not run the browser suite, so a `vitest`,
`@vitest/browser` or `playwright` bump is unverified without the third command
(CI's `browser-tests` job is the backstop).

The `vitest` family is a special case: its packages pin each other at an exact
version, so `npm update vitest` alone cannot move them and fails with
`ERESOLVE`. They have to go together, and npm will still anchor on the old
cluster recorded in the lockfile. Delete those entries and let it re-resolve:

```console
python3 - <<'EOF'
import json, collections
lock = json.load(open('package-lock.json'), object_pairs_hook=collections.OrderedDict)
pkgs = lock['packages']
for k in [k for k in pkgs if k == 'node_modules/vitest' or '@vitest/' in k]:
    del pkgs[k]
json.dump(lock, open('package-lock.json', 'w'), indent=2)
EOF
rm -rf node_modules/vitest node_modules/@vitest && npm install
```

A toolchain bump is invisible to a student, so it adds no patch-notes line.

## What is worth acting on

+   Any alert whose scope is `runtime`.
+   Any alert against `better-auth` whose range covers the version in
    `server/package.json`'s `dependencies` — that is the running server's auth,
    and it is a different question from the CLI's bundled copy.
+   Anything reported against a package that no longer has an importer.
    `showdown` was carried for a long time with no `import` anywhere, and simply
    deleting it retired three advisories.
+   Any alert against the build toolchain, which is upgraded rather than
    reasoned about — see *Taking a toolchain bump*. It is cheaper to take the
    patch than to argue that a dev-server flaw cannot be reached.

Prefer dismissing an individual alert with a reason over adding a blanket
`ignore` to `.github/dependabot.yml`.
An `ignore` entry silences a package by name, which would also hide a future,
genuine advisory against the `better-auth` the server actually runs.

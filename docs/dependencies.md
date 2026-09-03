# Dependencies and security alerts

Most Dependabot alerts on this repository are against a tool we run at deploy
time, not against anything a student or a server ever executes.
This is the standing explanation, so that triage is a lookup rather than an
investigation each time.

## Check the scope first

Every alert carries a scope, and it decides how much the alert matters here:

```console
gh api repos/slag-plt/scamper/dependabot/alerts --paginate \
  -q '.[] | select(.state=="open") | [(.number|tostring), .security_advisory.severity, .dependency.package.name, .dependency.scope] | join(" | ")'
```

+   `runtime` — a package that reaches a built artefact. These matter; treat one
    as real until shown otherwise.
+   `development` — a build or deploy-time dependency. On this repository these
    are almost always the `@better-auth/cli` subtree described below.

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

## What is worth acting on

+   Any alert whose scope is `runtime`.
+   Any alert against `better-auth` whose range covers the version in
    `server/package.json`'s `dependencies` — that is the running server's auth,
    and it is a different question from the CLI's bundled copy.
+   Anything reported against a package that no longer has an importer.
    `showdown` was carried for a long time with no `import` anywhere, and simply
    deleting it retired three advisories.

Prefer dismissing an individual alert with a reason over adding a blanket
`ignore` to `.github/dependabot.yml`.
An `ignore` entry silences a package by name, which would also hide a future,
genuine advisory against the `better-auth` the server actually runs.

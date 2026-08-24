# Scamper

A mini-Scheme implementation designed for teaching, targeting multimedia applications on the web.

## Running Scamper

Scamper runs in three arrangements, differing only in **where a student's files
live**. The IDE decides at startup by fetching `/config.json`: no such file
means the browser's own storage, and one naming a server means that server.
Nothing is compiled in, so the same build serves all three.

| | Files live in | Sign-in | Run it with |
| --- | --- | --- | --- |
| **Static** | the browser (OPFS) | — | `npm run dev` |
| **In-memory** | the server, until you stop it | — | `npm run dev:memory` |
| **Full stack** | MariaDB, per account | yes | `scripts/server/server-up` |

The first two are development arrangements and are npm scripts. The third is
how Scamper is actually deployed, and its interface is the scripts in
`scripts/server/` — those are the administrator's tools, not build steps.

### Static — the front end alone

~~~console
$> npm install
$> npm run dev            # http://localhost:5173
~~~

Files go to the browser's own storage (OPFS). No server, no accounts, nothing
else to run — this is what most work on Scamper needs, including everything in
the language implementation.

It is also a real deployment: `npm run build` produces `dist/`, which is a plain
directory of static files that any web server can host. Without a `config.json`
beside them they behave exactly as above. `npm run preview` serves that build
locally.

### In-memory — the server, without a database

~~~console
$> npm run dev:memory     # front end on :5173, back end on :3000
~~~

Runs both halves wired together, with the back end keeping everything in memory
(`SCAMPER_STUB=1`). The IDE uses the server's file system instead of the
browser's, so the server-backed paths get exercised — but there are no accounts,
everyone shares one namespace, and it is all lost when you stop it.

The middle ground: enough to work on the API, the file system, or save history
without Docker. Not enough to work on sign-in.

### Full stack — the deployment

~~~console
$> cp .env.example .env                                  # passwords and secret
$> scripts/server/server-up                              # everything, in Docker
$> scripts/server/user-add you@example.com "Your Name"   # prints a password
~~~

Then open the URL `server-up` prints and sign in. Three containers: MariaDB, the
API, and Caddy serving the built front end while proxying `/api` to the API.

**The whole app is one origin**, which is the point of the Caddy container: the
session cookie stays first-party, so there is no CORS, no `SameSite=None`, and
nothing to break when browsers restrict third-party cookies.

Two things bite here:

- **`--build` after changing anything.** The images hold *copies* of `server/`
  and of the built front end, so source edits do nothing until you rebuild:
  `scripts/server/server-up --build`. For a front-end-only change use
  `scripts/server/web-update`, which swaps just that container and leaves the
  API and database running.
- **`BETTER_AUTH_URL` must match the origin in your address bar, port and all.**
  Otherwise sign-in — and only sign-in — fails with `Invalid origin`. It is
  `http://localhost:8080` for the stack itself; `server-up` prints the current
  value. `SCAMPER_TRUSTED_ORIGINS` allows others, which is how the front end can
  also be run from Vite (`npm run dev -- --mode server`) against this same stack.

**There is no sign-up.** An administrator creates every account, because Scamper
has no mail server and so no way to verify an address or send a reset link.
`scripts/server/` holds the rest: `user-list`, `user-info`, `user-rename`,
`user-chpwd`, `user-delete`, plus `server-down` and `server-dump`. Each goes
through the container, because the database port is deliberately unpublished.

See [`server/README.md`](server/README.md) for the API, accounts, and deployment.

## Deployment

Two targets, matching the first and third arrangements above. They are
independent: a site can be deployed either way, or both at once.

What *triggers* a deployment is a release — a version bump on main, not every
merge. See [`RELEASING.md`](RELEASING.md).

### Static, to a plain web server

Builds Scamper and copies it to `<server>:<root>/<version>`, then writes
`<server>:<root>/index.html` redirecting to that version — so every past release
stays live at its own URL. Requires Mac/Linux and a `compsci` host pointing at
the web server.

~~~console
$> npm run deploy          # build and copy this version
$> npm run deploy:latest   # point the site root at it
~~~

Files land in the browser's storage, since no `config.json` is written. To point
such a deployment at a file server instead:

~~~console
$> npm run deploy:server-url -- https://scamper.example.edu/api/v1
$> npm run deploy:server-url          # no argument: back to local storage
~~~

That writes one `config.json` at the site root, which every deployed version
reads at startup — including releases shipped long before the server existed.

**A server on a different origin from the front end is not supported**, and that
is deliberate: it would mean CORS, `SameSite=None` cookies, a new CSRF check on
the file routes, and exposure to browsers restricting third-party cookies. Use
the full-stack deployment below, which serves both from one origin.

### Full stack, with Docker

~~~console
$> git pull && scripts/server/server-up --pull always --no-build
~~~

Pulls the images CI publishes from every green main and starts MariaDB, the API,
and Caddy. This is the whole deployment — there is no separate script, because
the compose file already says how everything starts, in a form that runs. A host
with the memory to build can use `--build` instead and skip the registry. See
[`server/README.md`](server/README.md).

## Information about the AST

The parser now outputs a `ParserOutput` object containing an abstract syntax tree in addition to the lowered scamper code. To access the tree, either run the
parser yourself and access the `ast` field of the `ParserOutput`, or if you have access to a `Scamper` object (one is created to run code in `ide.ts`), you can access
the `Scamper` object's `parseroutput.ast` field.

The `AST` object itself is defined in `ast.ts`, along with `SyntaxNode`. The tree holds a collection of nodes, and each node holds a textual description and a list
of children. Better documentation forthcoming.
# Scamper

A mini-Scheme implementation designed for teaching, targeting multimedia applications on the web.

## Development

To run a local instance of Scamper for development purposes:

~~~console
$> npm install    # Install NPM dependencies
$> npm run dev    # Spawns a local vite server to serve the application
~~~

Follow the terminal instructions to connect to your local Scamper instance.

That runs the front end alone, with your files in the browser's own storage.
It is what most work on Scamper needs, and it requires nothing else.

### Working on the file server

The file server (issue #357) keeps files per user instead, so they survive a
cleared browser and follow a student between machines. Two ways to run it:

**Without a database**, which is enough for front-end work:

~~~console
$> npm run dev:full   # Front end + the back end in server/, wired together
~~~

The IDE keeps files on the server, in memory, with no sign-in. Everything is
lost when you stop it, and everyone shares one namespace.

**With a real database and accounts**, for work on storage or sign-in:

~~~console
$> cp .env.example .env               # fill in the passwords and the secret
$> docker compose up -d               # MariaDB, migrations, and the server
$> docker compose exec server node_modules/.bin/tsx server/src/admin.ts \
     create you@example.com "Your Name"      # prints a password — keep it
$> npm run dev -- --mode server       # the front end, proxying /api to it
~~~

Then open the IDE and sign in with the address and password it printed.

Note the third step. **There is no sign-up**: an administrator creates every
account, because Scamper has no mail server and so no way to verify an address
or send a reset link. Without it there is nothing to sign in with, and the IDE
will sit at its sign-in dialog. `npm run account -- create ...` is the same
command when the database is reachable from your machine, but with the compose
stack it is not — the database port is deliberately unpublished.

Use `npm run dev`, not `dev:full`, alongside `docker compose up -d`: compose is
already running the back end, and `dev:full` would start a second one.

See [`server/README.md`](server/README.md) for the API, the account commands,
and deployment.

## Deployment

The deployment script builds Scamper and copies it to `<server>:<root>/<version>` and adds `<server>:<root>/index.html` which redirects to this latest version.
The deployment script also requires that you are on Mac/Linux and the `compsci` host points to the correct web server.

~~~console
$> npm run deploy   # Deploys Scamper
~~~

## Information about the AST

The parser now outputs a `ParserOutput` object containing an abstract syntax tree in addition to the lowered scamper code. To access the tree, either run the
parser yourself and access the `ast` field of the `ParserOutput`, or if you have access to a `Scamper` object (one is created to run code in `ide.ts`), you can access
the `Scamper` object's `parseroutput.ast` field.

The `AST` object itself is defined in `ast.ts`, along with `SyntaxNode`. The tree holds a collection of nodes, and each node holds a textual description and a list
of children. Better documentation forthcoming.
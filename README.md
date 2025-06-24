# Scamper

A mini-Scheme implementation designed for teaching, targeting multimedia applications on the web.

## Development

To run a local instance of Scamper for development purposes:

~~~console
$> npm install    # Install NPM dependencies
$> npm run dev    # Spawns a local vite server to serve the application
~~~

Follow the terminal instructions to connect to your local Scamper instance.

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
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
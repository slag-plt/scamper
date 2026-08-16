# The Scamper file server (issue #357).
#
# Built from the repository root, not from server/, because the server shares
# two modules with the front end -- src/fs/fs.ts and src/history/policy.ts --
# and sharing them is what keeps the two halves agreeing on what a hidden file
# is and when a save is worth recording. Build with:
#
#   docker compose build          (or: docker build -f server/Dockerfile .)

FROM node:22-alpine AS deps

WORKDIR /app

# Only the manifests first, so a change to source code does not re-run the
# install. `--omit=dev` and the workspace filter together mean the image gets
# better-auth, kysely, mysql2, and tsx -- not Vite, Vue, Playwright, or any of
# the front end's build tooling.
COPY package.json package-lock.json ./
COPY server/package.json ./server/
# --ignore-scripts skips the root postinstall, which generates the parser and
# library sources for the *front end* using devDependencies this image does not
# have. Nothing the server runs needs them.
#
# This image is larger than it looks like it should be: ~209 MB of node_modules,
# including Playwright, Vite, and Prisma. They arrive because better-auth
# declares nineteen optional peerDependencies (pg, better-sqlite3, drizzle,
# vitest, and so on, for the databases and frameworks it can integrate with),
# npm installs optional peers by default, and the lockfile therefore marks those
# packages `devOptional` -- reachable as a dev dependency *and* as an optional
# one. npm keeps such a package unless both are omitted.
#
# `--omit=optional` alongside `--omit=dev` does cut it to 57 MB and the server
# still starts -- but it also drops `@esbuild/linux-x64`, because esbuild ships
# its platform binaries as optional dependencies, and tsx needs esbuild. So it
# cannot be used while the runtime is tsx.
#
# The way out is to stop shipping tsx: compile to JavaScript in a build stage
# and run plain `node`, leaving only better-auth, kysely, and mysql2 here. That
# is a real change -- these are ESM sources whose relative imports carry no file
# extensions, so it needs a module-resolution decision, not just a `tsc` call --
# and it is worth doing deliberately rather than as a size tweak.
RUN npm ci --omit=dev --ignore-scripts --workspace @scamper/server

FROM node:22-alpine AS runtime

WORKDIR /app
ENV NODE_ENV=production

COPY --from=deps /app/node_modules ./node_modules
COPY package.json ./
COPY server/ ./server/

# The only parts of the front end the server may import. Copying these two
# directories rather than all of src/ keeps the boundary visible: an import
# from anywhere else in src/ fails the build here, which is the same rule
# eslint.config.mjs enforces at review time.
COPY src/fs/ ./src/fs/
COPY src/history/ ./src/history/

# Runs the TypeScript directly, as `npm run dev:server` does. The server is a
# few hundred lines with no build step of its own, and one way of running it in
# both places is worth more here than a compile stage.
USER node
EXPOSE 3000
CMD ["node_modules/.bin/tsx", "server/src/index.ts"]

# --- Migrations ---------------------------------------------------------------
#
# BetterAuth owns the user/session/account/verification tables and its CLI
# creates them; ours reference `user`, so this has to run first. It is a
# separate stage because that CLI drags in Prisma, Drizzle, and a native SQLite
# binding for the databases we do not use -- worth carrying for a container that
# runs for two seconds at deploy time, not for the one serving requests.
#
# docker-compose.yml runs this to completion before starting the server, so
# `docker compose up -d` is the whole deployment.
FROM node:22-alpine AS migrate

WORKDIR /app
COPY package.json package-lock.json ./
COPY server/package.json ./server/
# --ignore-scripts leaves better-sqlite3 uncompiled, which is fine: the CLI only
# reaches for a driver the configured database needs, and ours is MariaDB.
RUN npm ci --ignore-scripts --workspace @scamper/server

COPY server/ ./server/
COPY src/fs/ ./src/fs/
COPY src/history/ ./src/history/

# `-y` because there is no one to confirm at deploy time. The migration is
# additive and skips what already exists, so running it on every start is a
# no-op after the first.
CMD ["node_modules/.bin/better-auth", "migrate", "--config", "server/src/auth-config.ts", "-y"]

# Shared by the scripts in this directory. Sourced, not run.
#
# Every one of these drives the compose stack in docker-compose.yml, because
# that is how the server runs in development and in production alike. The
# database is deliberately not published to the host (see the compose file), so
# reaching it means going through a container -- which is exactly the awkward
# step these scripts exist to hide.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# Named explicitly so these work from any directory. Compose otherwise takes
# the project name from the working directory, and would invent a second,
# empty project when run from somewhere else.
#
# The override has to be listed too. Compose picks
# `docker-compose.override.yml` up on its own *only* when no `-f` is given, so
# naming the main file silently drops it -- and a script that ignores the
# override would recreate `web` without the bind mount someone deliberately
# added, undoing it with no message.
COMPOSE_FILES=(-f "${ROOT}/docker-compose.yml")
if [ -f "${ROOT}/docker-compose.override.yml" ]; then
  COMPOSE_FILES+=(-f "${ROOT}/docker-compose.override.yml")
fi

compose() {
  docker compose "${COMPOSE_FILES[@]}" "$@"
}

# Fails with something actionable rather than letting `exec` report a container
# name nobody has seen before.
require_server() {
  if [ -z "$(compose ps --status running --quiet server 2>/dev/null)" ]; then
    echo "The Scamper server is not running." >&2
    echo "Start it with: scripts/server/server-up" >&2
    exit 1
  fi
}

# Runs server/src/admin.ts inside the running server container.
#
# In the container because that is where the database is reachable from: it
# sits on compose's private network with no published port. `-T` keeps this
# usable in a pipe and from a script.
#
# stdin is closed on purpose. `exec` forwards it to the container, and admin.ts
# never reads it -- so without this, a command run before a prompt eats the
# answer that prompt was about to read, and user-delete's confirmation sees EOF.
admin() {
  require_server
  compose exec -T server node_modules/.bin/tsx server/src/admin.ts "$@" < /dev/null
}

# Asks before something irreversible. Answers other than the exact word are a
# no, so a stray return key cannot delete anyone's files.
confirm() {
  local prompt="$1" expected="$2" answer
  printf '%s\n' "${prompt}"
  printf 'Type %s to continue: ' "${expected}"
  read -r answer
  if [ "${answer}" != "${expected}" ]; then
    echo "Cancelled." >&2
    exit 1
  fi
}

# Prints usage and exits when a required argument is missing.
usage() {
  echo "Usage: $1" >&2
  exit 1
}

# Reads one setting from .env.
#
# The `|| true` is load-bearing. Under `set -euo pipefail` a grep that matches
# nothing fails its pipeline and aborts the whole script -- before any `${x:-…}`
# fallback can apply. That is not hypothetical: WEB_PORT is newer than most
# `.env` files, and without this a stack would come up and then the script would
# exit non-zero with nothing printed at all.
#
# @param 1 the variable's name
# @param 2 what to use when it is absent or empty
env_value() {
  local name="$1" fallback="$2" value
  # -f2- rather than -f2 so a value containing `=` survives intact.
  value="$(grep -E "^${name}=" "${ROOT}/.env" 2>/dev/null | cut -d= -f2- || true)"
  printf '%s' "${value:-${fallback}}"
}

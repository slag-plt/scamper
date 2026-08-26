#!/usr/bin/env bash

# Builds the Gradescope autograder image (issue #404): installs Node and a
# checkout of Scamper, so `run_autograder` has a CLI to run the harness with.
# Gradescope runs this once, as root, when the autograder is uploaded --- never
# per submission --- so everything slow belongs here rather than there.

set -euo pipefail

# Which Scamper to grade with. `main` is the latest; set this to a tag (say
# `v4.1.0`) to pin a semester, so a merge to main cannot change how work
# already submitted is graded.
SCAMPER_REF="${SCAMPER_REF:-main}"
SCAMPER_REPO="${SCAMPER_REPO:-https://github.com/slag-plt/scamper.git}"
SCAMPER_DIR=/autograder/scamper

apt-get update
apt-get install -y --no-install-recommends ca-certificates curl git

# The base image is Ubuntu 22.04, whose packaged Node is far too old for
# Scamper's toolchain; take Node 20 from NodeSource instead.
curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
apt-get install -y --no-install-recommends nodejs

git clone --depth 1 --branch "${SCAMPER_REF}" "${SCAMPER_REPO}" "${SCAMPER_DIR}"

# A full install, not --production: the CLI runs under `tsx` and Scamper's
# postinstall generates the parser and the library sources, both of which live
# in devDependencies.
cd "${SCAMPER_DIR}"
npm install

# Fail at build time, not on a student's first submission, if any of that went
# wrong.
echo '(+ 1 1)' > /tmp/scamper-smoke.scm
npx tsx src/app/cli/index.ts /tmp/scamper-smoke.scm
rm /tmp/scamper-smoke.scm

# Releasing

A release is a commit on main that changes `version` in `package.json`. Nothing
else marks one, and three things follow from it:

- CI tags the images it publishes with that version and moves `release`
  (`.github/workflows/node.js.yml`).
- Every host whose `.env` says `SCAMPER_TAG=release` deploys it within five
  minutes (`scripts/server/server-sync`).
- Every student whose last-seen version is older is shown the patch notes for it
  the next time they open the IDE (`src/app/web/patch-notes.ts`).

Ordinary merges do none of that. They collect on main and reach nobody until
someone cuts the next release, which keeps the version in the corner of the IDE,
the notes it shows, and the code behind them in agreement.

## Cutting one

A release is two edits: naming the pending patch notes, and bumping the version.

The notes are usually written already. They accumulate under the `next` entry in
`src/app/web/patch-notes.ts` as the work lands, so the release has not been named
yet. Read what is under `next` and decide the kind of release: `patch` for a fix,
`minor` for behaviour that is new or changed, `major` for a break students cannot
work around. Rename the entry to that version and leave a fresh, empty `next`
above it:

```ts
  {
    version: NEXT_RELEASE,
    notes: [
      // (the comment stays; the notes move down)
    ],
  },
  {
    version: '4.3.0',
    notes: [ ... ],
  },
```

The empty entry is required. Without it, the next two pull requests to add a note
each create a `next` entry of their own, and the `merge=union` rule in
`.gitattributes` keeps both instead of reporting a conflict.

Then the bump, using the same word:

```console
npm version minor --workspaces --include-workspace-root --no-git-tag-version
```

It writes `package.json`, `server/package.json`, and `package-lock.json`. All
three belong in the commit.

Open a pull request with those as its only changes. The `version` check confirms
that the three files agree, that the version rose, and that a minor or major
release has notes filed under its own number, which is what catches a forgotten
rename. A patch release may go without notes. Merge it, wait for `publish` to
finish, and hosts on `release` pick it up on their next sync.

## When a release turns out to be bad

Put the previous version in `SCAMPER_TAG` on the host. The next sync deploys it,
with no commands and no revert commit. The fix then goes out as a patch release.

## What makes the check binding

Actions only report; a ruleset is what blocks a merge. Two cover `main`
(**Settings → Rules**):

- **Main** — pull requests required, no force-pushes, no deleting the branch.
- **Release criteria** — `build (22.x)`, `database-tests`, `browser-tests`, and
  `version` all have to pass.

A required check has to *exist* on a pull request in order to pass, and the job
producing it comes from that branch's own copy of the workflow. A branch opened
before a check was added therefore stays blocked, showing nothing missing, until
main is merged into it.

## What is not automated

No git tag is made. The repository's existing tags disagree with each other:
`3.1.2` beside `v3.1.2`, and none at all for 3.5.0. Settling on a convention is a
separate decision, not something a version bump should do implicitly.

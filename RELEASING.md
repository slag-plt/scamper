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
someone decides the next release — which is the point: the version in the corner
of the IDE, the notes it shows, and the code behind them all say the same thing.

## Cutting one

The patch notes are usually written already. `patch-notes.ts` names the release
an entry belongs to, so notes are added as the work lands rather than gathered
at the end. What is left is the bump:

```console
npm version minor --workspaces --include-workspace-root --no-git-tag-version
```

`patch` for a fix, `minor` for behaviour that is new or changed, `major` for a
break students cannot work around. It writes three files — `package.json`,
`server/package.json`, and `package-lock.json` — and all three belong in the
commit.

Open a pull request with that as its only change. The `version` check reads it
and confirms the files agree, that the version went up, and that a minor or
major release has notes to show; a patch release may go without them, since a
bug fix does not deserve a modal in front of every student. Merge it, watch
`publish` finish, and hosts on `release` pick it up on their next sync.

## When a release turns out to be bad

Put the previous version in `SCAMPER_TAG` on the host. The next sync deploys it
— no commands, no revert commit — and the fix goes out as a patch release.

## What makes the check binding

Actions can only report; a ruleset is what stops a merge. Two of them cover
`main` (**Settings → Rules**):

- **Main** — pull requests required, no force-pushes, no deleting the branch.
- **Release criteria** — `build (22.x)`, `database-tests`, `browser-tests`, and
  `version` all have to pass.

One consequence is worth knowing before it puzzles someone: a required check has
to *exist* on a pull request to pass, and the job that produces it comes from
that branch's own copy of the workflow. So a branch opened before a check was
added sits blocked, showing nothing missing, until main is merged into it.

## What is not automated

No git tag is made. The tags this repository already has disagree with each
other — `3.1.2` beside `v3.1.2`, and none at all for 3.5.0 — so settling on a
convention is a decision to make deliberately, not something a bump should do
quietly on the way past.

# Releasing

A release is a commit on main that changes `version` in `package.json`.
Once this is done, the following actions occur:

- CI tags the images it publishes with that version and moves `release` (`.github/workflows/node.js.yml`).
- Every host whose `.env` says `SCAMPER_TAG=release` deploys it via the `server-sync` cronjob (`scripts/server/server-sync`).
- Every student whose last-seen version is older is shown the patch notes for it the next time they open the IDE (`src/app/web/patch-notes.ts`).

Note that ordinary merges do none of this.
They collect on main and reach nobody until someone cuts the next release.

## Process

A release is two edits: naming the pending patch notes, and bumping the version.

The notes are usually written already.
They accumulate under the `next` entry in `src/app/web/patch-notes.ts` as the work lands, so the release has not been named yet.
Read what is under `next` and decide the kind of release:

+   `patch`es are for bug fixes or refactoring work that does not impact user-facing interfaces/APIs.
+   `minor` releases are for significant changes to the user-facing experience, APIs, etc.
+   `major` releases are for between-semester updates that include significant revisions to functionality.

Rename the entry to that version and leave a fresh, empty `next` above it:

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

The empty entry is required.
Without it, the next two pull requests to add a note each create a `next` entry of their own, and the `merge=union` rule in `.gitattributes` keeps both instead of reporting a conflict.

## Landing several pull requests at once

`merge=union` is a rule in `.gitattributes`, which means it is a rule for *your*
git.
GitHub's own merge does not read it.
So a batch of pull requests that each append a note behaves differently than it
does locally: the first merges cleanly, and the moment it lands, every other one
reports a conflict on `src/app/web/patch-notes.ts`.

They have not really conflicted, and the fix is to resolve them where the rule
applies. For each one after the first:

```console
git -C <worktree> merge origin/main   # union settles patch-notes.ts here
git -C <worktree> push
gh pr merge <n> --squash
```

Then repeat for the next.
Each push restarts CI, so a batch lands one at a time rather than all at once.

None of this affects a single pull request, or concurrent work before it merges
— which is what the union rule is for.
It is only the second and later merges of a batch that need the step.

Then the bump, using the same release version:

```console
npm version minor --workspaces --include-workspace-root --no-git-tag-version
```

This writes `package.json`, `server/package.json`, and `package-lock.json`.
All three belong in the commit.

Open a pull request with those as its only changes.
The `version` check confirms that the three files agree, that the version rose, and that a minor or major release has notes filed under its own number, which is what catches a forgotten rename.
A patch release may go without notes.
Merge it, wait for `publish` to finish, and hosts on `release` pick it up on their next sync.

## Rolling back

If there's a need to rollback to a previous version, put the previous version in `SCAMPER_TAG` on the host.
The next sync deploys it, with no commands and no revert commit.
The fix then goes out as a patch release.

## CI/CD enforcement

Actions only report; a ruleset is what blocks a merge.
Two cover `main` (**Settings → Rules**):

- **Main** — pull requests required, no force-pushes, no deleting the branch.
- **Release criteria** — `build (22.x)`, `database-tests`, `browser-tests`, and `version` all have to pass.

A required check has to *exist* on a pull request in order to pass, and the job producing it comes from that branch's own copy of the workflow.
A branch opened before a check was added therefore stays blocked, showing nothing missing, until main is merged into it.

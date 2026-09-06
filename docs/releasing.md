# Releasing

A release is a commit on main that changes `version` in `package.json`.
Once this is done, the following actions occur:

- CI tags the images it publishes with that version and moves `release` (`.github/workflows/node.js.yml`).
- Every host whose `.env` says `SCAMPER_TAG=release` deploys it via the `server-sync` cronjob (`scripts/server/server-sync`).
- Every student whose last-seen version is older is shown the patch notes for it the next time they open the IDE (`patch-notes.md`).

Note that ordinary merges do none of this.
They collect on main and reach nobody until someone cuts the next release.

## Patch notes

The notes live in `patch-notes.md` at the root of the repository.
`src/app/web/patch-notes.ts` only parses it.

```markdown
# next

- A sentence a student would recognise.

# 4.3.0

> An optional headline for the release

- What shipped in 4.3.0.
```

A `# <version>` line starts an entry, a `- ` line is one note, and a `> ` line
directly under a heading is that release's headline.
Everything else — blank lines, the comment the file opens with — is ignored.

Notes are written as the work lands rather than gathered at release time, so
they accumulate under `# next` and no pull request has to guess which release it
belongs to.
One line per pull request, one sentence, written for a student in terms of what
they will notice.
Work a student cannot see — a refactor, a test, a CI change, contributor
documentation — adds no line; say so in the pull request instead, so the
omission reads as a decision.

**The text of a note is not Markdown**, despite the file's name.
`PatchNotesModal.vue` renders each one with `{{ item }}` — plain-text
interpolation, no Markdown pass — so a code span or a link reaches students
verbatim, backticks and all. That is what happened to the 4.1.1 notes; name
procedures bare. `test/regressions/patch-notes-plain-text.test.ts` pins it.

`.gitattributes` merges this file by union, so concurrent pull requests append
side by side instead of conflicting.
Nothing may depend on the order of notes within an entry, because that merge
decides it.
There is no other convention to remember: a bullet is a whole line, so two
appends cannot collide into something malformed the way two array elements
could.

## Previewing a release

A pull request that changes the version publishes a **client-side-only** build
of itself to GitHub Pages, and comments the URL:

```
https://slag-plt.github.io/scamper/4.4.0-rc.1/
```

That is the ordinary bundle, unmodified. The IDE decides where files live by
fetching `/config.json` at startup and falling back to browser storage when
there is none; the build emits no such file, and nothing serves one here. So
**signing in, server file storage, and history are not part of a preview**, and
a report that sign-in is broken there is a report about the preview rather than
about the release.

The site holds the release under review and nothing else.
Every candidate for it stays, so `rc.1` and `rc.2` can be compared, and the whole
set is cleared the moment the next release starts.
Past releases are not kept: the live app is where a shipped release is looked at.

### One-time setup

The `preview` job creates the `gh-pages` branch on its first run. Pages has to be
pointed at it once, in **Settings → Pages** (source: *Deploy from a branch*,
branch `gh-pages`, folder `/`), or equivalently:

```console
gh api -X POST repos/slag-plt/scamper/pages \
  -f 'source[branch]=gh-pages' -f 'source[path]=/'
```

## Process

A release is two edits: naming the pending patch notes, and bumping the version.

The notes are usually written already, accumulating under `# next`.
Read what is there and decide the kind of release:

+   `patch`es are for bug fixes or refactoring work that does not impact user-facing interfaces/APIs.
+   `minor` releases are for significant changes to the user-facing experience, APIs, etc.
+   `major` releases are for between-semester updates that include significant revisions to functionality.

Rename the heading to that version and leave a fresh, empty `# next` above it:

```markdown
# next

# 4.4.0

- (the notes stay where they are; only the heading above them changed)
```

The empty entry is required.
Without it, the next two pull requests to add a note each create a `# next` of
their own, and the union merge keeps both instead of reporting a conflict.

Then the bump. While the release is under review it carries a release-candidate
version, so that each preview has a URL of its own:

```console
npm version 4.4.0-rc.1 --workspaces --include-workspace-root --no-git-tag-version
```

Open a pull request with those changes as its only ones.
The `version` check rehearses the whole release — that `package.json`,
`server/package.json`, and both entries in `package-lock.json` agree, that the
version rose, and that a minor or major release has notes filed under its own
number — and then **fails, because a candidate is not a release**.
That is the gate. It stays red while the release is being previewed.

Re-cut as often as the review needs; each one publishes its own preview:

```console
npm version prerelease --workspaces --include-workspace-root --no-git-tag-version
```

When it is ready, drop the suffix:

```console
npm version 4.4.0 --workspaces --include-workspace-root --no-git-tag-version
```

The `version` check goes green, the pull request becomes mergeable, and merging
it releases.
Wait for `publish` to finish, and hosts on `release` pick it up on their next
sync.

A candidate never reaches main, so the images, the version shown in the IDE, and
the notes always agree.

## Landing several pull requests at once

`merge=union` is a rule in `.gitattributes`, which means it is a rule for *your*
git.
GitHub's own merge does not read it.
So a batch of pull requests that each append a note behaves differently than it
does locally: the first merges cleanly, and the moment it lands, every other one
reports a conflict on `patch-notes.md`.

They have not really conflicted, and the fix is to resolve them where the rule
applies. For each one after the first:

```console
git -C <worktree> merge origin/main   # union settles patch-notes.md here
git -C <worktree> push
gh pr merge <n> --squash
```

Then repeat for the next.
Each push restarts CI, so a batch lands one at a time rather than all at once.

None of this affects a single pull request, or concurrent work before it merges
— which is what the union rule is for.
It is only the second and later merges of a batch that need the step.

## Rolling back

If there's a need to rollback to a previous version, put the previous version in `SCAMPER_TAG` on the host.
The next sync deploys it, with no commands and no revert commit.
The fix then goes out as a patch release.

## CI/CD enforcement

Actions only report; a ruleset is what blocks a merge.
Two cover `main` (**Settings → Rules**):

- **Main** — pull requests required, no force-pushes, no deleting the branch.
- **Release criteria** — `build (22.x)`, `database-tests`, `browser-tests`, `server-smoke`, and `version` all have to pass.

`preview` is deliberately not among them.
A preview that fails to publish should not block a release.

Adding a check to that list is a two-step job, in this order: merge the pull request that adds the job first, then add the check.
The reason is the paragraph below — required *before* it exists on main blocks every open branch at once.
This is how `server-smoke` was added; the same commands take the next one, with its own name in place of `server-smoke`.

```console
gh api repos/slag-plt/scamper/rulesets/21264173 > /tmp/ruleset.json
jq '(.rules[] | select(.type == "required_status_checks")
      | .parameters.required_status_checks) += [{"context": "server-smoke"}]
    | {name, target, enforcement, conditions, rules}' /tmp/ruleset.json \
  > /tmp/ruleset-updated.json
gh api -X PUT repos/slag-plt/scamper/rulesets/21264173 --input /tmp/ruleset-updated.json
```

Or the same thing in **Settings → Rules → Release criteria**, which is less to get wrong.

A required check has to *exist* on a pull request in order to pass, and the job producing it comes from that branch's own copy of the workflow.
A branch opened before a check was added therefore stays blocked, showing nothing missing, until main is merged into it.

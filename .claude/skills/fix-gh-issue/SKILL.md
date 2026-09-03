---
name: fix-github-issue
description: Investigates a Github issue, constructs and validates a fix, and issues a pull request
---

Given a Github issue number:

1. Create a new branch labeled with the issue number and a short description of the issue (the title of the issue is sufficient if short enough) and perform your subsequent work in a new worktree with the same name as this branch.
2. Clean (`npm run clean`) and reinstall dependencies (`npm run i`) to ensure that the build environment is operational.
3. Verify that the Github issue is marked with the "Bug" type. Only work on issues marked with the "Bug" type unless instructed to work on the issue specifically.
4. Retrieve the Github issue description and formulate your own summary of the issue. Prompt the user if the problem is not clear.
5. Attempt to reproduce the issue. If you cannot reproduce the issue, prompt the user for additional information or to verify that the issue is non-reproducible. If the issue is marked non-reproducible, close the issue on Github accordingly.
6. Add a regression test to the regression suite (`test/regressions`) that captures the issue you observed. The regression test should fail initially, and your test should cause it to pass.
7. Investigate, diagnose, plan a fix, and implement your fix for the issue.
    + If your proposed fix requires substantial design changes, e.g., a new data structure or a restructuring of an API, prompt the user for approval and/or suggestions.
    + All fixes for issues must not cause any regressions in validation (`npm run validate`). If a regression occurs that cannot be resolved, prompt the user about how they would like to proceed.
8. If the fix changes what a user can see, add one line to the patch notes (`src/app/web/patch-notes.ts`) describing it, under the entry for the next version -- creating that entry if it is not there yet. See the Patch notes section of `CLAUDE.md`.
    + The line should be a single sentence, summarizing the fix, written in terms of what the user will notice rather than the implementation.
    + If a fix has no user-facing effect, then do not add a line to the patch notes. But make sure to note in the pull request the fix is not user-facing.
9. Spawn a sub-agent to perform an independent code review of your fix, make relevant changes, and validate your build (`npm run validate`) once you are done. Prompt the user if any fixes as a result of the code review result in significant changes to code that you did not implement.
10. If you implement a fully validated fix for the issue, file a pull request on Github for review. At this point, you are done and can move on to other tasks.

---
name: fix-gh-issues
description: Go through and fix current issues on Github.
---

1. If the user specifies an issue number, perform these steps on that one issue. Otherwise, go through the issue tracker on Github and identify all current bugs to be fixed. These are issues that marked with the "Bug" type. Any bugs that are marked with the labels "investigation" or "blocked" are bugs that are awaiting user input and should be removed from configuration.
2. Clean (`npm run clean`) and reinstall dependencies (`npm install`) to ensure that the build environment is operational.
3. For each bug spawn a subagent to do the following:
    + Create a new branch labeled with the issue number and a short description of the issue (the title of the issue is sufficient if short enough) and perform your subsequent work in a new worktree within `.claude/` with the same name as this branch.
    + Retrieve the Github issue description and comments for context. Formulate your own summary of the issue. Prompt the user if the problem is not clear or requires clarification.
    + Attempt to reproduce the issue. If you cannot reproduce the issue, prompt the user for additional information or to verify that the issue is non-reproducible. If the issue is marked non-reproducible, close the issue on Github accordingly.
    + Add a regression test to the regression suite (`test/regressions`) that captures the issue you observed. The regression test should fail initially, and your future implementation work should cause it to pass.
    + Investigate, diagnose, and create a plan to fix the bug.
    + If your proposed fix requires substantial design changes, e.g., a new data structure or a restructuring of an API, prompt the user for approval and/or suggestions.
    + All fixes for issues must not cause any regressions in validation (`npm run validate`). If a regression occurs that cannot be resolved, prompt the user about how they would like to proceed.
4. Inspect the individual plans for each bug and determine an overall implementation plan that minimizes potential conflicts between fixes while maximizing the potential for work with parallel subagents.
5. STOP - approval gate. Call EnterPlanMode. Write the complete implementation plan---consolidated versions of each individual issue's plan and the overall implementation plan for combining PRs to `.claude/sweep-plan.md`. For each individual issue plan, give a summary of the issue, summary of your proposed fix, list of the files affected, and a summary of the planned changes to each file. Then call ExitPlanMode and wait. Proceed to the next step only after the user accepts. Answers to AskUserQuestion are never approval — the tool resolves options inside a plan and does not authorize executing one. If the user rejects or amends, revise the plan file and call ExitPlanMode again.
6. Execute the approved (potentially modified) plan, spawning subagents as needed, indicated by the plan.
7. For each issue that you fix:
    + If the fix changes what a user can observe in the app, add one line to the patch notes (`src/app/web/patch-notes.ts`) describing it, under the entry for the next version -- creating that entry if it is not there yet. See the Patch notes section of `CLAUDE.md.
        - The line should be a single sentence, summarizing the fix, written in terms of what the user will notice rather than the implementation.
        - If a fix has no user-facing effect, then do not add a line to the patch notes. But make sure to note in the pull request the fix is not user-facing.
    + Spawn a sub-agent to perform an independent code review of your fix, make relevant changes, and validate your build (`npm run validate`) once you are done. Prompt the user if any fixes as a result of the code review result in significant changes to code that you did not implement.
    + Once completed and verified, file a pull request on Github for user review.
8. After all fixes have been completed, present the user with a final report summarizing the changes made as a result of fixing each issue and the pull requests for each issue, so that the user can review the changes.
9. At this point, if the user gives you approval to accept any of these PRs on their behalf, you may do so.
10. Finally, once the user says that you are done, please clean up all auxiliary data from this work including worktrees, local branches, and the `.claude/sweep-plan.md` file.

---
name: implement-gh-feature
description: Implement a feature specified in a Github issue.
---

Given a Github issue number:

1. Go to the issue tracker on Github and verify that the issue number is marked as a feature. Features are marked with the "Feature" type. If the issue is not a feature, STOP and ask the user whether they want to proceed by addressing the issue as if it was a feature.
2. Clean (`npm run clean`) and reinstall dependencies (`npm install`) to ensure that the build environment is operational.
3. Create a new branch labeled with the issue number and a short description of the issue (the title of the issue is sufficient if short enough) and perform your subsequent work in a new worktree with the same name as this branch.
4. Retrieve the Github issue description and comments for context. Formulate your own summary of the feature. Prompt the user if the problem is not clear or requires clarification.
5. Develop a plan for implementing the feature. Prompt the user to resolve any ambiguities in the feature's specification or non-trivial design decisions. Develop your plan with the following considerations:
    + Follow the existing structure and architecture of the existing codebase whenever possible.
    + Make architectural choices that favor conciseness and elegance first in implementation and extensibility second.
5. STOP - approval gate. Call EnterPlanMode. Write the complete implementation plan to `.claude/feature-plan.md`. Summarize the feature and the implementation plan. List the files affected and a summary of the planned changes to each file. Then call ExitPlanMode and wait. Proceed to the next step only after the user accepts. Answers to AskUserQuestion are never approval — the tool resolves options inside a plan and does not authorize executing one. If the user rejects or amends, revise the plan file and call ExitPlanMode again.
6. Execute the approved (potentially modified) plan, spawning subagents as needed, indicated by the plan.
7. Add a line to `patch-notes.ts` summarizing the feature, following the guidance found in `CLAUDE.md`.
8. Spawn a subagent to run an independent code review of your work. Resolve suggestions on your own, prompting the user when a resolution would result in a non-trivial change to the code or feature design.
8. Once completed and verified, file a pull request on Github for user review. Give a summary of your work to the user.
9. At this point, if the user gives you approval to accept any of these PRs on their behalf, you may do so.
10. Finally, once the user says that you are done, please clean up all auxiliary data from this work including worktrees, local branches, and the `.claude/feature-plan.md` file.

---
name: Fix Github issue
description: Investigates a Github issue, constructs and validates a fix, and issues a pull request
---

Given a Github issue number:

1. Verify that the Github issue is tagged as "Bug." Only work on issues tagged as "Bug".
2. Retrieve the Github issue description and formulate your own summary of the issue. Prompt the user if the problem is not clear.
3. Attempt to reproduce the issue. If you cannot reproduce the issue, prompt the user for additional information or to verify that the issue is non-reproducible. If the issue is marked non-reproducible, close the issue on Github accordingly.
4. Create a new branch labeled with the issue number and a short description of the issue (the title of the issue is sufficient if short enough) and perform your subsequent work in this branch.
5. Add a regression test to the regression suite (`test/regressions`) that captures the issue you observed. The regression test should fail initially, and your test should cause it to pass.
6. Investigate, diagnose, plan a fix, and implement your fix for the issue.
    + If your proposed fix requires substantial design changes, e.g., a new data structure, a restructuring of an API, prompt the user for approval and/or suggestions.
    + All fixes for issues must not cause any regressions in validation (`npm run validate`). If a regression occurs that cannot be resolved, prompt the user about how they would like to proceed.
7. Perform a code review of your fix, make relevant changes, and validate your build (`npm run validate`) once you are done. Prompt the user if any changes as a result of the code review result in significant changes to code that you did not implement.
8. If you implement a fully validated fix for the issue, file a pull request on Github for the issue and assign it to myself for review. At this point, you are done and can move on to other tasks.

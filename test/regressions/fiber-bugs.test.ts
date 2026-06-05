import { describe, expect, test } from "vitest"
import * as U from "../../src/lpm/util"
import { ScamperError } from "../../src/lpm"
import { makeTestFiber } from "../test-utils"

// Regressions for unintended bugs in `src/lpm/fiber.ts`. Each `describe`
// block documents one bug and asserts the *desired* (post-fix) behavior,
// so failing tests double as a to-do list. Tests that no longer fail are
// kept as guards against the bug returning.
//
// N.B., contract / intended-behavior tests live in test/lpm/.

/**
 * Bug: `Fiber` has no equivalent of `Thread.reportAndUnwind`. When a
 * `ScamperError` is raised during `step()`, frames are left intact and
 * `#currStmtIdx` is not advanced, so every subsequent `step()` re-hits the
 * same failing instruction and throws again. The old thread model reported
 * once, cleared frames, and advanced past the statement.
 */
describe("runtime errors unwind instead of rethrowing every step", () => {
  test("an undefined-variable error is not raised on every subsequent step", async () => {
    const fiber = makeTestFiber([U.mkDisp([U.mkVar("test-bad-var")])])
    const scamperErrors: ScamperError[] = []

    while (!fiber.isDone()) {
      try {
        await fiber.step()
      } catch (e) {
        if (e instanceof ScamperError) {
          scamperErrors.push(e)
          continue
        }
        throw e
      }
    }

    expect(scamperErrors.length).toBeLessThanOrEqual(1)
    expect(fiber.isDone()).toBe(true)
  })
})

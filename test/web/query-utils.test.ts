import { describe, expect, test } from "vitest"
import { nextTick } from "vue"
import { Range } from "../../src/lpm"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import type { QueryEntry } from "../../src/scamper"
import { useReportedValue } from "../../src/web/components/query/query-utils"

describe("query result text", () => {
  test("distinguishes a pending query from an unreachable target", async () => {
    let resolve!: () => void
    const done = new Promise<void>((r) => {
      resolve = r
    })
    const query: QueryEntry = {
      id: "query",
      queriedRange: Range.none,
      err: new SimpleErrorChannel(),
      done,
    }

    const value = useReportedValue(query)
    expect(value.value).toBe("Querying…")

    resolve()
    await done
    await nextTick()
    expect(value.value).toBe("Queried code could not be reached!")
  })
})

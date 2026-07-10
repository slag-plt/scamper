import { describe, expect, test } from "vitest"
import { Frame } from "../../src/lpm/frame"
import { Env } from "../../src/lpm/lang"
import * as U from "../../src/lpm/util"

describe("Frame application-site labels", () => {
  test("stay distinct and stable when a code body is reused", () => {
    const trueBranchCall = U.mkAp(0)
    const falseBranchCall = U.mkAp(0)
    const afterMatchCall = U.mkAp(0)
    const body = [
      U.mkMatch([
        [U.mkPLit(true), [trueBranchCall]],
        [U.mkPLit(false), [falseBranchCall]],
      ]),
      afterMatchCall,
    ]

    new Frame("first invocation", Env.empty, body)
    const firstIndices = [
      trueBranchCall.apIdx,
      falseBranchCall.apIdx,
      afterMatchCall.apIdx,
    ]

    new Frame("second invocation", Env.empty, body)
    const secondIndices = [
      trueBranchCall.apIdx,
      falseBranchCall.apIdx,
      afterMatchCall.apIdx,
    ]

    expect(firstIndices.every((idx) => idx !== undefined)).toBe(true)
    expect(new Set(firstIndices).size).toBe(firstIndices.length)
    expect(secondIndices).toEqual(firstIndices)
  })
})

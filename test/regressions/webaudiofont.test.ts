import { afterEach, beforeEach, expect, test, vi } from "vitest"
import { waf } from "../../src/lib/webaudiofont/webaudiofont"

const mockWafInstance = {
  test: 123,
}
beforeEach(() => {
  vi.stubGlobal("window", {
    wafInstance: mockWafInstance,
  })
})
afterEach(() => {
  vi.unstubAllGlobals()
})

test("defined wafInstance should not be undefined", () => {
  const testWaf = waf()
  expect(testWaf).toBeDefined()
  expect(testWaf).toEqual(mockWafInstance)
})

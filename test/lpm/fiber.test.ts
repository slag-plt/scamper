import { beforeEach, describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import * as U from "../../src/lpm/util"
import {
  LoggingChannel,
  OutputChannel,
  Value,
} from "../../src/lpm"

async function testExecute(fiber: Fiber, out: OutputChannel) {
  // execute fiber until it's done
  while (!fiber.isDone) {
    // skip minor steps
    if (!(await fiber.step())) continue
    // technically, we only send output when we are done with a statement.
    if (fiber.isProcessingBlk) continue
    out.send(fiber.lastResult)
  }
}

describe("basic ops", () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  const litCases: Value[] = [42, "hi", false, null]
  test.for(litCases)("lit %o", async (lit) => {
    const fiber = new Fiber([U.mkDisp([U.mkLit(lit)])])
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
    expect(out.log).toStrictEqual([lit])
  })

  const varCases: [string, Value][] = [
    ["+", (a: number, b: number) => a + b],
    ["a", 42],
    ["var2", null],
    ["woah", "wee"],
  ]
  test.for(varCases)("var exists: %s -> %o", async ([name, value]) => {
    const fiber = new Fiber([U.mkDisp([U.mkVar(name)])])
    fiber.topLevelEnv.set(name, value)
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
    expect(out.log).toStrictEqual([value])
  })

  test("var doesn't exist", async () => {
    const fiber = new Fiber([U.mkDisp([U.mkVar("test-bad-var")])])
    await expect(testExecute(fiber, out)).rejects.toThrow()
  })

  test("ctor", async () => {
    const fiber = new Fiber([
      U.mkDisp([
        U.mkLit("test"),
        U.mkLit(2),
        U.mkCtor("test-ctor", ["a", "b"]),
      ]),
    ])
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
    expect(out.log.at(0)).toStrictEqual(
      U.mkStruct("test-ctor", ["a", "b"], ["test", 2]),
    )
  })

  test("cls", async () => {
    const clsBody = [U.mkVar("+"), U.mkVar("x"), U.mkLit(1), U.mkAp(2)]
    const fiber = new Fiber([
      U.mkDisp([U.mkCls(["x"], clsBody, "add-one"), U.mkLit(1), U.mkAp(1)]),
    ])
    fiber.topLevelEnv.set("+", (a: number, b: number) => a + b)
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
    expect(out.log.at(0)).toStrictEqual(2)
  })
})

import { beforeEach, describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import * as U from "../../src/lpm/util"
import { LoggingChannel, OutputChannel, Prog, Value } from "../../src/lpm"

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

function makeTestFiber(prog: Prog): Fiber {
  const fiber = new Fiber(prog)
  fiber.topLevelEnv.set("+", (a: number, b: number) => a + b)
  return fiber
}

describe("basic ops", () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  async function expectSuccessfulExecution(fiber: Fiber) {
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
  }
  async function expectFailedExecution(fiber: Fiber) {
    await expect(testExecute(fiber, out)).rejects.toThrow()
  }

  const litCases: Value[] = [42, "hi", false, null]
  test.for(litCases)("lit %o", async (lit) => {
    const fiber = makeTestFiber([U.mkDisp([U.mkLit(lit)])])
    await expectSuccessfulExecution(fiber)
    expect(out.log).toStrictEqual([lit])
  })

  const varCases: [string, Value][] = [
    ["-", (a: number, b: number) => a - b],
    ["a", 42],
    ["var2", null],
    ["woah", "wee"],
  ]
  test.for(varCases)("var exists: %s -> %o", async ([name, value]) => {
    const fiber = makeTestFiber([U.mkDisp([U.mkVar(name)])])
    fiber.topLevelEnv.set(name, value)
    await expectSuccessfulExecution(fiber)
    expect(out.log).toStrictEqual([value])
  })

  test("var doesn't exist", async () => {
    const fiber = makeTestFiber([U.mkDisp([U.mkVar("test-bad-var")])])
    await expectFailedExecution(fiber)
  })

  test("ctor", async () => {
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("test"),
        U.mkLit(2),
        U.mkCtor("test-ctor", ["a", "b"]),
      ]),
    ])
    await expectSuccessfulExecution(fiber)
    expect(out.log.at(0)).toStrictEqual(
      U.mkStruct("test-ctor", ["a", "b"], ["test", 2]),
    )
  })

  test("cls", async () => {
    const clsBody = [U.mkVar("+"), U.mkVar("x"), U.mkLit(1), U.mkAp(2)]
    const fiber = makeTestFiber([
      U.mkDisp([U.mkCls(["x"], clsBody, "add-one"), U.mkLit(1), U.mkAp(1)]),
    ])
    await expectSuccessfulExecution(fiber)
    expect(out.log.at(0)).toStrictEqual(2)
  })

  test("ap", async () => {
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar("+"), U.mkLit(3), U.mkLit(4), U.mkAp(2)]),
    ])
    await expectSuccessfulExecution(fiber)
    expect(out.log).toStrictEqual([7])
  })

  test("match w/ first pattern", async () => {
    const ifBranch = [U.mkLit("matched")]
    const elseBranch = [U.mkLit("not matched")]
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit(42),
        U.mkMatch([
          [U.mkPLit(42), ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    await expectSuccessfulExecution(fiber)
    expect(out.log).toEqual(["matched"])
  })

  test("match w/ second pattern", async () => {
    const ifBranch = [U.mkLit("matched")]
    const elseBranch = [U.mkLit("not matched")]
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit(42),
        U.mkMatch([
          [U.mkPLit(99), ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    await expectSuccessfulExecution(fiber)
    expect(out.log).toEqual(["not matched"])
  })
})

import { beforeEach, describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import * as U from "../../src/lpm/util"
import { LoggingChannel, OutputChannel, Prog, Value } from "../../src/lpm"

async function testExecute(fiber: Fiber, out: OutputChannel) {
  // execute fiber until it's done
  while (!fiber.isDone) {
    // skip minor steps
    if (!(await fiber.step())) continue
    if (fiber.isProcessingBlk) continue
    if (fiber.lastStatement.tag !== "disp") continue
    // we only display output when we're done with a statement AND the statement we just finished was a disp.
    out.send(fiber.lastResult)
  }
}

function makeTestFiber(prog: Prog): Fiber {
  const fiber = new Fiber(prog)
  fiber.topLevelEnv.set("+", (a: number, b: number) => a + b)
  fiber.topLevelEnv.set("-", (a: number, b: number) => a - b)
  fiber.topLevelEnv.set("*", (a: number, b: number) => a * b)
  return fiber
}

describe("basic ops", () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  async function expectSuccessfulExec(fiber: Fiber) {
    await expect(testExecute(fiber, out)).resolves.not.toThrow()
  }
  async function expectFailedExec(fiber: Fiber) {
    await expect(testExecute(fiber, out)).rejects.toThrow()
  }

  const litCases: Value[] = [42, "hi", false, null]
  test.for(litCases)("lit %o", async (lit) => {
    const fiber = makeTestFiber([U.mkDisp([U.mkLit(lit)])])
    await expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([lit])
  })

  describe("var", () => {
    const varCases: [string, Value][] = [
      ["/", (a: number, b: number) => a / b],
      ["a", 42],
      ["var2", null],
      ["woah", "wee"],
    ]
    test.for(varCases)("exists: %s -> %o", async ([name, value]) => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar(name)])])
      fiber.topLevelEnv.set(name, value)
      await expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([value])
    })

    test("doesn't exist", async () => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar("test-bad-var")])])
      await expectFailedExec(fiber)
    })
  })

  test("ctor", async () => {
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("test"),
        U.mkLit(2),
        U.mkCtor("test-ctor", ["a", "b"]),
      ]),
    ])
    await expectSuccessfulExec(fiber)
    expect(out.log.at(0)).toStrictEqual(
      U.mkStruct("test-ctor", ["a", "b"], ["test", 2]),
    )
  })

  test("cls", async () => {
    const clsBody = [U.mkVar("+"), U.mkVar("x"), U.mkLit(1), U.mkAp(2)]
    const fiber = makeTestFiber([
      U.mkDisp([U.mkCls(["x"], clsBody, "add-one"), U.mkLit(1), U.mkAp(1)]),
    ])
    await expectSuccessfulExec(fiber)
    expect(out.log.at(0)).toStrictEqual(2)
  })

  test("ap", async () => {
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar("+"), U.mkLit(3), U.mkLit(4), U.mkAp(2)]),
    ])
    await expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([7])
  })

  describe("match", () => {
    test("w/ plit", async () => {
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
      await expectSuccessfulExec(fiber)
      expect(out.log).toEqual(["matched"])
    })

    test("w/ second pattern", async () => {
      const ifBranch = [U.mkLit("wrong match")]
      const elseBranch = [U.mkLit("other one")]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkMatch([
            [U.mkPLit(99), ifBranch],
            [U.mkPLit(42), elseBranch],
          ]),
        ]),
      ])
      await expectSuccessfulExec(fiber)
      expect(out.log).toEqual(["other one"])
    })

    test("failed", async () => {
      const ifBranch = [U.mkLit("wrong match")]
      const elseBranch = [U.mkLit("other one")]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkMatch([
            [U.mkPLit(99), ifBranch],
            [U.mkPLit(67), elseBranch],
          ]),
        ]),
      ])
      await expectFailedExec(fiber)
    })

    test("w/ pvar", async () => {
      const ifBranch = [U.mkVar("+"), U.mkVar("x"), U.mkLit(10), U.mkAp(2)]
      const elseBranch = [U.mkLit(0)]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(5),
          U.mkMatch([
            [U.mkPVar("x"), ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      await expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([15])
    })

    test("w/ pwild", async () => {
      const ifBranch = [U.mkLit("always matches")]
      const elseBranch = [U.mkLit("never reached")]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit("anything"),
          U.mkMatch([
            [U.mkPWild(), ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      await expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(["always matches"])
    })

    test("w/ pctor", async () => {
      const testStruct = [
        U.mkLit(1),
        U.mkLit(2),
        U.mkCtor("test-struct", ["field1", "field2"]),
      ]
      const ifBranch = [
        U.mkVar("+"),
        U.mkVar("a"),
        U.mkVar("b"),
        U.mkAp(2),
      ]
      const elseBranch = [U.mkRaise("no match"), U.mkPops()]
      const pattern = U.mkPCtor("test-struct", [U.mkPVar("a"), U.mkPVar("b")])
      const fiber = makeTestFiber([
        U.mkDisp([
          ...testStruct,
          U.mkMatch([
            [pattern, ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      await expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([3])
    })
  })

  test("factorial", async () => {
    const factorialCls = U.mkCls(
      ["n"],
      [
        U.mkVar("n"),
        U.mkMatch([
          [U.mkPLit(0), [U.mkLit(1)]],
          [
            U.mkPWild(),
            [
              U.mkVar("*"),
              U.mkVar("n"),
              U.mkVar("fact"),
              U.mkVar("-"),
              U.mkVar("n"),
              U.mkLit(1),
              U.mkAp(2),
              U.mkAp(1),
              U.mkAp(2),
            ],
          ],
        ]),
      ],
      "fact",
    )
    const fiber = makeTestFiber([
      U.mkDefine("fact", [factorialCls]),
      U.mkDisp([U.mkVar("fact"), U.mkLit(5), U.mkAp(1)]),
    ])
    await expectSuccessfulExec(fiber)
    // TODO: the test executor outputs define statements
    expect(out.log).toStrictEqual([120])
  })
})

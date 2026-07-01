import { beforeEach, describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import * as U from "../../src/lpm/util"
import {
  LoggingChannel,
  OutputChannel,
  ReportError,
  Value,
} from "../../src/lpm"
import { makeTestFiber } from "../test-utils"
import { anyRange } from "../scheme/util"

function testExecute(fiber: Fiber, out: OutputChannel) {
  // execute fiber until it's done
  while (!fiber.isDone()) {
    const res = fiber.step()
    if (res.tag === "display") {
      out.send(fiber.lastResult)
    }
  }
}

describe("basic ops", () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  function expectSuccessfulExec(fiber: Fiber) {
    expect(() => {
      testExecute(fiber, out)
    }).not.toThrow()
  }
  function expectFailedExec(fiber: Fiber) {
    expect(() => {
      testExecute(fiber, out)
    }).toThrow()
  }

  const litCases: Value[] = [42, "hi", false, null]
  test.for(litCases)("lit %o", (lit) => {
    const fiber = makeTestFiber([U.mkDisp([U.mkLit(lit)])])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([lit])
  })

  describe("var", () => {
    const varCases: [string, Value][] = [
      ["/", (a: number, b: number) => a / b],
      ["a", 42],
      ["var2", null],
      ["woah", "wee"],
    ]
    test.for(varCases)("exists: %s -> %o", ([name, value]) => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar(name)])])
      fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([name, value])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([value])
    })

    test("doesn't exist", () => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar("test-bad-var")])])
      expectFailedExec(fiber)
    })
  })

  test("ctor", () => {
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("test"),
        U.mkLit(2),
        U.mkCtor("test-ctor", ["a", "b"]),
      ]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log.at(0)).toStrictEqual(
      U.mkStruct("test-ctor", ["a", "b"], ["test", 2]),
    )
  })

  test("cls", () => {
    const clsBody = [U.mkVar("+"), U.mkVar("x"), U.mkLit(1), U.mkAp(2)]
    const fiber = makeTestFiber([
      U.mkDisp([U.mkCls(["x"], clsBody, "add-one"), U.mkLit(1), U.mkAp(1)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log.at(0)).toStrictEqual(2)
  })

  test("ap", () => {
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar("+"), U.mkLit(3), U.mkLit(4), U.mkAp(2)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([7])
  })

  describe("match", () => {
    test("w/ plit", () => {
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
      expectSuccessfulExec(fiber)
      expect(out.log).toEqual(["matched"])
    })

    test("w/ second pattern", () => {
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
      expectSuccessfulExec(fiber)
      expect(out.log).toEqual(["other one"])
    })

    test("failed", () => {
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
      expectFailedExec(fiber)
    })

    test("w/ pvar", () => {
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
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([15])
    })

    test("w/ pwild", () => {
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
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(["always matches"])
    })

    test("w/ pctor", () => {
      const testStruct = [
        U.mkLit(1),
        U.mkLit(2),
        U.mkCtor("test-struct", ["field1", "field2"]),
      ]
      const ifBranch = [U.mkVar("+"), U.mkVar("a"), U.mkVar("b"), U.mkAp(2)]
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
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([3])
    })
  })

  test("define", () => {
    const fiber = makeTestFiber([
      U.mkDefine("x", [U.mkLit(1)]),
      U.mkDisp([U.mkVar("+"), U.mkLit(1), U.mkVar("x"), U.mkAp(2)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([2])
  })

  test("factorial", () => {
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
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([120])
  })

  test("report", () => {
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar("+"), U.mkLit(1), U.mkLit(2), U.mkAp(2), U.mkRept()]),
    ])

    const expectedError = new ReportError(3, anyRange)

    const testRunner = () => {
      try {
        testExecute(fiber, out)
        expect.fail("oops... should not have gotten here")
      } catch (e) {
        return e
      }
    }

    expect(testRunner()).toStrictEqual(expectedError)
  })
})

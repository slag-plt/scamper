import { describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import * as U from "../../src/lpm/util"
import { LoggingChannel, OutputChannel, ReportError } from "../../src/lpm"
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
  test("lit", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([U.mkDisp([U.mkLit(42)])])
    testExecute(fiber, out)
    expect(out.log).toEqual([42])
  })

  test("var", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([U.mkDisp([U.mkVar("+")])])
    testExecute(fiber, out)
    expect(out.log).toEqual([fiber.topLevelEnv.get("+")])
  })

  test("ctor", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("test"),
        U.mkLit(2),
        U.mkCtor("test-ctor", ["a", "b"]),
      ]),
    ])
    testExecute(fiber, out)
    const result = out.log[0]
    expect(result).toEqual(U.mkStruct("test-ctor", ["a", "b"], ["test", 2]))
  })

  test("cls", () => {
    const out = new LoggingChannel(false, false)
    const body = [U.mkVar("+"), U.mkVar("x"), U.mkLit(1), U.mkAp(2)]
    const fiber = makeTestFiber([
      U.mkDisp([U.mkCls(["x"], body, "add-one"), U.mkLit(1), U.mkAp(1)]),
    ])
    testExecute(fiber, out)
    const result = out.log[0]
    expect(result).toBe(2)
  })

  test("ap", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar("+"), U.mkLit(3), U.mkLit(4), U.mkAp(2)]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual([7])
  })

  test("match - successful pattern", () => {
    const out = new LoggingChannel(false, false)
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
    testExecute(fiber, out)
    expect(out.log).toEqual(["matched"])
  })

  test("match - failed pattern", () => {
    const out = new LoggingChannel(false, false)
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
    testExecute(fiber, out)
    expect(out.log).toEqual(["not matched"])
  })

  test("match - variable pattern", () => {
    const out = new LoggingChannel(false, false)
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
    testExecute(fiber, out)
    expect(out.log).toEqual([15])
  })

  test("match - wildcard pattern", () => {
    const out = new LoggingChannel(false, false)
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
    testExecute(fiber, out)
    expect(out.log).toEqual(["always matches"])
  })

  test("disp", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([U.mkDisp([U.mkLit("hello world")])])
    testExecute(fiber, out)
    expect(out.log).toEqual(["hello world"])
  })

  test("reporting", () => {
    const out = new LoggingChannel(false, false)
    const fiber = makeTestFiber([
      U.mkDisp([U.mkRptBegin(), U.mkLit("test error"), U.mkRptEnd()]),
    ])
    expect(() => {
      testExecute(fiber, out)
    }).toThrow(new ReportError({ tag: "value", value: "test error" }, anyRange))
  })

  // TODO: need a pop test?
})

describe("pattern matching", () => {
  test("pwild - matches anything", () => {
    const out = new LoggingChannel(false, false)
    const ifBranch = [U.mkLit("matched")]
    const elseBranch = [U.mkLit("not matched")]
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("any value"),
        U.mkMatch([
          [U.mkPWild(), ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual(["matched"])
  })

  test("plit - literal pattern match", () => {
    const out = new LoggingChannel(false, false)
    const ifBranch = [U.mkLit("number matched")]
    const elseBranch = [U.mkLit("number not matched")]
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit(123),
        U.mkMatch([
          [U.mkPLit(123), ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual(["number matched"])
  })

  test("pvar - variable binding", () => {
    const out = new LoggingChannel(false, false)
    const ifBranch = [
      U.mkVar("+"),
      U.mkVar("captured"),
      U.mkLit(" was captured"),
      U.mkAp(2),
    ]
    const elseBranch = [U.mkLit("no match")]
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkLit("hello"),
        U.mkMatch([
          [U.mkPVar("captured"), ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual(["hello was captured"])
  })

  test("pctor - constructor pattern", () => {
    const out = new LoggingChannel(false, false)
    // First create a struct to match against
    const setupStruct = [
      U.mkLit(1),
      U.mkLit(2),
      U.mkCtor("test-struct", ["field1", "field2"]),
    ]
    const ifBranch = [U.mkVar("+"), U.mkVar("a"), U.mkVar("b"), U.mkAp(2)]
    const elseBranch = [U.mkLit("no match"), U.mkRptEnd()]
    const pattern = U.mkPCtor("test-struct", [U.mkPVar("a"), U.mkPVar("b")])

    const fiber = makeTestFiber([
      U.mkDisp([
        ...setupStruct,
        U.mkMatch([
          [pattern, ifBranch],
          [U.mkPWild(), elseBranch],
        ]),
      ]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual([3])
  })

  test("factorial", () => {
    const out = new LoggingChannel(false, false)
    const factorial = U.mkCls(
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
      U.mkDefine("fact", [factorial]),
      U.mkDisp([U.mkVar("fact"), U.mkLit(5), U.mkAp(1)]),
    ])
    testExecute(fiber, out)
    expect(out.log).toEqual([120])
  })
})

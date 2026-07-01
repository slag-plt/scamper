import { expect, test } from "vitest"
import { expToString } from "../../src/scheme/ast.js"
import { raiseFiber } from "../../src/scheme/raise.js"
import * as LPM from "../../src/lpm/"
import { Frame } from "../../src/lpm/frame.js"
import { makeTestFiber } from "../test-utils.js"

function raiseBlk(blk: LPM.Blk): string {
  const fiber = makeTestFiber([])
  fiber.pushFrame(new Frame("f1", LPM.Env.empty, blk))
  return expToString(raiseFiber(fiber))
}

test("basic complete binary raising", () => {
  const result = raiseBlk([
    LPM.mkVar("+"),
    LPM.mkLit(1),
    LPM.mkLit(1),
    LPM.mkAp(2),
  ])
  expect(result).toBe("(+ 1 1)")
})

test("basic mid-evaluation raising", () => {
  const fn = (x: number, y: number) => x + y
  LPM.nameFn("+", fn)
  const fiber = makeTestFiber([])
  const frame = new Frame("f1", LPM.Env.empty, [LPM.mkLit(1), LPM.mkAp(2)])
  frame.values.push(fn, 1)
  fiber.pushFrame(frame)
  const result = expToString(raiseFiber(fiber))
  expect(result).toBe("(+ 1 1)")
})

test("mkVar operation raising", () => {
  const result = raiseBlk([LPM.mkVar("x")])
  expect(result).toBe("x")
})

test("mkLit operation raising with number", () => {
  const result = raiseBlk([LPM.mkLit(42)])
  expect(result).toBe("42")
})

test("mkLit operation raising with string", () => {
  const result = raiseBlk([LPM.mkLit("hello")])
  expect(result).toBe('"hello"')
})

test("mkLit operation raising with boolean", () => {
  const result = raiseBlk([LPM.mkLit(true)])
  expect(result).toBe("#t")
})

test("mkCtor operation raising", () => {
  const result = raiseBlk([
    LPM.mkLit(1),
    LPM.mkLit(2),
    LPM.mkCtor("pair", ["fst", "snd"]),
  ])
  expect(result).toBe("(pair 1 2)")
})

test("mkCls operation raising", () => {
  const result = raiseBlk([LPM.mkCls(["x"], [LPM.mkVar("x")], "identity")])
  expect(result).toBe("identity")
})

test("mkAp operation raising", () => {
  const result = raiseBlk([
    LPM.mkVar("f"),
    LPM.mkLit(10),
    LPM.mkLit(20),
    LPM.mkAp(2),
  ])
  expect(result).toBe("(f 10 20)")
})

test("mkMatch operation raising", () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(1), [LPM.mkLit("one")]],
    [LPM.mkPWild(), [LPM.mkLit("other")]],
  ]
  const result = raiseBlk([LPM.mkLit(1), LPM.mkMatch(branches)])
  expect(result).toBe('(match 1 [1 "one"] [_ "other"])')
})

test("mkRaise operation raising", () => {
  const result = raiseBlk([LPM.mkRaise("Test error message")])
  expect(result).toBe('(raise "Test error message")')
})

test("nested operations raising", () => {
  const result = raiseBlk([
    LPM.mkVar("+"),
    LPM.mkVar("*"),
    LPM.mkLit(2),
    LPM.mkLit(3),
    LPM.mkAp(2),
    LPM.mkLit(4),
    LPM.mkAp(2),
  ])
  expect(result).toBe("(+ (* 2 3) 4)")
})

test("complex constructor with multiple fields", () => {
  const result = raiseBlk([
    LPM.mkLit("John"),
    LPM.mkLit(30),
    LPM.mkLit("Engineer"),
    LPM.mkCtor("person", ["name", "age", "job"]),
  ])
  expect(result).toBe('(person "John" 30 "Engineer")')
})

test("lambda with multiple parameters", () => {
  const result = raiseBlk([LPM.mkCls(["x", "y"], [LPM.mkVar("x")], "add")])
  expect(result).toBe("add")
})

test("pattern matching with multiple branches", () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(0), [LPM.mkLit("zero")]],
    [LPM.mkPLit(1), [LPM.mkLit("one")]],
    [LPM.mkPVar("n"), [LPM.mkVar("n")]],
  ]
  const result = raiseBlk([LPM.mkLit(5), LPM.mkMatch(branches)])
  expect(result).toBe('(match 5 [0 "zero"] [1 "one"] [n n])')
})

test("pattern matching with constructor pattern", () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPCtor("pair", [LPM.mkPVar("x"), LPM.mkPVar("y")]), [LPM.mkVar("x")]],
  ]
  const result = raiseBlk([LPM.mkLit("test"), LPM.mkMatch(branches)])
  expect(result).toBe('(match "test" [(pair x y) x])')
})

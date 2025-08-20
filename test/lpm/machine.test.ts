import { expect, test, describe } from "@jest/globals"
import { Machine, Output } from "../../src/lpm/machine.js"
import { Code, Id, mkBranch, mkClosure, mkList, mkSym, mkPVar, Program, Value } from "../../src/lpm/runtime.js"
import Ops from "../../src/lpm/ops.js";
import Thread from "../../src/lpm/thread.js";

class LoggingOutput implements Output {
  log: Value[] = []
  send (value: Value) {
    this.log.push(value)
  }
}

function makeProgram(code: [string, Iterable<number>, number][], identifiers: string[] = [], objects: Value[] = []): Program {
  const codeMap = new Map<Id, Code>();
  for (const [id, bytes, numLocals] of code) {
    codeMap.set(id, { ops: new Uint8Array(bytes), numLocals });
  }
  return { code: codeMap, identifiers, objects };
}

function makeLoggingMachine(
  code: [string, Iterable<number>, number][],
  log: Output,
  identifiers: string[] = [],
  objects: Value[] = [],
  globals: [string, Value][] = [],
): Machine {
  const program = makeProgram(code, identifiers, objects);
  return new Machine(
    program,
    // TODO: populate this with the appropriate builtin libs (from lib/builtin.js)
    new Map(),
    log,
    { maxArgs: 4, maxCallStackDepth: 10 },
    new Map<string, Value>(globals),
  );
}

function machineOutputOf(
  code: [string, Iterable<number>, number][],
  identifiers: string[] = [],
  objects: Value[] = [],
  globals: [string, Value][] = []
): Value {
  const output = new LoggingOutput();
  const machine = makeLoggingMachine(code, output, identifiers, objects, globals);
  const thread = new Thread('main', [])
  machine.execute(thread);
  return output.log[0]
}

describe("Literals", () => {
  test("int literal", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(42)
  })

  test("bool literal true", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.bool, 1,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("bool literal false", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.bool, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("str literal", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.str, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], ["hello"])).toBe("hello")
  })

  test("obj literal", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.obj, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], [], ["string as object"])).toBe("string as object")
  })
})

describe("Basic arithmetic", () => {
  test("(+ 1 1)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 1,
        Ops.int, 1,
        Ops.add, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(2)
  })

  test("(- 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.sub, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(2)
  })

  test("(* 4 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 4,
        Ops.int, 3,
        Ops.mul, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(12)
  })

  test("(/ 15 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 15,
        Ops.int, 3,
        Ops.div, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(5)
  })

  test("(+ (* 2 3) 4)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 2,
        Ops.int, 3,
        Ops.mul, 0,
        Ops.int, 4,
        Ops.add, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(10)
  })

  test("(- (* 7 2) (/ 8 4))", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 7,
        Ops.int, 2,
        Ops.mul, 0,
        Ops.int, 8,
        Ops.int, 4,
        Ops.div, 0,
        Ops.sub, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(12)
  })
})

describe("Comparisons", () => {
  test("(< 3 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 3,
        Ops.int, 5,
        Ops.lt, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(< 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.lt, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("(<= 3 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 3,
        Ops.int, 5,
        Ops.lte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(<= 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.lte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("(<= 5 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 5,
        Ops.lte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(> 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.gt, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(> 3 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 3,
        Ops.int, 5,
        Ops.gt, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("(>= 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.gte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(>= 3 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 3,
        Ops.int, 5,
        Ops.gte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("(>= 5 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 5,
        Ops.gte, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(= 5 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 5,
        Ops.eq, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(= 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.eq, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })

  test("(!= 5 3)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 3,
        Ops.neq, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(true)
  })

  test("(!= 5 5)", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 5,
        Ops.int, 5,
        Ops.neq, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]])).toBe(false)
  })
})

describe("Variables", () => {
  test("globals", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 41,
        Ops.gstore, 0,
        Ops.int, 1,
        Ops.gload, 0,
        Ops.add, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], ["x"])).toBe(42)
  })

  test("locals", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 41,
        Ops.lstore, 0,
        Ops.int, 1,
        Ops.lload, 0,
        Ops.add, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 1]])).toBe(42)
  })
})

describe("Conditionals", () => {
  test("Bjmp true", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 8,
        Ops.int, 8,
        Ops.eq, 0,
        Ops.ifnb, 16,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 76,
        Ops.disp, 0,
        Ops.int, 0, 
        Ops.ret, 0,
      ], 0]]
    )).toBe(42)
  })

  test("Bjmp false", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 8,
        Ops.int, 2,
        Ops.eq, 0,
        Ops.ifnb, 16,
        Ops.int, 76,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0, 
        Ops.ret, 0,
      ], 0]]
    )).toBe(42)
  })
})

describe("Pattern matching", () => {
  test("Match literal int success", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 255,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], [], [mkBranch(255, 12)])).toBe(42)
  })

  test("Match literal int failure", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.int, 100,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], [], [mkBranch(255, 12)])).toBe(18)
  })

  test("Match literal bool success", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.bool, 1,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], [], [mkBranch(true, 12)])).toBe(42)
  })

  test("Match literal bool failure", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.bool, 0,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], [], [mkBranch(true, 12)])).toBe(18)
  })

  test("Match literal string success", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.str, 0,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], ["hello"], [mkBranch("hello", 12)])).toBe(42)
  })

  test("Match literal string failure", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.str, 0,
        Ops.ifnm, 0,
        Ops.int, 42,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 18,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 0]], ["world"], [mkBranch("hello", 12)])).toBe(18)
  })

  test("Match list pattern", () => {
    expect(machineOutputOf(
      [["main", [
        Ops.obj, 1,
        Ops.ifnm, 0,
        Ops.lload, 0,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
        Ops.int, 99,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 2]],
      [], 
      [mkBranch(mkList(mkSym("cons"), mkPVar(0), mkPVar(1)), 16), mkList(42, 7)]
    )).toBe(42)
  })
})

describe("Function calls", () => {
  test("Basic call", () => {
    expect(machineOutputOf(
      [["f", [
        Ops.lload, 0,
        Ops.int, 41,
        Ops.add, 0,
        Ops.ret, 0
      ], 1],
      ["main", [
        Ops.gload, 0,
        Ops.int, 1,
        Ops.ap, 1,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], ["f"], [], [["f", mkClosure(1, ["x"], "f", [])]])).toBe(42)
  })

  test("Factorial (non-tail)", () => {
    expect(machineOutputOf(
      [["factorial", [
        Ops.lload, 0,
        Ops.int, 0,
        Ops.neq, 0,
        Ops.ifnb, 24,
        Ops.lload, 0,
        Ops.gload, 0,
        Ops.lload, 0,
        Ops.int, 1,
        Ops.sub, 0,
        Ops.ap, 1,
        Ops.mul, 0,
        Ops.ret, 0,
        Ops.int, 1,
        Ops.ret, 0,
      ], 1],
      ["main", [
        Ops.gload, 0,
        Ops.int, 5,
        Ops.ap, 1,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], ["factorial"], [],
      [["factorial", mkClosure(1, ["n"], "factorial", [])]])).toBe(120)
  })

  test("Factorial (tail)", () => {
    expect(machineOutputOf(
      [["factorial", [
        Ops.lload, 0,
        Ops.int, 0,
        Ops.neq, 0,
        Ops.ifnb, 26,
        Ops.gload, 0,
        Ops.lload, 0,
        Ops.int, 1,
        Ops.sub, 0,
        Ops.lload, 0,
        Ops.lload, 1,
        Ops.mul, 0,
        Ops.ap, 2,
        Ops.ret, 0,
        Ops.lload, 1,
        Ops.ret, 0,
      ], 2],
      ["main", [
        Ops.gload, 0,
        Ops.int, 5,
        Ops.int, 1,
        Ops.ap, 2,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]], ["factorial"], [],
      [["factorial", mkClosure(2, ["n", "ret"], "factorial", [])]])).toBe(120)
  })

  test("Recursive list length", () => {
    expect(machineOutputOf(
      [["length", [
        Ops.lload, 0,
        Ops.ifnm, 0,
        Ops.gload, 0,
        Ops.lload, 2,
        Ops.ap, 1,
        Ops.int, 1,
        Ops.add, 0,
        Ops.ret, 0,
        Ops.int, 0,
        Ops.ret, 0,
      ], 3],
      ["main", [
        Ops.gload, 0,
        Ops.obj, 1,
        Ops.ap, 1,
        Ops.disp, 0,
        Ops.int, 0,
        Ops.ret, 0
      ], 0]],
      ["length"],
      [mkBranch(mkList(mkSym("cons"), mkPVar(1), mkPVar(2)), 16), mkList(1, 2, 3, 4)],
      [["length", mkClosure(1, ["lst"], "length", [])]]
    )).toBe(4)
  })
})
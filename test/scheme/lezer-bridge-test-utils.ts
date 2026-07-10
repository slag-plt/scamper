import { expect } from "vitest"
import * as L from "../../src/lpm"
import { parseProgramFromSource } from "../../src/scheme/lezer-bridge"
import { parseProgram } from "../../src/scheme/parser"
import { read } from "../../src/scheme/reader"

export function bothParse(src: string) {
  const oldErrors: L.ScamperError[] = []
  const oldProg = parseProgram(oldErrors, read(src))
  const newErrors: L.ScamperError[] = []
  const newProg = parseProgramFromSource(newErrors, src)
  return { oldProg, oldErrors, newProg, newErrors }
}

// N.B., L.equals (and so expEquals/stmtEquals) falls back to reference
// equality for L.Range/L.Loc, since those are plain classes rather than
// Scamper-tagged structs -- so it can never confirm two independently
// constructed ranges are equal. That's fine for stmtEquals/expEquals's own
// purposes (they don't compare top-level AST node ranges at all), but it's
// unusable here, since we specifically want to confirm the bridge produces
// byte-for-byte identical ranges everywhere, including inside quoted/vector
// literal data. Hence this test's own deep, range-aware comparator.
export function deepEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true
  if (a instanceof L.Range && b instanceof L.Range) {
    return deepEqual(a.begin, b.begin) && deepEqual(a.end, b.end)
  }
  if (a instanceof L.Loc && b instanceof L.Loc) {
    return a.line === b.line && a.col === b.col && a.idx === b.idx
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((x, i) => deepEqual(x, b[i]))
  }
  if (
    typeof a === "object" &&
    a !== null &&
    typeof b === "object" &&
    b !== null
  ) {
    const keys = new Set([...Object.keys(a), ...Object.keys(b)])
    for (const k of keys) {
      if (!deepEqual((a as never)[k], (b as never)[k])) return false
    }
    return true
  }
  return false
}

export function expectEquivalent(src: string) {
  const { oldProg, oldErrors, newProg, newErrors } = bothParse(src)
  expect(oldErrors, `old parser errors for ${JSON.stringify(src)}`).toEqual([])
  expect(newErrors, `new parser errors for ${JSON.stringify(src)}`).toEqual([])
  expect(newProg.length, `statement count for ${JSON.stringify(src)}`).toBe(
    oldProg.length,
  )
  for (let i = 0; i < oldProg.length; i++) {
    expect(
      deepEqual(oldProg[i], newProg[i]),
      `statement ${i} mismatch for ${JSON.stringify(src)}:\n  old: ${JSON.stringify(oldProg[i])}\n  new: ${JSON.stringify(newProg[i])}`,
    ).toBe(true)
  }
}

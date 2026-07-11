import { expect } from "vitest"
import * as L from "../../src/lpm"
import { parseProgramFromSource } from "../../src/scheme/lezer-bridge"
import { Prog } from "../../src/scheme/ast"

export function parse(src: string): { prog: Prog; errors: L.ScamperError[] } {
  const errors: L.ScamperError[] = []
  const prog = parseProgramFromSource(errors, src)
  return { prog, errors }
}

// A lighter-weight check for the many samples that exist to exercise every
// form/token combination the grammar supports: just confirm the source
// parses without error. Exact AST-shape assertions for the trickiest cases
// (quote's raw payload, vector literals, docstrings, wildcard patterns)
// live directly in the tests that care instead.
export function expectParses(src: string) {
  const { prog, errors } = parse(src)
  expect(errors, `errors for ${JSON.stringify(src)}`).toEqual([])
  expect(
    prog.length,
    `statement count for ${JSON.stringify(src)}`,
  ).toBeGreaterThan(0)
}

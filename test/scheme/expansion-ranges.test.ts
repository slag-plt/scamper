import { describe, expect, test } from 'vitest'
import * as A from '../../src/scheme/ast'
import { expandStmt } from '../../src/scheme/expansion'
import { Loc, Range } from '../../src/lpm/range'

// Expansion rewrites statements, and each rewritten statement has to keep the
// range it came from: it is what points errors, traces, and the output pane's
// source captions back at the code the person wrote.
describe('expansion keeps source ranges', () => {
  const range = new Range(new Loc(1, 1, 4), new Loc(1, 10, 13))

  test('a bare expression statement keeps its range', () => {
    // Regression: this case alone dropped the range, so every top-level
    // expression -- the bulk of student code -- came out located nowhere.
    const [expanded] = expandStmt(A.mkStmtExp(A.mkLit(1, range), range))
    expect(expanded.range).toEqual(range)
  })

  test('so do the statements around it', () => {
    const cases: A.Stmt[] = [
      A.mkDisp(A.mkLit(1, range), range),
      A.mkDefine(A.mkId('x', range), A.mkLit(1, range), range),
      A.mkExport([A.mkId('x', range)], range),
    ]
    for (const stmt of cases) {
      expect(expandStmt(stmt)[0].range).toEqual(range)
    }
  })
})

import { describe, expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { drawing_rectangle, drawing_rotate } from '../../src/js/image/drawing.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// https://github.com/slag-plt/scamper/issues/454
//
// `rotate`'s docstring told students it was "currently buggy and shifts
// off-center" long after #102/#279 fixed the geometry, so the note is removed
// and the invariant it doubted asserted instead. rotate-bounding-box.test.ts
// covers those fixes at 0 and 90 degrees; this adds the centring invariant at
// arbitrary angles, and the docstring itself.

const DEG = Math.PI / 180

describe('rotate is centred, and no longer says otherwise (#454)', () => {
  /** The centre of `d`'s box after `d` is placed at the origin of a rotate. */
  function rotatedCentre (w: number, h: number, degrees: number) {
    const { dx, dy } = drawing_rotate(degrees, drawing_rectangle(w, h, 'solid', 'red'))
    const a = degrees * DEG
    return {
      x: (w / 2) * Math.cos(a) - (h / 2) * Math.sin(a) + dx,
      y: (w / 2) * Math.sin(a) + (h / 2) * Math.cos(a) + dy,
    }
  }

  // The axis-aligned bounding box of the rotated shape:
  //   w' = |w cos t| + |h sin t|,  h' = |w sin t| + |h cos t|
  test.each([
    [100, 100, 45],
    [100, 40, 30],
    [100, 40, -45],
    [37, 91, 137],
  ])('a %ix%i box rotated %i degrees gets the axis-aligned box', (w, h, deg) => {
    const a = deg * DEG
    const r = drawing_rotate(deg, drawing_rectangle(w, h, 'solid', 'red'))
    expect(r.width).toBeCloseTo(Math.abs(w * Math.cos(a)) + Math.abs(h * Math.sin(a)), 6)
    expect(r.height).toBeCloseTo(Math.abs(w * Math.sin(a)) + Math.abs(h * Math.cos(a)), 6)
  })

  // The centring invariant: the centre of the original box, carried through
  // the transform the renderer applies (rotate about the origin, then shift by
  // dx/dy), must land on the centre of the new box.
  test.each([0, 15, 30, 45, 90, 137, 180, -45, -137])(
    'the old centre lands on the new centre at %i degrees',
    (deg) => {
      const r = drawing_rotate(deg, drawing_rectangle(100, 40, 'solid', 'red'))
      const c = rotatedCentre(100, 40, deg)
      expect(c.x).toBeCloseTo(r.width / 2, 6)
      expect(c.y).toBeCloseTo(r.height / 2, 6)
    },
  )

  // A 100x100 square turned 45 degrees is a diamond in a ~141.42 box, with a
  // vertex touching the middle of each edge -- the concrete case in the issue.
  test('a square rotated 45 degrees is a centred diamond', () => {
    const r = drawing_rotate(45, drawing_rectangle(100, 100, 'solid', 'red'))
    expect(r.width).toBeCloseTo(141.4213562, 6)
    expect(r.height).toBeCloseTo(141.4213562, 6)
    const a = 45 * DEG
    const corners: [number, number][] = [[0, 0], [100, 0], [100, 100], [0, 100]]
    const placed = corners.map(([x, y]) => [
      x * Math.cos(a) - y * Math.sin(a) + r.dx,
      x * Math.sin(a) + y * Math.cos(a) + r.dy,
    ])
    // every corner sits inside the box, and each edge of the box is touched
    for (const [x, y] of placed) {
      expect(x).toBeGreaterThanOrEqual(-1e-9)
      expect(y).toBeGreaterThanOrEqual(-1e-9)
      expect(x).toBeLessThanOrEqual(r.width + 1e-9)
      expect(y).toBeLessThanOrEqual(r.height + 1e-9)
    }
    expect(Math.min(...placed.map(([x]) => x))).toBeCloseTo(0, 6)
    expect(Math.max(...placed.map(([x]) => x))).toBeCloseTo(r.width, 6)
    expect(Math.min(...placed.map(([, y]) => y))).toBeCloseTo(0, 6)
    expect(Math.max(...placed.map(([, y]) => y))).toBeCloseTo(r.height, 6)
  })

  // The documentation a student reads must not still warn about the fixed bug.
  test('the docstring no longer warns that rotate is buggy', () => {
    const src = readFileSync(resolve(__dirname, '../../src/lib/image.scm'), 'utf-8')
    const diagnostics: ScamperDiagnostic[] = []
    const prog = parseProgramFromSource(diagnostics, src)
    const def = prog.find(
      (s) => (s.tag === 'define' || s.tag === 'defexport') && s.name.name === 'rotate',
    )
    expect(def).toBeDefined()
    if (def === undefined || (def.tag !== 'define' && def.tag !== 'defexport')) return
    const { doc } = parseFunctionDocFromComments(def.docComments ?? [])
    expect(doc).toBeDefined()
    expect(doc?.description).toContain('bounding box')
    expect(doc?.description).not.toMatch(/buggy|off-center/i)
  })
})

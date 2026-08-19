import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import { Loc } from '../../src/lpm/range'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
import TextRenderer from '../../src/lpm/renderers/text'

beforeAll(async () => {
  await initialize()
})

/** Traces the statement containing `idx`, returning its rendered steps. */
async function trace(src: string, idx: number, maxSteps = 1000) {
  const result = await Scamper.getInstance().traceStatement({
    src,
    cursorLoc: new Loc(1, 1, idx),
    err: new SimpleErrorChannel(),
    maxSteps,
  })
  return result === null
    ? null
    : { ...result, rendered: result.steps.map((s) => TextRenderer.render(s)) }
}

/** As {@link trace}, for the cases that are about what a trace contains. */
async function traceOf(src: string, idx: number, maxSteps = 1000) {
  const result = await trace(src, idx, maxSteps)
  if (result === null) throw new Error(`no statement at index ${String(idx)}`)
  return result
}

// Stepping one statement means running the whole program -- the statement
// leans on what came before it -- and keeping only that statement's own
// reductions, gathered in full so they can be paged through afterwards.
describe('tracing a single statement', () => {
  test('collects the reductions of the statement under the cursor', async () => {
    const src = '(define sq (lambda (x) (* x x)))\n(sq 5)\n'
    const result = await traceOf(src, src.indexOf('(sq 5)') + 2)
    expect(result.source).toBe('(sq 5)')
    // It reduces, rather than jumping straight to the answer.
    expect(result.steps.length).toBeGreaterThan(1)
    expect(result.rendered.at(-1)).toContain('25')
  })

  test('keeps only the traced statement, not its neighbours', async () => {
    const src = '(display 1)\n(display 2)\n(display 3)\n'
    const result = await traceOf(src, src.indexOf('(display 2)') + 2)
    expect(result.source).toBe('(display 2)')
    expect(result.rendered.join(' ')).toContain('2')
    expect(result.rendered.join(' ')).not.toContain('1')
    expect(result.rendered.join(' ')).not.toContain('3')
  })

  test('the statements before it still run, so it can use them', async () => {
    // If earlier statements were skipped, `x` would be unbound here.
    const src = '(define x 7)\n(+ x 1)\n'
    const result = await traceOf(src, src.indexOf('(+ x 1)') + 2)
    expect(result.rendered.at(-1)).toContain('8')
  })

  test('an error becomes the last step rather than being lost', async () => {
    const src = '(display 1)\n(car 5)\n'
    const result = await traceOf(src, src.indexOf('(car 5)') + 2)
    expect(result.rendered.at(-1)).toMatch(/error/i)
  })

  test('a cursor outside every statement traces nothing', async () => {
    const src = '(display 1)\n\n\n'
    expect(await trace(src, src.length - 1)).toBeNull()
  })

  test('a program that does not compile traces nothing', async () => {
    expect(await trace('(display', 2)).toBeNull()
  })

  test('a runaway statement is capped rather than hanging', async () => {
    // Without the cap this never returns.
    const src = '(define loop (lambda (n) (loop (+ n 1))))\n(loop 0)\n'
    const result = await traceOf(src, src.indexOf('(loop 0)') + 2, 50)
    expect(result.truncated).toBe(true)
    expect(result.steps.length).toBe(50)
  }, 20000)
})

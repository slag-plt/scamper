import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize, type Env } from '../../src/scamper'
import { ScamperError } from '../../src/lpm/error'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
import { DiscardOutput } from '../../src/lpm/output/discard'
import TextRenderer from '../../src/lpm/renderers/text'

beforeAll(async () => {
  await initialize()
})

/** A session whose output goes nowhere: these tests read traces, not output. */
function open() {
  const err = new SimpleErrorChannel()
  return Scamper.getInstance().startRepl({ out: new DiscardOutput(), err })
}

/** Traces `src` in `env`, returning its rendered steps alongside the raw ones. */
async function trace(src: string, env: Env, maxSteps = 1000) {
  const result = await Scamper.getInstance().traceReplEntry({
    src,
    env,
    err: new SimpleErrorChannel(),
    maxSteps,
  })
  return result === null
    ? null
    : { ...result, rendered: result.steps.map((s) => TextRenderer.render(s)) }
}

/** As {@link trace}, for the cases that are about what a trace contains. */
async function traceOf(src: string, env: Env, maxSteps = 1000) {
  const result = await trace(src, env, maxSteps)
  if (result === null) throw new Error(`nothing to step in ${src}`)
  return result
}

// Stepping a REPL entry replays that one statement in the top level it
// originally ran in, rather than re-running a program to rebuild it (#424).
describe('tracing a REPL entry', () => {
  test('collects the reductions of the entry', async () => {
    const session = open()
    await session.evaluate('(define sq (lambda (x) (* x x)))')
    const result = await traceOf('(sq 5)', session.env)
    expect(result.source).toBe('(sq 5)')
    // It reduces, rather than jumping straight to the answer.
    expect(result.steps.length).toBeGreaterThan(1)
    expect(result.rendered.at(-1)).toContain('25')
    session.end()
  })

  test('sees what the session defined before it', async () => {
    // The environment is the whole of the context: nothing is re-run to
    // rebuild it, so if the capture were empty `x` would be unbound here.
    const session = open()
    await session.evaluate('(define x 7)')
    const result = await traceOf('(+ x 1)', session.env)
    expect(result.rendered.at(-1)).toContain('8')
    session.end()
  })

  test('sees an import an earlier entry made', async () => {
    const session = open()
    await session.evaluate('(import image)')
    const result = await traceOf('(procedure? rectangle)', session.env)
    expect(result.rendered.at(-1)).toContain('#t')
    session.end()
  })

  test('a captured top level is not disturbed by later entries', async () => {
    // The point of capturing one per entry: an entry stepped after the name it
    // used has been redefined must still show the value it actually saw. A top
    // level is persistent, so the capture below survives the redefinition.
    const session = open()
    await session.evaluate('(define x 1)')
    const asItWas = session.env
    await session.evaluate('(define x 10)')

    expect((await traceOf('(+ x 1)', asItWas)).rendered.at(-1)).toContain('2')
    // ...while the session itself has moved on.
    expect((await traceOf('(+ x 1)', session.env)).rendered.at(-1)).toContain(
      '11',
    )
    session.end()
  })

  test('an error becomes the last step rather than being lost', async () => {
    // The entry someone most wants to step is the one that went wrong.
    const session = open()
    const result = await traceOf('(car 5)', session.env)
    expect(result.rendered.at(-1)).toMatch(/error/i)
    session.end()
  })

  test('an entry that does not compile traces nothing', async () => {
    const session = open()
    expect(await trace('(display', session.env)).toBeNull()
    session.end()
  })

  test('an entry with no statement in it traces nothing', async () => {
    // A comment parses fine and yields no statement, so `null` has to come
    // from the empty program rather than from a diagnostic.
    const session = open()
    expect(await trace('; just a comment', session.env)).toBeNull()
    session.end()
  })

  test('reports a diagnostic rather than swallowing it', async () => {
    const session = open()
    const reported: ScamperError[] = []
    const result = await Scamper.getInstance().traceReplEntry({
      src: '(display',
      env: session.env,
      err: {
        report: (e: ScamperError) => {
          reported.push(e)
        },
      },
    })
    expect(result).toBeNull()
    expect(reported.length).toBeGreaterThan(0)
    session.end()
  })

  test('a runaway entry is capped rather than hanging', async () => {
    // Without the cap this never returns. The timeout is real -- a regression
    // here hangs rather than fails.
    const session = open()
    await session.evaluate('(define loop (lambda (n) (loop (+ n 1))))')
    const result = await traceOf('(loop 0)', session.env, 50)
    expect(result.truncated).toBe(true)
    expect(result.steps.length).toBe(50)
    session.end()
  }, 20000)

  test('an entry can still be stepped after its session has ended', async () => {
    // Stepping is asynchronous, so a session can be closed or restarted while
    // one is on its way. The captured top level is plain data and the trace is
    // a side run, so neither depends on the session still being there.
    const session = open()
    await session.evaluate('(define x 1)')
    const asItWas = session.env
    session.end()
    expect((await traceOf('(+ x 1)', asItWas)).rendered.at(-1)).toContain('2')
  })

  test('a replayed entry applies its effects again', async () => {
    // The accepted cost of replaying rather than recording: the statement runs
    // a second time, and a mutation lands on state the session still shares.
    // Pinned so that changing it is a decision rather than an accident.
    const session = open()
    await session.evaluate('(define v (vector 0))')
    await session.evaluate('(vector-set! v 0 (+ 1 (vector-ref v 0)))')
    const afterOnce = session.env
    await traceOf('(vector-set! v 0 (+ 1 (vector-ref v 0)))', afterOnce)
    const seen = await traceOf('(vector-ref v 0)', session.env)
    // Once at the prompt, once more in the replay.
    expect(seen.rendered.at(-1)).toContain('2')
    session.end()
  })

  test('stepping leaves the session usable', async () => {
    // A trace is a side run: it must not adopt the session's run or leave its
    // fiber pointing somewhere else.
    const session = open()
    await session.evaluate('(define x 1)')
    await traceOf('(+ x 1)', session.env)
    await session.evaluate('(define y 2)')
    expect((await traceOf('(+ x y)', session.env)).rendered.at(-1)).toContain(
      '3',
    )
    session.end()
  })
})

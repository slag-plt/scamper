import { afterEach, beforeEach, expect, test, vi } from 'vitest'
import { runProgram } from '../harness.js'
import * as Scheme from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { LoggingChannel } from '../../src/lpm/output/index.js'
import { Scheduler } from '../../src/lpm/scheduler.js'

// runProgram (test/harness.ts) steps a fiber directly and has no tracing
// toggle, so tracing needs the real Scheduler wired up with isTracing --
// that's the only thing that ever emits trace-wrapped output (see the
// isTracing branch in src/lpm/scheduler.ts).
async function runProgramTraced(src: string, isTracing = true): Promise<string[]> {
  src = src.trim()
  const out = new LoggingChannel()
  const env = Scheme.mkInitialEnv()
  const prog = await Scheme.compile(out, src)
  if (out.log.length !== 0) {
    return out.log as string[]
  }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  const fiber = new Fiber(prog, env)
  const sched = new Scheduler()
  await new Promise<void>((resolve) => {
    sched.schedule({
      id: crypto.randomUUID(),
      fiber,
      out,
      err: out,
      isTracing,
      onComplete: resolve,
    })
  })
  return out.log as string[]
}

beforeEach(() => {
  vi.stubGlobal('window', {
    AudioContext: vi.fn(),
  })
})
afterEach(() => {
  vi.unstubAllGlobals()
})

test('traces each define with an arrow-prefixed value, never a bare one', async () => {
  expect(
    await runProgramTraced(`
      (define x 5)
      (define y 10)
    `),
  ).toEqual(['--> null', '--> 5', '--> 5', '--> 10'])
})

test('traces a builtin import as a single step', async () => {
  expect(await runProgramTraced('(import music)')).toEqual(['--> null'])
})

test('a traced top-level expression ends with its correct value, then displays it raw', async () => {
  const result = await runProgramTraced('(+ 1 2)')
  // fiber.lastResult only updates once the whole statement finishes, so every
  // trace line emitted while still evaluating the expression reports the
  // fiber's previous (here: still-initial) result rather than a partial one.
  // Only the last trace line -- emitted once evaluation completes -- is
  // accurate; this is the "coarse" granularity the scheduler offers.
  expect(result.slice(0, -2).every((line) => line === '--> null')).toBe(true)
  expect(result.slice(-2)).toEqual(['--> 3', '3'])
})

test('tracing only adds arrow-prefixed lines on top of the untraced output', async () => {
  const src = `
    (define x 5)
    (display "hi")
    (+ x 1)
  `
  const traced = await runProgramTraced(src)
  const untraced = await runProgram(src)
  expect(traced.filter((line) => !line.startsWith('--> '))).toEqual(untraced)
})

test('tracing disabled never emits arrow-prefixed lines', async () => {
  const result = await runProgramTraced('(define x 5)\n(+ x 1)', false)
  expect(result.some((line) => line.startsWith('--> '))).toBe(false)
  expect(result).toEqual(['6'])
})

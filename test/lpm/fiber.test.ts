import { describe, expect, test, vi } from 'vitest'
import { importFileStep } from '../../src/lpm/fiber'
import { Frame } from '../../src/lpm/frame'
import { ICE, ScamperError, SubthreadErrors } from '../../src/lpm'
import * as U from '../../src/lpm/util'
import { makeTestFiber } from '../util'

describe('Fiber', () => {
  test('step and lastStatement guard against running past the program', () => {
    // prog.at(-1) wraps around to the last element on a non-empty array, so
    // lastStatement's guard only trips on a program with no statements at all.
    expect(() => makeTestFiber([]).lastStatement).toThrow(ICE)

    const fiber = makeTestFiber([U.mkStmtExp([U.mkLit(1)])])
    while (!fiber.isDone()) {
      fiber.step()
    }
    expect(() => fiber.step()).toThrow(ICE)
  })

  test('pushFrame throws once the max call stack depth is exceeded', () => {
    const fiber = makeTestFiber([])
    const frame = new Frame('f', fiber.topLevelEnv, [])
    for (let i = 0; i < 10_000; i++) {
      fiber.pushFrame(frame)
    }
    expect(() => {
      fiber.pushFrame(frame)
    }).toThrow(/Max call stack depth/)
  })

  test('stepFrame guards against missing or empty frames', () => {
    const fiber = makeTestFiber([])
    expect(() => fiber.stepFrame()).toThrow(ScamperError)

    fiber.pushFrame(new Frame('f', fiber.topLevelEnv, []))
    expect(() => fiber.stepFrame()).toThrow(ICE)
  })

  test('stepFrame throws ICE for deprecated raise and pops ops', () => {
    for (const op of [U.mkRaise('boom'), U.mkPops()]) {
      const fiber = makeTestFiber([])
      fiber.pushFrame(new Frame('f', fiber.topLevelEnv, [op]))
      expect(() => fiber.stepFrame()).toThrow(/deprecated/)
    }
  })

  test('completeCurrentFrame guards against invalid completion states', () => {
    const fiber = makeTestFiber([])
    fiber.pushFrame(
      new Frame('f', fiber.topLevelEnv, [U.mkLit(1), U.mkLit(2)]),
    )
    fiber.stepFrame()
    expect(() => fiber.stepFrame()).toThrow(/exactly one value/)

    // stepFrame() always guards against an empty stack before it can reach
    // completeCurrentFrame, so its own "no frame" guard is only reachable by
    // calling the private method directly.
    const noFrame = makeTestFiber([]) as unknown as {
      completeCurrentFrame(): void
    }
    expect(() => {
      noFrame.completeCurrentFrame()
    }).toThrow(/none remain/)
  })

  test('loadModule rejects unknown builtins and defers file imports', () => {
    const fiber = makeTestFiber([])
    expect(() => fiber.loadModule('no-such-lib', 'builtin')).toThrow(
      /No such built-in library/,
    )
    expect(fiber.loadModule('my-file', 'file')).toEqual(
      importFileStep('my-file'),
    )
  })
})

describe('errors', () => {
  test('ScamperError.isFatal and SubthreadErrors expose phase and aggregated messages', () => {
    expect(new ScamperError('Runtime', 'oops').isFatal).toBe(true)
    expect(new ScamperError('Docstring', 'oops').isFatal).toBe(false)

    const errors = [
      new ScamperError('Runtime', 'first'),
      new ScamperError('Parser', 'second'),
    ]
    const err = new SubthreadErrors(errors)
    expect(err.message).toBe(errors.map((e) => e.toString()).join(' '))
    expect(err.errors).toEqual(errors)
  })
})

describe('schedulerYield', () => {
  test('prefers the native scheduler API when present', async () => {
    const nativeYield = vi.fn().mockResolvedValue(undefined)
    const globalWithScheduler = globalThis as typeof globalThis & {
      scheduler?: { yield(): Promise<void> }
    }
    globalWithScheduler.scheduler = { yield: nativeYield }
    // nativeScheduler is captured at module load, so a fresh import is
    // needed to pick up the mocked global.
    vi.resetModules()
    try {
      const { schedulerYield } = await import('../../src/lpm/scheduler-yield')
      await schedulerYield()
      expect(nativeYield).toHaveBeenCalledOnce()
    } finally {
      delete globalWithScheduler.scheduler
      vi.resetModules()
    }
  })
})

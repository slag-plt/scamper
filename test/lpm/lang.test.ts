import { describe, expect, test } from 'vitest'
import { callScamperFn, Env, JsFunction, Module, ScamperError, Value } from '../../src/lpm'

function mkModule(bindings: Record<string, unknown>): Module {
  const mod = new Module()
  for (const [name, value] of Object.entries(bindings)) {
    mod.registerValue(name, value as Value)
  }
  return mod
}

describe('Env.withoutLocals', () => {
  test('removes the named local while keeping other locals', () => {
    const env = Env.empty.extendWithLocals(['x', 1], ['y', 2])
    const trimmed = env.withoutLocals('x')
    expect(trimmed.has('x')).toBe(false)
    expect(trimmed.get('y')).toBe(2)
  })

  test('leaves locals untouched when no names are passed', () => {
    const env = Env.empty.extendWithLocals(['x', 1])
    const trimmed = env.withoutLocals()
    expect(trimmed.get('x')).toBe(1)
  })

  test('still resolves a top-level binding of the same name once shadowing local is removed', () => {
    const env = Env.empty
      .extendWithTopLevel(['x', 'top-level'])
      .extendWithLocals(['x', 'local'])
    const trimmed = env.withoutLocals('x')
    expect(trimmed.get('x')).toBe('top-level')
  })
})

describe('Module.fromLibs', () => {
  test('combines bindings from multiple modules', () => {
    const a = mkModule({ x: 1 })
    const b = mkModule({ y: 2 })
    const combined = Module.fromLibs(a, b)
    expect(combined.bindings.get('x')).toBe(1)
    expect(combined.bindings.get('y')).toBe(2)
  })

  test('later modules win on colliding names', () => {
    const a = mkModule({ x: 'first' })
    const b = mkModule({ x: 'second' })
    const combined = Module.fromLibs(a, b)
    expect(combined.bindings.get('x')).toBe('second')
  })
})

describe('callScamperFn', () => {
  test('always throws, regardless of the function or arguments passed', () => {
    const dummyFn: JsFunction = (...args: Value[]) => args[0]
    // eslint-disable-next-line @typescript-eslint/no-deprecated -- testing the deprecated stub itself
    expect(() => callScamperFn(dummyFn, 1, 2)).toThrow(ScamperError)
  })
})

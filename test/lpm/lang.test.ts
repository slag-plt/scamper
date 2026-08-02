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

describe('Module.registerValue', () => {
  test('assigns the registered name to a function value', () => {
    const mod = new Module()
    mod.registerValue('my-fn', ((x: number) => x) as Value)
    expect((mod.bindings.get('my-fn') as JsFunction).name).toBe('my-fn')
  })

  test('stores non-function values unchanged', () => {
    const mod = new Module()
    mod.registerValue('answer', 42)
    expect(mod.bindings.get('answer')).toBe(42)
  })
})

describe('Env.get resolution order', () => {
  test('a local binding shadows top-level and imported bindings', () => {
    const env = Env.empty
      .extendWithImport('m', mkModule({ x: 'import' }))
      .extendWithTopLevel(['x', 'top-level'])
      .extendWithLocals(['x', 'local'])
    expect(env.get('x')).toBe('local')
  })

  test('a top-level binding shadows an imported binding', () => {
    const env = Env.empty
      .extendWithImport('m', mkModule({ x: 'import' }))
      .extendWithTopLevel(['x', 'top-level'])
    expect(env.get('x')).toBe('top-level')
  })

  test('resolves from imports when neither local nor top-level bind the name', () => {
    const env = Env.empty.extendWithImport('m', mkModule({ x: 'import' }))
    expect(env.get('x')).toBe('import')
  })

  test('the most recently imported module wins on colliding names', () => {
    const env = Env.empty
      .extendWithImport('a', mkModule({ x: 'first' }))
      .extendWithImport('b', mkModule({ x: 'second' }))
    expect(env.get('x')).toBe('second')
  })

  test('throws a ScamperError when the name is unbound', () => {
    expect(() => Env.empty.get('nope')).toThrow(ScamperError)
  })
})

describe('Env.has', () => {
  test('true for a local, top-level, or imported binding; false otherwise', () => {
    const env = Env.empty
      .extendWithImport('m', mkModule({ imported: 1 }))
      .extendWithTopLevel(['top', 2])
      .extendWithLocals(['loc', 3])
    expect(env.has('loc')).toBe(true)
    expect(env.has('top')).toBe(true)
    expect(env.has('imported')).toBe(true)
    expect(env.has('missing')).toBe(false)
  })
})

describe('Env immutability', () => {
  test('extend* returns a new env and leaves the original unchanged', () => {
    const base = Env.empty.extendWithTopLevel(['x', 1])
    const extended = base.extendWithLocals(['y', 2])
    expect(extended.get('y')).toBe(2)
    expect(base.has('y')).toBe(false)
  })
})

describe('Env.extendReplacingLocals', () => {
  test('replaces the entire locals map rather than extending it', () => {
    const env = Env.empty.extendWithLocals(['x', 1], ['y', 2])
    const replaced = env.extendReplacingLocals(['z', 3])
    expect(replaced.get('z')).toBe(3)
    expect(replaced.has('x')).toBe(false)
    expect(replaced.has('y')).toBe(false)
  })

  test('leaves top-level bindings intact', () => {
    const env = Env.empty
      .extendWithTopLevel(['top', 'kept'])
      .extendWithLocals(['x', 1])
    expect(env.extendReplacingLocals(['y', 2]).get('top')).toBe('kept')
  })
})

describe('Env.getTopLevelAsModule', () => {
  test('returns a Module of exactly the top-level bindings, excluding locals', () => {
    const mod = Env.empty
      .extendWithTopLevel(['a', 1], ['b', 2])
      .extendWithLocals(['local', 3])
      .getTopLevelAsModule()
    expect(mod.bindings.get('a')).toBe(1)
    expect(mod.bindings.get('b')).toBe(2)
    expect(mod.bindings.has('local')).toBe(false)
  })
})

describe('Env.getLocals', () => {
  test('returns the map of local bindings', () => {
    const locals = Env.empty.extendWithLocals(['x', 1], ['y', 2]).getLocals()
    expect(locals.get('x')).toBe(1)
    expect(locals.get('y')).toBe(2)
  })
})

describe('callScamperFn', () => {
  test('always throws, regardless of the function or arguments passed', () => {
    const dummyFn: JsFunction = (...args: Value[]) => args[0]
    // eslint-disable-next-line @typescript-eslint/no-deprecated -- testing the deprecated stub itself
    expect(() => callScamperFn(dummyFn, 1, 2)).toThrow(ScamperError)
  })
})

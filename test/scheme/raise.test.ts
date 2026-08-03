import { describe, expect, test } from 'vitest'
import { expToString } from '../../src/scheme/ast.js'
import { raiseFiber, raiseFrames, valuesToExps } from '../../src/scheme/raise.js'
import * as LPM from '../../src/lpm/'
import { Frame } from '../../src/lpm/frame.js'
import { makeTestFiber } from '../util.js'

// The raiser lifts a running fiber's frames back into Scheme AST for
// pretty-printing (the inverse of codegen). These tests are organized by (1)
// the value-stack conversion, (2) each LPM instruction's reconstruction, (3)
// frame/fiber state, and (4) folding the value stack together with the
// remaining instruction stream. Assertions are on the pretty-printed string,
// since pretty-printing is the raiser's purpose.

// ---- Helpers ---------------------------------------------------------------

/**
 * Raise a single frame: a source-order `blk`, plus optional runtime values
 * already on the value stack (the mid-evaluation case) and an env. The Frame
 * constructor reverses `blk` internally, matching real execution.
 */
function raiseState(
  blk: LPM.Blk,
  opts: { values?: LPM.Value[]; env?: LPM.Env } = {},
): string {
  const fiber = makeTestFiber([])
  const frame = new Frame('f', opts.env ?? LPM.Env.empty, blk)
  if (opts.values) frame.values.push(...opts.values)
  fiber.pushFrame(frame)
  return expToString(raiseFiber(fiber))
}

/** Raise a freshly-pushed, unexecuted single frame. */
function raiseBlk(blk: LPM.Blk): string {
  return raiseState(blk)
}

/** A binary `+` function value; renders as its name (`+`) on the value stack. */
function plus(): LPM.Value {
  return LPM.nameFn('+', (a: number, b: number) => a + b) as LPM.Value
}

/** A named nullary function value, for building call-stack fixtures. */
function named(name: string): LPM.Value {
  return LPM.nameFn(name, () => 0) as LPM.Value
}

// ---- valuesToExps ----------------------------------------------------------

describe('valuesToExps (value stack -> expressions)', () => {
  test('primitive values render as their printed literals', () => {
    const vals: LPM.Value[] = [
      42, -3.5, 'hi', true, false, null, undefined,
      LPM.mkChar('a'), LPM.mkSym('sym'), [1, 2, 3], [],
    ]
    expect(valuesToExps(vals).map(expToString)).toEqual([
      '42', '-3.5', '"hi"', '#t', '#f', 'null', 'void',
      '#\\a', 'sym', '(vector 1 2 3)', '(vector)',
    ])
  })

  test('a struct value renders structurally', () => {
    const s = LPM.mkStruct('point', ['x', 'y'], [1, 2])
    expect(valuesToExps([s]).map(expToString)).toEqual(['(point 1 2)'])
  })

  test('a list value renders as (list ...)', () => {
    expect(valuesToExps([LPM.mkList(1, 2, 3)]).map(expToString)).toEqual([
      '(list 1 2 3)',
    ])
  })

  test('a named function value becomes an identifier (its name)', () => {
    expect(valuesToExps([plus()]).map(expToString)).toEqual(['+'])
  })

  test('a function without a usable name falls back to a literal', () => {
    const anon = LPM.nameFn('', () => 0) as LPM.Value
    expect(valuesToExps([anon]).map(expToString)[0]).toMatch(/^\[Function/)
  })

  test('order is preserved across multiple values', () => {
    expect(valuesToExps([1, 'a', true]).map(expToString)).toEqual([
      '1', '"a"', '#t',
    ])
  })

  test('an empty stack yields no expressions', () => {
    expect(valuesToExps([])).toEqual([])
  })
})

// ---- Per-instruction reconstruction ---------------------------------------

describe('raising each LPM instruction', () => {
  describe('lit', () => {
    test('numbers, strings, and booleans', () => {
      expect(raiseBlk([LPM.mkLit(42)])).toBe('42')
      expect(raiseBlk([LPM.mkLit(-3.5)])).toBe('-3.5')
      expect(raiseBlk([LPM.mkLit('hi')])).toBe('"hi"')
      expect(raiseBlk([LPM.mkLit(true)])).toBe('#t')
      expect(raiseBlk([LPM.mkLit(false)])).toBe('#f')
    })

    test('null and void', () => {
      expect(raiseBlk([LPM.mkLit(null)])).toBe('null')
      expect(raiseBlk([LPM.mkLit(undefined)])).toBe('void')
    })

    test('char, symbol, vector, struct, and list values', () => {
      expect(raiseBlk([LPM.mkLit(LPM.mkChar('a'))])).toBe('#\\a')
      expect(raiseBlk([LPM.mkLit(LPM.mkSym('sym'))])).toBe('sym')
      expect(raiseBlk([LPM.mkLit([1, 2, 3])])).toBe('(vector 1 2 3)')
      expect(
        raiseBlk([LPM.mkLit(LPM.mkStruct('point', ['x', 'y'], [1, 2]))]),
      ).toBe('(point 1 2)')
      expect(raiseBlk([LPM.mkLit(LPM.mkList(1, 2, 3))])).toBe('(list 1 2 3)')
    })
  })

  describe('var', () => {
    test('an unbound variable raises to its name', () => {
      expect(raiseBlk([LPM.mkVar('x')])).toBe('x')
    })

    test('a variable bound to a function raises to the reference name', () => {
      // uses the variable name, not the underlying function value's name
      const env = LPM.Env.empty.extendWithLocals(['g', plus()])
      expect(raiseState([LPM.mkVar('g')], { env })).toBe('g')
    })

    test('a variable bound to a non-function value substitutes the value', () => {
      expect(
        raiseState([LPM.mkVar('n')], {
          env: LPM.Env.empty.extendWithLocals(['n', 99]),
        }),
      ).toBe('99')
      expect(
        raiseState([LPM.mkVar('s')], {
          env: LPM.Env.empty.extendWithLocals(['s', 'hi']),
        }),
      ).toBe('"hi"')
    })

    test('a top-level binding resolves the same way as a local', () => {
      expect(
        raiseState([LPM.mkVar('n')], {
          env: LPM.Env.empty.extendWithTopLevel(['n', 7]),
        }),
      ).toBe('7')
    })
  })

  describe('cls', () => {
    test('a named closure raises to its name', () => {
      expect(
        raiseBlk([LPM.mkCls(['x'], [LPM.mkVar('x')], 'identity')]),
      ).toBe('identity')
    })

    test('an anonymous closure raises to a lambda', () => {
      expect(raiseBlk([LPM.mkCls(['x'], [LPM.mkVar('x')])])).toBe(
        '(lambda (x) x)',
      )
    })

    test('multiple parameters', () => {
      expect(raiseBlk([LPM.mkCls(['x', 'y'], [LPM.mkVar('y')])])).toBe(
        '(lambda (x y) y)',
      )
    })

    test('zero parameters', () => {
      expect(raiseBlk([LPM.mkCls([], [LPM.mkLit(1)])])).toBe('(lambda () 1)')
    })

    test('a rest parameter renders with &', () => {
      expect(
        raiseBlk([
          LPM.mkCls(['x'], [LPM.mkVar('rest')], undefined, undefined, 'rest'),
        ]),
      ).toBe('(lambda (x & rest) rest)')
    })

    test('parameters are excluded from the body, rendering as names not values', () => {
      // x is bound to 5 in the surrounding env, yet the body must show `x`
      const env = LPM.Env.empty.extendWithLocals(['x', 5])
      expect(raiseState([LPM.mkCls(['x'], [LPM.mkVar('x')])], { env })).toBe(
        '(lambda (x) x)',
      )
    })

    test('nested closures keep an outer parameter as a name', () => {
      expect(
        raiseBlk([LPM.mkCls(['x'], [LPM.mkCls(['y'], [LPM.mkVar('x')])])]),
      ).toBe('(lambda (x) (lambda (y) x))')
    })
  })

  describe('ap', () => {
    test('zero arguments (a thunk call)', () => {
      expect(raiseBlk([LPM.mkVar('f'), LPM.mkAp(0)])).toBe('(f)')
    })

    test('one argument', () => {
      expect(raiseBlk([LPM.mkVar('f'), LPM.mkLit(1), LPM.mkAp(1)])).toBe('(f 1)')
    })

    test('several arguments', () => {
      expect(
        raiseBlk([
          LPM.mkVar('f'),
          LPM.mkLit(1),
          LPM.mkLit(2),
          LPM.mkLit(3),
          LPM.mkAp(3),
        ]),
      ).toBe('(f 1 2 3)')
    })

    test('a computed operator and nested applications', () => {
      expect(
        raiseBlk([
          LPM.mkVar('+'),
          LPM.mkVar('*'),
          LPM.mkLit(2),
          LPM.mkLit(3),
          LPM.mkAp(2),
          LPM.mkLit(4),
          LPM.mkAp(2),
        ]),
      ).toBe('(+ (* 2 3) 4)')
    })

    test('an application in operator position', () => {
      expect(
        raiseBlk([LPM.mkVar('f'), LPM.mkAp(0), LPM.mkLit(1), LPM.mkAp(1)]),
      ).toBe('((f) 1)')
    })
  })

  describe('match', () => {
    test('a single literal branch', () => {
      expect(
        raiseBlk([
          LPM.mkLit(1),
          LPM.mkMatch([[LPM.mkPLit(1), [LPM.mkLit('one')]]]),
        ]),
      ).toBe('(match 1 [1 "one"])')
    })

    test('literal, variable, and wildcard patterns across branches', () => {
      const branches: [LPM.Pat, LPM.Blk][] = [
        [LPM.mkPLit(0), [LPM.mkLit('zero')]],
        [LPM.mkPVar('n'), [LPM.mkVar('n')]],
        [LPM.mkPWild(), [LPM.mkLit('other')]],
      ]
      expect(raiseBlk([LPM.mkLit(5), LPM.mkMatch(branches)])).toBe(
        '(match 5 [0 "zero"] [n n] [_ "other"])',
      )
    })

    test('a constructor pattern binds sub-variables', () => {
      expect(
        raiseBlk([
          LPM.mkVar('p'),
          LPM.mkMatch([
            [
              LPM.mkPCtor('pair', [LPM.mkPVar('a'), LPM.mkPVar('b')]),
              [LPM.mkVar('a')],
            ],
          ]),
        ]),
      ).toBe('(match p [(pair a b) a])')
    })

    test('a zero-argument constructor pattern', () => {
      expect(
        raiseBlk([
          LPM.mkVar('u'),
          LPM.mkMatch([[LPM.mkPCtor('unit', []), [LPM.mkLit(1)]]]),
        ]),
      ).toBe('(match u [(unit) 1])')
    })

    test('a nested constructor pattern', () => {
      const pat = LPM.mkPCtor('pair', [
        LPM.mkPCtor('pair', [LPM.mkPVar('a'), LPM.mkPVar('b')]),
        LPM.mkPVar('c'),
      ])
      expect(
        raiseBlk([LPM.mkVar('p'), LPM.mkMatch([[pat, [LPM.mkVar('a')]]])]),
      ).toBe('(match p [(pair (pair a b) c) a])')
    })

    test('branch pattern variables are excluded from the body (render as names)', () => {
      // n is bound to 5 in the env, but the branch body must show `n`
      const env = LPM.Env.empty.extendWithLocals(['n', 5])
      expect(
        raiseState(
          [LPM.mkLit(1), LPM.mkMatch([[LPM.mkPVar('n'), [LPM.mkVar('n')]]])],
          { env },
        ),
      ).toBe('(match 1 [n n])')
    })

    test('a computed scrutinee', () => {
      expect(
        raiseBlk([
          LPM.mkVar('f'),
          LPM.mkLit(1),
          LPM.mkAp(1),
          LPM.mkMatch([[LPM.mkPWild(), [LPM.mkLit(0)]]]),
        ]),
      ).toBe('(match (f 1) [_ 0])')
    })
  })

  describe('let (unstarted: idx 0)', () => {
    // A let that has not begun evaluating: all bindings come from their value
    // sub-blocks, and every binder renders as a name.
    const bind = (name: string, value: LPM.Blk) => ({
      pat: LPM.mkPVar(name),
      value,
    })

    test('a single binding', () => {
      expect(
        raiseBlk([
          LPM.mkLet([bind('x', [LPM.mkLit(1)])], [LPM.mkVar('x')]),
          LPM.mkPopScope(),
        ]),
      ).toBe('(let ([x 1]) x)')
    })

    test('multiple bindings', () => {
      expect(
        raiseBlk([
          LPM.mkLet(
            [bind('x', [LPM.mkLit(1)]), bind('y', [LPM.mkLit(2)])],
            [LPM.mkVar('y')],
          ),
          LPM.mkPopScope(),
        ]),
      ).toBe('(let ([x 1] [y 2]) y)')
    })

    test('a binder referenced in a later value renders as a name', () => {
      expect(
        raiseBlk([
          LPM.mkLet(
            [
              bind('x', [LPM.mkLit(1)]),
              bind('y', [LPM.mkVar('+'), LPM.mkVar('x'), LPM.mkLit(1), LPM.mkAp(2)]),
            ],
            [LPM.mkVar('y')],
          ),
          LPM.mkPopScope(),
        ]),
      ).toBe('(let ([x 1] [y (+ x 1)]) y)')
    })

    test('zero bindings', () => {
      expect(
        raiseBlk([LPM.mkLet([], [LPM.mkLit(1)]), LPM.mkPopScope()]),
      ).toBe('(let () 1)')
    })

    test('a constructor pattern binding', () => {
      expect(
        raiseBlk([
          LPM.mkLet(
            [
              {
                pat: LPM.mkPCtor('pair', [LPM.mkPVar('a'), LPM.mkPVar('b')]),
                value: [LPM.mkVar('p')],
              },
            ],
            [LPM.mkVar('a')],
          ),
          LPM.mkPopScope(),
        ]),
      ).toBe('(let ([(pair a b) p]) a)')
    })

    test('a computed binding value', () => {
      expect(
        raiseBlk([
          LPM.mkLet(
            [bind('x', [LPM.mkVar('f'), LPM.mkLit(1), LPM.mkAp(1)])],
            [LPM.mkVar('x')],
          ),
          LPM.mkPopScope(),
        ]),
      ).toBe('(let ([x (f 1)]) x)')
    })

    test('binders render as names even when an outer binding shadows them', () => {
      const env = LPM.Env.empty.extendWithLocals(['x', 99])
      expect(
        raiseState(
          [
            LPM.mkLet([bind('x', [LPM.mkLit(1)])], [LPM.mkVar('x')]),
            LPM.mkPopScope(),
          ],
          { env },
        ),
      ).toBe('(let ([x 1]) x)')
    })
  })

  describe('let (per-binding progress: idx > 0)', () => {
    // These reconstruct mid-let states so a trace shows progress. The Let op's
    // `idx` is the number of assigned bindings; the current binding's value is
    // on the stack, done bindings are substituted from the env, and pending
    // binders render as names. Tracks (let ([x (+ 1 1)] [y (+ x 1)]) y).
    const bindings = [
      {
        pat: LPM.mkPVar('x'),
        value: [LPM.mkVar('+'), LPM.mkLit(1), LPM.mkLit(1), LPM.mkAp(2)],
      },
      {
        pat: LPM.mkPVar('y'),
        value: [LPM.mkVar('+'), LPM.mkVar('x'), LPM.mkLit(1), LPM.mkAp(2)],
      },
    ]
    const body: LPM.Blk = [LPM.mkVar('y')]

    test("x's value has reduced to 2 but is not yet assigned", () => {
      // scope declared (both holes); 2 sits on the stack as x's computed value
      const result = raiseState(
        [LPM.mkLet(bindings, body, undefined, 1), LPM.mkPopScope()],
        { values: [2], env: LPM.Env.empty.declareScope(['x', 'y']) },
      )
      expect(result).toBe('(let ([x 2] [y (+ x 1)]) y)')
    })

    test('x is assigned and omitted; y = (+ x 1) is evaluating with x substituted', () => {
      const env = LPM.Env.empty.declareScope(['x', 'y'])
      env.assign('x', 2)
      const result = raiseState(
        [
          // y's value ops in flight, then the Let op at idx 2 (x already assigned)
          LPM.mkVar('+'),
          LPM.mkVar('x'),
          LPM.mkLit(1),
          LPM.mkAp(2),
          LPM.mkLet(bindings, body, undefined, 2),
          LPM.mkPopScope(),
        ],
        { env },
      )
      expect(result).toBe('(let ([y (+ 2 1)]) y)')
    })

    test("y's value has reduced to 3 but is not yet assigned", () => {
      const env = LPM.Env.empty.declareScope(['x', 'y'])
      env.assign('x', 2)
      const result = raiseState(
        [LPM.mkLet(bindings, body, undefined, 2), LPM.mkPopScope()],
        { values: [3], env },
      )
      expect(result).toBe('(let ([y 3]) y)')
    })
  })

  describe('if', () => {
    test('an unbound guard and literal branches', () => {
      expect(
        raiseBlk([LPM.mkVar('b'), LPM.mkIf([LPM.mkLit(1)], [LPM.mkLit(2)])]),
      ).toBe('(if b 1 2)')
    })

    test('a computed guard and branches', () => {
      expect(
        raiseBlk([
          LPM.mkVar('p'),
          LPM.mkLit(0),
          LPM.mkAp(1),
          LPM.mkIf(
            [LPM.mkVar('+'), LPM.mkLit(1), LPM.mkLit(2), LPM.mkAp(2)],
            [LPM.mkLit(0)],
          ),
        ]),
      ).toBe('(if (p 0) (+ 1 2) 0)')
    })

    test('nested ifs', () => {
      expect(
        raiseBlk([
          LPM.mkVar('a'),
          LPM.mkIf(
            [LPM.mkVar('b'), LPM.mkIf([LPM.mkLit(1)], [LPM.mkLit(2)])],
            [LPM.mkLit(3)],
          ),
        ]),
      ).toBe('(if a (if b 1 2) 3)')
    })
  })

  describe('pop-scope', () => {
    test('is transparent (reconstructs nothing)', () => {
      expect(raiseBlk([LPM.mkLit(5), LPM.mkPopScope()])).toBe('5')
    })

    test('multiple trailing pop-scopes are all transparent', () => {
      expect(
        raiseBlk([LPM.mkLit(5), LPM.mkPopScope(), LPM.mkPopScope()]),
      ).toBe('5')
    })
  })

  describe('ap-spread', () => {
    test('reconstructs an apply call', () => {
      expect(
        raiseBlk([LPM.mkVar('f'), LPM.mkVar('args'), LPM.mkApSpread()]),
      ).toBe('(apply f args)')
    })

    test('with a computed function and argument list', () => {
      expect(
        raiseBlk([
          LPM.mkVar('compose'),
          LPM.mkVar('f'),
          LPM.mkVar('g'),
          LPM.mkAp(2),
          LPM.mkVar('xs'),
          LPM.mkApSpread(),
        ]),
      ).toBe('(apply (compose f g) xs)')
    })
  })

  describe('push-handler / pop-handler (with-handler brackets)', () => {
    test('push-handler is transparent', () => {
      expect(raiseBlk([LPM.mkLit(1), LPM.mkPushHandler()])).toBe('1')
    })

    test('pop-handler drops the handler value and keeps the guarded result', () => {
      // stack is [handler, result]; pop-handler leaves the result
      expect(
        raiseBlk([LPM.mkVar('h'), LPM.mkLit(42), LPM.mkPopHandler()]),
      ).toBe('42')
    })

    test('a full with-handler bracket reconstructs the guarded call', () => {
      // with-handler's closure body: [var h, var t, push-handler, ap 0, pop-handler]
      expect(
        raiseBlk([
          LPM.mkVar('h'),
          LPM.mkVar('t'),
          LPM.mkPushHandler(),
          LPM.mkAp(0),
          LPM.mkPopHandler(),
        ]),
      ).toBe('(t)')
    })
  })
})

// ---- Frame and fiber state -------------------------------------------------

describe('frame and fiber state', () => {
  describe('a single frame', () => {
    test('fully unexecuted: reconstructs the whole block', () => {
      expect(
        raiseBlk([LPM.mkVar('+'), LPM.mkLit(1), LPM.mkLit(1), LPM.mkAp(2)]),
      ).toBe('(+ 1 1)')
    })

    test('partially executed: pre-computed values combine with remaining ops', () => {
      // + and 1 are already on the stack; the literal 2 and the ap remain
      expect(raiseState([LPM.mkLit(2), LPM.mkAp(2)], { values: [plus(), 1] })).toBe(
        '(+ 1 2)',
      )
    })

    test('finished frame: a lone value with no ops raises to that value', () => {
      expect(raiseState([], { values: [42] })).toBe('42')
    })

    test('with ops exhausted, the top of the value stack is the result', () => {
      expect(raiseState([], { values: [1, 2] })).toBe('2')
    })
  })

  describe('multiple frames (the call stack)', () => {
    test('an inner frame result becomes an argument in the outer frame', () => {
      // outer: (+ 1 _) with + and 1 pre-computed and the ap pending;
      // inner: (* 2 3). result: (+ 1 (* 2 3))
      const fiber = makeTestFiber([])
      const outer = new Frame('outer', LPM.Env.empty, [LPM.mkAp(2)])
      outer.values.push(plus(), 1)
      const inner = new Frame('inner', LPM.Env.empty, [
        LPM.mkVar('*'),
        LPM.mkLit(2),
        LPM.mkLit(3),
        LPM.mkAp(2),
      ])
      fiber.pushFrame(outer)
      fiber.pushFrame(inner)
      expect(expToString(raiseFiber(fiber))).toBe('(+ 1 (* 2 3))')
    })

    test('three frames deep: f(g(h()))', () => {
      const fiber = makeTestFiber([])
      const f = new Frame('f', LPM.Env.empty, [LPM.mkAp(1)])
      f.values.push(named('f'))
      const g = new Frame('g', LPM.Env.empty, [LPM.mkAp(1)])
      g.values.push(named('g'))
      const h = new Frame('h', LPM.Env.empty, [LPM.mkVar('h'), LPM.mkAp(0)])
      fiber.pushFrame(f)
      fiber.pushFrame(g)
      fiber.pushFrame(h)
      expect(expToString(raiseFiber(fiber))).toBe('(f (g (h)))')
    })
  })

  describe('edge cases', () => {
    test('a fiber with no frames raises an ICE', () => {
      expect(() => raiseFiber(makeTestFiber([]))).toThrow(LPM.ICE)
    })

    test('raiseFrames on an empty list raises an ICE', () => {
      expect(() => raiseFrames([])).toThrow(LPM.ICE)
    })
  })
})

// ---- Value stack folded with the instruction stream ------------------------

describe('folding the value stack with the instruction stream', () => {
  test('all operands pre-computed, only the ap remains', () => {
    expect(raiseState([LPM.mkAp(2)], { values: [plus(), 1, 2] })).toBe('(+ 1 2)')
  })

  test('operator pre-computed, operands supplied by ops', () => {
    expect(
      raiseState([LPM.mkLit(1), LPM.mkLit(2), LPM.mkAp(2)], { values: [plus()] }),
    ).toBe('(+ 1 2)')
  })

  test('operator and first operand pre-computed, second from ops', () => {
    expect(raiseState([LPM.mkLit(2), LPM.mkAp(2)], { values: [plus(), 1] })).toBe(
      '(+ 1 2)',
    )
  })

  test('a pre-computed value becomes a match scrutinee', () => {
    expect(
      raiseState([LPM.mkMatch([[LPM.mkPWild(), [LPM.mkLit(0)]]])], {
        values: [5],
      }),
    ).toBe('(match 5 [_ 0])')
  })

  test('a pre-computed function value renders as its name in the reconstruction', () => {
    expect(
      raiseState([LPM.mkLit(1), LPM.mkAp(1)], { values: [named('g')] }),
    ).toBe('(g 1)')
  })

  test('a pre-computed non-function value renders as a literal operator', () => {
    expect(raiseState([LPM.mkLit(1), LPM.mkAp(1)], { values: [42] })).toBe(
      '(42 1)',
    )
  })
})

// ---- Frame.canTailCall (the TCO predicate the raiser's frames rely on) ------

describe('Frame.canTailCall (TCO predicate)', () => {
  test('true when no ops remain', () => {
    expect(new Frame('f', LPM.Env.empty, []).canTailCall()).toBe(true)
  })

  test('true when only pop-scope ops remain', () => {
    expect(
      new Frame('f', LPM.Env.empty, [
        LPM.mkPopScope(),
        LPM.mkPopScope(),
      ]).canTailCall(),
    ).toBe(true)
  })

  test('false when any non-pop-scope op remains', () => {
    expect(
      new Frame('f', LPM.Env.empty, [
        LPM.mkLit(1),
        LPM.mkPopScope(),
      ]).canTailCall(),
    ).toBe(false)
  })
})

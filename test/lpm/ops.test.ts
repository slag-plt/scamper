import { beforeEach, describe, expect, test } from 'vitest'
import { Fiber } from '../../src/lpm/fiber'
import * as U from '../../src/lpm/util'
import {
  ICE,
  LoggingChannel,
  OutputChannel,
  Value,
} from '../../src/lpm'
import { makeTestFiber } from '../util'

function testExecute(fiber: Fiber, out: OutputChannel) {
  // execute fiber until it's done
  while (!fiber.isDone()) {
    const res = fiber.step()
    if (res.tag === 'display') {
      out.send(fiber.lastResult)
    }
  }
}

describe('basic ops', () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  function expectSuccessfulExec(fiber: Fiber) {
    expect(() => {
      testExecute(fiber, out)
    }).not.toThrow()
  }

  function expectFailedExec(
    fiber: Fiber,
    matcher?: RegExp | (new (...args: never[]) => Error),
  ) {
    expect(() => {
      testExecute(fiber, out)
    }).toThrow(matcher)
  }

  const litCases: [string, Value][] = [
    ['number', 42],
    ['string', 'hi'],
    ['empty string', ''],
    ['boolean', false],
    ['null', null],
    ['void', undefined],
    ['char', U.mkChar('a')],
    ['symbol', U.mkSym('x')],
    ['vector', [1, 2, 3]],
    ['nested struct', U.mkStruct('point', ['x', 'y'], [1, 2])],
  ]
  test.for(litCases)('lit %s', ([, lit]) => {
    const fiber = makeTestFiber([U.mkDisp([U.mkLit(lit)])])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([lit])
  })

  describe('var', () => {
    const varCases: [string, Value][] = [
      ['/', (a: number, b: number) => a / b],
      ['a', 42],
      ['var2', null],
      ['woah', 'wee'],
    ]
    test.for(varCases)('exists: %s -> %o', ([name, value]) => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar(name)])])
      fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([name, value])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([value])
    })

    test("doesn't exist", () => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar('test-bad-var')])])
      expectFailedExec(fiber)
    })
  })

  test('cls', () => {
    const clsBody = [U.mkVar('+'), U.mkVar('x'), U.mkLit(1), U.mkAp(2)]
    const fiber = makeTestFiber([
      U.mkDisp([U.mkCls(['x'], clsBody, 'add-one'), U.mkLit(1), U.mkAp(1)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log.at(0)).toStrictEqual(2)
  })

  test('cls: a returned closure captures its defining scope', () => {
    // ((lambda (x) (lambda (y) (+ x y))) 10) applied to 5 => 15
    const inner = U.mkCls(
      ['y'],
      [U.mkVar('+'), U.mkVar('x'), U.mkVar('y'), U.mkAp(2)],
      'inner',
    )
    const outer = U.mkCls(['x'], [inner], 'outer')
    const fiber = makeTestFiber([
      U.mkDisp([outer, U.mkLit(10), U.mkAp(1), U.mkLit(5), U.mkAp(1)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([15])
  })

  test('ap', () => {
    const fiber = makeTestFiber([
      U.mkDisp([U.mkVar('+'), U.mkLit(3), U.mkLit(4), U.mkAp(2)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([7])
  })

  test('ap without enough values on the stack throws an ICE', () => {
    const fiber = makeTestFiber([U.mkDisp([U.mkVar('+'), U.mkAp(2)])])
    expectFailedExec(fiber, ICE)
  })

  describe('ap-spread', () => {
    test('spreads a list as call arguments', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkVar('+'), U.mkLit(U.mkList(3, 4)), U.mkApSpread()]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([7])
    })

    test('malformed args: arg value is not a list', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkVar('+'), U.mkLit(42), U.mkApSpread()]),
      ])
      expectFailedExec(fiber)
    })

    test('without enough values on the stack throws an ICE', () => {
      const fiber = makeTestFiber([U.mkDisp([U.mkVar('+'), U.mkApSpread()])])
      expectFailedExec(fiber, ICE)
    })

    test('applying a non-function, non-closure value', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit(42), U.mkLit(U.mkList()), U.mkApSpread()]),
      ])
      expectFailedExec(fiber)
    })

    test('a JS function throwing a non-Scamper error gets wrapped', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkVar('boom'), U.mkLit(U.mkList()), U.mkApSpread()]),
      ])
      fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([
        'boom',
        () => {
          throw new TypeError('kaboom')
        },
      ])
      expect(() => {
        testExecute(fiber, out)
      }).toThrow(/Unexpected error in Javascript function call/)
    })
  })

  describe('match', () => {
    test('w/ plit', () => {
      const ifBranch = [U.mkLit('matched')]
      const elseBranch = [U.mkLit('not matched')]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkMatch([
            [U.mkPLit(42), ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toEqual(['matched'])
    })

    test('w/ second pattern', () => {
      const ifBranch = [U.mkLit('wrong match')]
      const elseBranch = [U.mkLit('other one')]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkMatch([
            [U.mkPLit(99), ifBranch],
            [U.mkPLit(42), elseBranch],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toEqual(['other one'])
    })

    test('failed', () => {
      const ifBranch = [U.mkLit('wrong match')]
      const elseBranch = [U.mkLit('other one')]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkMatch([
            [U.mkPLit(99), ifBranch],
            [U.mkPLit(67), elseBranch],
          ]),
        ]),
      ])
      expectFailedExec(fiber)
    })

    test('w/ pvar', () => {
      const ifBranch = [U.mkVar('+'), U.mkVar('x'), U.mkLit(10), U.mkAp(2)]
      const elseBranch = [U.mkLit(0)]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(5),
          U.mkMatch([
            [U.mkPVar('x'), ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([15])
    })

    test('w/ pwild', () => {
      const ifBranch = [U.mkLit('always matches')]
      const elseBranch = [U.mkLit('never reached')]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit('anything'),
          U.mkMatch([
            [U.mkPWild(), ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['always matches'])
    })

    test('w/ pctor', () => {
      const testStruct = [
        U.mkLit(U.mkStruct('test-struct', ['field1', 'field2'], [1, 2])),
      ]
      const ifBranch = [U.mkVar('+'), U.mkVar('a'), U.mkVar('b'), U.mkAp(2)]
      const elseBranch = [U.mkLit('no match')]
      const pattern = U.mkPCtor('test-struct', [U.mkPVar('a'), U.mkPVar('b')])
      const fiber = makeTestFiber([
        U.mkDisp([
          ...testStruct,
          U.mkMatch([
            [pattern, ifBranch],
            [U.mkPWild(), elseBranch],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([3])
    })

    test('missing scrutinee throws an ICE', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkMatch([[U.mkPWild(), [U.mkLit('unreached')]]])]),
      ])
      expectFailedExec(fiber, ICE)
    })

    // plit uses structural `equals`, so each literal kind must match its own
    // value (chars compare by value; numbers/strings/booleans/null by identity
    // or primitive equality).
    const plitCases: [string, Value][] = [
      ['number', 0],
      ['string', 'hi'],
      ['boolean', false],
      ['null', null],
      ['char', U.mkChar('a')],
    ]
    test.for(plitCases)('plit matches a %s literal', ([, v]) => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(v),
          U.mkMatch([
            [U.mkPLit(v), [U.mkLit('matched')]],
            [U.mkPWild(), [U.mkLit('fallthrough')]],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['matched'])
    })

    test('w/ nested pctor binds sub-pattern variables', () => {
      // scrutinee (pair (pair 1 2) 3); pattern (pair (pair a b) c) => a+b+c = 6
      const pattern = U.mkPCtor('pair', [
        U.mkPCtor('pair', [U.mkPVar('a'), U.mkPVar('b')]),
        U.mkPVar('c'),
      ])
      const body = [
        U.mkVar('+'),
        U.mkVar('a'),
        U.mkVar('+'),
        U.mkVar('b'),
        U.mkVar('c'),
        U.mkAp(2),
        U.mkAp(2),
      ]
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(
            U.mkStruct('pair', ['fst', 'snd'], [
              U.mkStruct('pair', ['fst', 'snd'], [1, 2]),
              3,
            ]),
          ),
          U.mkMatch([
            [pattern, body],
            [U.mkPWild(), [U.mkLit(-1)]],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([6])
    })

    test('w/ zero-arg pctor', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(U.mkStruct('unit', [], [])),
          U.mkMatch([
            [U.mkPCtor('unit', []), [U.mkLit('matched')]],
            [U.mkPWild(), [U.mkLit('no')]],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['matched'])
    })

    test('pctor with a mismatched ctor name falls through', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(U.mkStruct('point', ['x', 'y'], [1, 2])),
          U.mkMatch([
            [U.mkPCtor('other', [U.mkPWild(), U.mkPWild()]), [U.mkLit('wrong')]],
            [U.mkPWild(), [U.mkLit('fallthrough')]],
          ]),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['fallthrough'])
    })
  })

  test('define', () => {
    const fiber = makeTestFiber([
      U.mkDefine('x', [U.mkLit(1)]),
      U.mkDisp([U.mkVar('+'), U.mkLit(1), U.mkVar('x'), U.mkAp(2)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([2])
  })

  test('factorial', () => {
    const factorialCls = U.mkCls(
      ['n'],
      [
        U.mkVar('n'),
        U.mkMatch([
          [U.mkPLit(0), [U.mkLit(1)]],
          [
            U.mkPWild(),
            [
              U.mkVar('*'),
              U.mkVar('n'),
              U.mkVar('fact'),
              U.mkVar('-'),
              U.mkVar('n'),
              U.mkLit(1),
              U.mkAp(2),
              U.mkAp(1),
              U.mkAp(2),
            ],
          ],
        ]),
      ],
      'fact',
    )
    const fiber = makeTestFiber([
      U.mkDefine('fact', [factorialCls]),
      U.mkDisp([U.mkVar('fact'), U.mkLit(5), U.mkAp(1)]),
    ])
    expectSuccessfulExec(fiber)
    expect(out.log).toStrictEqual([120])
  })

  describe('if', () => {
    test('a #t guard runs the then-branch', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit(true), U.mkIf([U.mkLit('then')], [U.mkLit('else')])]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['then'])
    })

    test('a #f guard runs the else-branch', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit(false), U.mkIf([U.mkLit('then')], [U.mkLit('else')])]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['else'])
    })

    test('the chosen branch is evaluated, not returned as a literal block', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(true),
          U.mkIf([U.mkVar('+'), U.mkLit(1), U.mkLit(2), U.mkAp(2)], [U.mkLit(0)]),
        ]),
      ])
      fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([
        '+',
        (a: number, b: number) => a + b,
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([3])
    })

    test('a non-boolean guard throws a clear runtime error', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit(5), U.mkIf([U.mkLit('then')], [U.mkLit('else')])]),
      ])
      expectFailedExec(fiber, /expected a boolean guard/)
    })

    test('no guard on the stack throws an ICE', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkIf([U.mkLit('then')], [U.mkLit('else')])]),
      ])
      expectFailedExec(fiber, ICE)
    })
  })

  describe('let', () => {
    test('a single pvar binding is visible in the body', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(42),
          U.mkLet([U.mkPVar('x')], [U.mkVar('x')]),
          U.mkPopScope(),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([42])
    })

    test('bindings are positional and non-telescoping (p1<-v1, p2<-v2)', () => {
      // (- a b) = -1 confirms a<-1 (first value) and b<-2 (second), not swapped.
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(1),
          U.mkLit(2),
          U.mkLet(
            [U.mkPVar('a'), U.mkPVar('b')],
            [U.mkVar('-'), U.mkVar('a'), U.mkVar('b'), U.mkAp(2)],
          ),
          U.mkPopScope(),
        ]),
      ])
      fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([
        '-',
        (a: number, b: number) => a - b,
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([-1])
    })

    test('a constructor pattern destructures the bound value', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(U.mkStruct('pair', ['fst', 'snd'], [3, 4])),
          U.mkLet(
            [U.mkPCtor('pair', [U.mkPVar('a'), U.mkPVar('b')])],
            [U.mkVar('b')],
          ),
          U.mkPopScope(),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([4])
    })

    test('a wildcard binding runs the value for effect and returns the body', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(99),
          U.mkLet([U.mkPWild()], [U.mkLit(42)]),
          U.mkPopScope(),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([42])
    })

    test('a matching literal pattern binds nothing and runs the body', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(7),
          U.mkLet([U.mkPLit(7)], [U.mkLit('ok')]),
          U.mkPopScope(),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['ok'])
    })

    test('zero bindings run the body directly (no stack splicing)', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLet([], [U.mkLit(7)]), U.mkPopScope()]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([7])
    })

    test('a void binding value is bound, not mistaken for an empty stack', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(undefined),
          U.mkLet([U.mkPVar('x')], [U.mkLit('body')]),
          U.mkPopScope(),
        ]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual(['body'])
    })

    test('a value that does not match its pattern throws the failMsg', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(5),
          U.mkLet(
            [U.mkPCtor('pair', [U.mkPVar('a'), U.mkPVar('b')])],
            [U.mkVar('a')],
            undefined,
            'let: nope',
          ),
          U.mkPopScope(),
        ]),
      ])
      expectFailedExec(fiber, /let: nope/)
    })

    test('fewer values on the stack than patterns throws an ICE', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit(1), U.mkLet([U.mkPVar('a'), U.mkPVar('b')], [U.mkVar('a')]), U.mkPopScope()]),
      ])
      expectFailedExec(fiber, ICE)
    })

    test('a binder does not leak past its pop-scope', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(1),
          U.mkLet([U.mkPVar('x')], [U.mkVar('x')]),
          U.mkPopScope(),
          U.mkVar('x'),
        ]),
      ])
      expectFailedExec(fiber, /Variable not found/)
    })
  })

  describe('pop-scope', () => {
    test('is a no-op when there are no local scopes', () => {
      const fiber = makeTestFiber([U.mkDisp([U.mkLit(5), U.mkPopScope()])])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([5])
    })

    test('a match branch binder does not leak past the trailing pop-scope', () => {
      const fiber = makeTestFiber([
        U.mkDisp([
          U.mkLit(7),
          U.mkMatch([[U.mkPVar('m'), [U.mkVar('m')]]]),
          U.mkPopScope(),
          U.mkVar('m'),
        ]),
      ])
      expectFailedExec(fiber, /Variable not found/)
    })
  })

  describe('with-handler opcodes', () => {
    test('push-handler with fewer than two stack values throws an ICE', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit('only-one'), U.mkPushHandler()]),
      ])
      expectFailedExec(fiber, ICE)
    })

    test('pop-handler with fewer than two stack values throws an ICE', () => {
      const fiber = makeTestFiber([
        U.mkDisp([U.mkLit('only-one'), U.mkPopHandler()]),
      ])
      expectFailedExec(fiber, ICE)
    })
  })
})

describe('rest parameters', () => {
  let out: LoggingChannel
  beforeEach(() => {
    out = new LoggingChannel(false, false)
  })

  function expectSuccessfulExec(fiber: Fiber) {
    expect(() => {
      testExecute(fiber, out)
    }).not.toThrow()
  }

  function expectFailedExec(fiber: Fiber) {
    expect(() => {
      testExecute(fiber, out)
    }).toThrow(/Arity mismatch/)
  }

  describe('ap w/ rest param', () => {
    test('zero extra args binds an empty list', () => {
      const cls = U.mkCls(['x'], [U.mkVar('y')], 'f', undefined, 'y')
      const fiber = makeTestFiber([U.mkDisp([cls, U.mkLit(1), U.mkAp(1)])])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([null])
    })

    test('one extra arg binds a single-element list', () => {
      const cls = U.mkCls(['x'], [U.mkVar('y')], 'f', undefined, 'y')
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkAp(2)]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([U.mkList(2)])
    })

    test('multiple extra args bind a multi-element list, in call order', () => {
      const cls = U.mkCls(['x'], [U.mkVar('y')], 'f', undefined, 'y')
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkLit(3), U.mkAp(3)]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([U.mkList(2, 3)])
    })

    test('fixed params still resolve to their own arguments', () => {
      const cls = U.mkCls(
        ['x', 'y'],
        [U.mkVar('+'), U.mkVar('x'), U.mkVar('y'), U.mkAp(2)],
        'f',
        undefined,
        'z',
      )
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkLit(3), U.mkAp(3)]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([3])
    })

    test('rest param is a proper list, not a JS array/vector', () => {
      const cls = U.mkCls(['x'], [U.mkVar('y')], 'f', undefined, 'y')
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkAp(2)]),
      ])
      expectSuccessfulExec(fiber)
      const result = out.log.at(0)
      expect(Array.isArray(result)).toBe(false)
    })

    test('does not throw when call has exactly the fixed arity', () => {
      const cls = U.mkCls(['x', 'y'], [U.mkVar('x')], 'f', undefined, 'z')
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkAp(2)]),
      ])
      expectSuccessfulExec(fiber)
      expect(out.log).toStrictEqual([1])
    })
  })

  describe('ap arity mismatch', () => {
    test('fewer than required args, rest param present', () => {
      const cls = U.mkCls(['x', 'y'], [U.mkVar('x')], 'f', undefined, 'z')
      const fiber = makeTestFiber([U.mkDisp([cls, U.mkLit(1), U.mkAp(1)])])
      expectFailedExec(fiber)
    })

    test('zero args against a rest-only lambda with one required param', () => {
      const cls = U.mkCls(['x'], [U.mkVar('x')], 'f', undefined, 'y')
      const fiber = makeTestFiber([U.mkDisp([cls, U.mkAp(0)])])
      expectFailedExec(fiber)
    })

    test('fewer than required args, no rest param', () => {
      const cls = U.mkCls(['x', 'y'], [U.mkVar('x')], 'f')
      const fiber = makeTestFiber([U.mkDisp([cls, U.mkLit(1), U.mkAp(1)])])
      expectFailedExec(fiber)
    })

    test('more args than fixed params, no rest param', () => {
      const cls = U.mkCls(['x'], [U.mkVar('x')], 'f')
      const fiber = makeTestFiber([
        U.mkDisp([cls, U.mkLit(1), U.mkLit(2), U.mkAp(2)]),
      ])
      expectFailedExec(fiber)
    })
  })
})

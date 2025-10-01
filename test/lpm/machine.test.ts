import { expect, test, describe } from 'vitest'
import * as L from '../../src/lpm'
import * as U from '../../src/lpm/util.js'
import { LoggingOutputChannel, LoggingErrorChannel } from "../../src/lpm/output.js"

// A stub builtin libraries map for testing purposes
const builtinLibs: Map<string, L.Library> = new Map()

const env = (() => {
  const ret = new L.Env()
  ret.set('+', ((a: number, b: number) => a + b))
  ret.set('-', ((a: number, b: number) => a - b))
  ret.set('*', ((a: number, b: number) => a * b))
  ret.set('/', ((a: number, b: number) => a / b))
  return ret
})()

function makeMachine (blk: L.Blk): [L.Machine, LoggingOutputChannel, LoggingErrorChannel] {
  const out = new LoggingOutputChannel()
  const err = new LoggingErrorChannel()
  const machine = new L.Machine(
    builtinLibs,
    env,
    blk,
    out,
    err
  )
  return [machine, out, err]
}

describe('basic ops', () => {
  test('lit', () => {
    const [machine, out, _] = makeMachine([U.mkLit(42), U.mkDisp(), U.mkLit(undefined)])
    machine.evaluate()
    expect(out.log).toEqual([42])
  })

  test('var', () => {
    const [machine, out, _] = makeMachine([U.mkVar('+'), U.mkDisp(), U.mkLit(undefined)])
    machine.evaluate()
    expect(out.log).toEqual([env.get('+')!])
  })

  test('ctor', () => {
    const [machine, out, _] = makeMachine([U.mkLit('test'), U.mkLit(2), U.mkCtor('test-ctor', ['a', 'b']), U.mkDisp(), U.mkLit(undefined)])
    machine.evaluate()
    const result = out.log[0] as L.Value
    expect(result).toEqual(U.mkStruct('test-ctor', ['a', 'b'], ['test', 2]))
  })

  test('cls', () => {
    const body = [U.mkVar('+'), U.mkVar('x'), U.mkLit(1), U.mkAp(2)]
    const [machine, out, _] = makeMachine([
      U.mkCls(['x'], body, 'add-one'),
      U.mkLit(1),
      U.mkAp(1),
      U.mkDisp(),
      U.mkLit(undefined)]
    )
    machine.evaluate()
    const result = out.log[0] as L.Value
    expect(result).toBe(2)
  })

  test('ap', () => {
    const [machine, out, _] = makeMachine([
      U.mkVar('+'),
      U.mkLit(3),
      U.mkLit(4),
      U.mkAp(2),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([7])
  })

  test('match - successful pattern', () => {
    const ifBranch = [U.mkLit('matched')]
    const elseBranch = [U.mkLit('not matched')]
    const [machine, out, _] = makeMachine([
      U.mkLit(42),
      U.mkMatch([
        [U.mkPLit(42), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['matched'])
  })

  test('match - failed pattern', () => {
    const ifBranch = [U.mkLit('matched')]
    const elseBranch = [U.mkLit('not matched')]
    const [machine, out, _] = makeMachine([
      U.mkLit(42),
      U.mkMatch([
        [U.mkPLit(99), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['not matched'])
  })

  test('match - variable pattern', () => {
    const ifBranch = [U.mkVar('+'), U.mkVar('x'), U.mkLit(10), U.mkAp(2)]
    const elseBranch = [U.mkLit(0)]
    const [machine, out, _] = makeMachine([
      U.mkLit(5),
      U.mkMatch([
        [U.mkPVar('x'), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([15])
  })

  test('match - wildcard pattern', () => {
    const ifBranch = [U.mkLit('always matches')]
    const elseBranch = [U.mkLit('never reached')]
    const [machine, out, _] = makeMachine([
      U.mkLit('anything'),
      U.mkMatch([
        [U.mkPWild(), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['always matches'])
  })

  test('disp', () => {
    const [machine, out, _] = makeMachine([U.mkLit('hello world'), U.mkDisp(), U.mkLit(undefined)])
    machine.evaluate()
    expect(out.log).toEqual(['hello world'])
  })

  test('raise', () => {
    const [machine, _out, err] = makeMachine([U.mkRaise('test error')])
    machine.evaluate()
    expect(err.log.length).toBe(1)
  })

  test('pop - environment restoration', () => {
    // Create a nested environment scenario
    const innerBody = [U.mkVar('x'), U.mkDisp(), U.mkPops()]
    const [machine, out, _] = makeMachine([
      U.mkLit(42),
      U.mkMatch([
        [U.mkPVar('x'), innerBody],
        [U.mkPWild(), [U.mkLit('fail')]]
      ]),
      U.mkLit('after pop'),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([42, 'after pop'])
  })
})

describe('pattern matching', () => {
  test('pwild - matches anything', () => {
    const ifBranch = [U.mkLit('matched')]
    const elseBranch = [U.mkLit('not matched')]
    const [machine, out, _] = makeMachine([
      U.mkLit('any value'),
      U.mkMatch([
        [U.mkPWild(), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['matched'])
  })

  test('plit - literal pattern match', () => {
    const ifBranch = [U.mkLit('number matched')]
    const elseBranch = [U.mkLit('number not matched')]
    const [machine, out, _] = makeMachine([
      U.mkLit(123),
      U.mkMatch([
        [U.mkPLit(123), ifBranch], 
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['number matched'])
  })

  test('pvar - variable binding', () => {
    const ifBranch = [U.mkVar('+'), U.mkVar('captured'), U.mkLit(' was captured'), U.mkAp(2)]
    const elseBranch = [U.mkLit('no match')]
    const [machine, out, _] = makeMachine([
      U.mkLit('hello'),
      U.mkMatch([
        [U.mkPVar('captured'), ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['hello was captured'])
  })

  test('pctor - constructor pattern', () => {
    // First create a struct to match against
    const setupStruct = [U.mkLit(1), U.mkLit(2), U.mkCtor('test-struct', ['field1', 'field2'])]
    const ifBranch = [U.mkVar('+'), U.mkVar('a'), U.mkVar('b'), U.mkAp(2), U.mkPops()]
    const elseBranch = [U.mkRaise('no match'), U.mkPops(), U.mkLit(undefined)]
    const pattern = U.mkPCtor('test-struct', [U.mkPVar('a'), U.mkPVar('b')])
    
    const [machine, out, _] = makeMachine([
      ...setupStruct,
      U.mkMatch([
        [pattern, ifBranch],
        [U.mkPWild(), elseBranch]
      ]),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([3])
  })

  test('factorial', () => {
    const factorial = [
      U.mkCls(
        ['n'], [
          U.mkVar('n'),
          U.mkMatch([
            [U.mkPLit(0), [U.mkLit(1)]],
            [U.mkPWild(), [
              U.mkVar('*'),
                U.mkVar('n'),
                U.mkVar('fact'),
                  U.mkVar('-'),
                  U.mkVar('n'),
                  U.mkLit(1),
                  U.mkAp(2),
                U.mkAp(1),
              U.mkAp(2)
            ]]
      ])], 'fact')]
    const [machine, out, _] = makeMachine([
      ...factorial,
      U.mkDefine('fact'),
      U.mkVar('fact'),
      U.mkLit(5),
      U.mkAp(1),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([120])
  })
})
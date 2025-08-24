import { expect, test, describe } from "@jest/globals"
import * as LPM from '../../src/lpm'
import * as U from '../../src/lpm/util.js'
import { LoggingOutputChannel, LoggingErrorChannel } from "../../src/lpm/output.js"

// A stub builtin libraries map for testing purposes
const builtinLibs: Map<string, [string, LPM.Value][]> = new Map()

const env = (() => {
  const ret = new LPM.Env()
  ret.set('+', ((a: number, b: number) => a + b))
  ret.set('-', ((a: number, b: number) => a - b))
  ret.set('*', ((a: number, b: number) => a * b))
  ret.set('/', ((a: number, b: number) => a / b))
  return ret
})()

function makeMachine (exp: LPM.Exp): [LPM.Machine, LoggingOutputChannel, LoggingErrorChannel] {
  const out = new LoggingOutputChannel()
  const err = new LoggingErrorChannel()
  const machine = new LPM.Machine(
    builtinLibs,
    env,
    exp,
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
    const result = out.log[0] as LPM.Value
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
    const result = out.log[0] as LPM.Value
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
      U.mkMatch(U.mkPLit(42), ifBranch, elseBranch),
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
      U.mkMatch(U.mkPLit(99), ifBranch, elseBranch),
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
      U.mkMatch(U.mkPVar('x'), ifBranch, elseBranch),
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
      U.mkMatch(U.mkPWild(), ifBranch, elseBranch),
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
    const [machine, _out, _err] = makeMachine([U.mkRaise('test error')])
    expect(() => machine.evaluate()).toThrow('test error')
  })

  test('pop - environment restoration', () => {
    // Create a nested environment scenario
    const innerBody = [U.mkVar('x'), U.mkDisp(), U.mkPop()]
    const [machine, out, _] = makeMachine([
      U.mkLit(42),
      U.mkMatch(U.mkPVar('x'), innerBody, [U.mkLit('fail')]),
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
      U.mkMatch(U.mkPWild(), ifBranch, elseBranch),
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
      U.mkMatch(U.mkPLit(123), ifBranch, elseBranch),
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
      U.mkMatch(U.mkPVar('captured'), ifBranch, elseBranch),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual(['hello was captured'])
  })

  test('pctor - constructor pattern', () => {
    // First create a struct to match against
    const setupStruct = [U.mkLit(1), U.mkLit(2), U.mkCtor('test-struct', ['field1', 'field2'])]
    const ifBranch = [U.mkVar('+'), U.mkVar('a'), U.mkVar('b'), U.mkAp(2), U.mkPop()]
    const elseBranch = [U.mkRaise('no match'), U.mkPop(), U.mkLit(undefined)]
    const pattern = U.mkPCtor('test-struct', [U.mkPVar('a'), U.mkPVar('b')])
    
    const [machine, out, _] = makeMachine([
      ...setupStruct,
      U.mkMatch(pattern, ifBranch, elseBranch),
      U.mkDisp(),
      U.mkLit(undefined)
    ])
    machine.evaluate()
    expect(out.log).toEqual([3])
  })
})
import { expect, test, describe } from "@jest/globals"
import { expToString } from '../../src/scheme/ast.js'
import { raiseFrames } from '../../src/scheme/raise.js'
import * as LPM from '../../src/lpm/'

test('basic complete binary raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkVar('+'),
      LPM.mkLit(1),
      LPM.mkLit(1),
      LPM.mkAp(2),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(+ 1 1)')
})

test('basic mid-evaluation raising', () => {
  const fn = (x: number, y: number) => x + y
  LPM.nameFn('+', fn)
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(1),
      LPM.mkAp(2),
    ])
  ]
  frames[0].values.push(fn, 1)
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(+ 1 1)')
})

test('mkVar operation raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkVar('x'),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('x')
})

test('mkLit operation raising with number', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(42),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('42')
})

test('mkLit operation raising with string', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit('hello'),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('"hello"')
})

test('mkLit operation raising with boolean', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(true),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('true')
})

test('mkCtor operation raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(1),
      LPM.mkLit(2),
      LPM.mkCtor('pair', ['fst', 'snd']),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(pair 1 2)')
})

test('mkCls operation raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkCls(['x'], [LPM.mkVar('x')], 'identity'),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(lambda (x) x)')
})

test('mkAp operation raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkVar('f'),
      LPM.mkLit(10),
      LPM.mkLit(20),
      LPM.mkAp(2),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(f 10 20)')
})

test('mkMatch operation raising', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(1), [LPM.mkLit('one')]],
    [LPM.mkPWild(), [LPM.mkLit('other')]]
  ]
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(1),
      LPM.mkMatch(branches),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(match 1 [1 "one"] [_ "other"])')
})

test('mkRaise operation raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkRaise('Test error message'),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(raise "Test error message")')
})


test('nested operations raising', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkVar('+'),
      LPM.mkVar('*'),
      LPM.mkLit(2),
      LPM.mkLit(3),
      LPM.mkAp(2),
      LPM.mkLit(4),
      LPM.mkAp(2),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(+ (* 2 3) 4)')
})

test('complex constructor with multiple fields', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit('John'),
      LPM.mkLit(30),
      LPM.mkLit('Engineer'),
      LPM.mkCtor('person', ['name', 'age', 'job']),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(person "John" 30 "Engineer")')
})

test('lambda with multiple parameters', () => {
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkCls(['x', 'y'], [LPM.mkVar('x')], 'add'),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(lambda (x y) x)')
})

test('pattern matching with multiple branches', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(0), [LPM.mkLit('zero')]],
    [LPM.mkPLit(1), [LPM.mkLit('one')]],
    [LPM.mkPVar('n'), [LPM.mkVar('n')]]
  ]
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit(5),
      LPM.mkMatch(branches),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(match 5 [0 "zero"] [1 "one"] [n n])')
})

test('pattern matching with constructor pattern', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPCtor('pair', [LPM.mkPVar('x'), LPM.mkPVar('y')]), [LPM.mkVar('x')]]
  ]
  const frames = [
    new LPM.Frame('f1', new LPM.Env(), [
      LPM.mkLit('test'),
      LPM.mkMatch(branches),
    ])
  ]
  const result = raiseFrames(frames)
  expect(expToString(result)).toBe('(match "test" [(pair x y) x])')
})
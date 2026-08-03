import { expect, test } from 'vitest'
import { expToString } from '../../src/scheme/ast.js'
import { raiseFiber } from '../../src/scheme/raise.js'
import * as LPM from '../../src/lpm/'
import { Frame } from '../../src/lpm/frame.js'
import { makeTestFiber } from '../util.js'

function raiseBlk(blk: LPM.Blk): string {
  const fiber = makeTestFiber([])
  fiber.pushFrame(new Frame('f1', LPM.Env.empty, blk))
  return expToString(raiseFiber(fiber))
}

test('basic complete binary raising', () => {
  const result = raiseBlk([
    LPM.mkVar('+'),
    LPM.mkLit(1),
    LPM.mkLit(1),
    LPM.mkAp(2),
  ])
  expect(result).toBe('(+ 1 1)')
})

test('basic mid-evaluation raising', () => {
  const fn = (x: number, y: number) => x + y
  LPM.nameFn('+', fn)
  const fiber = makeTestFiber([])
  const frame = new Frame('f1', LPM.Env.empty, [LPM.mkLit(1), LPM.mkAp(2)])
  frame.values.push(fn, 1)
  fiber.pushFrame(frame)
  const result = expToString(raiseFiber(fiber))
  expect(result).toBe('(+ 1 1)')
})

test('mkVar operation raising', () => {
  const result = raiseBlk([LPM.mkVar('x')])
  expect(result).toBe('x')
})

test('mkLit operation raising with number', () => {
  const result = raiseBlk([LPM.mkLit(42)])
  expect(result).toBe('42')
})

test('mkLit operation raising with string', () => {
  const result = raiseBlk([LPM.mkLit('hello')])
  expect(result).toBe('"hello"')
})

test('mkLit operation raising with boolean', () => {
  const result = raiseBlk([LPM.mkLit(true)])
  expect(result).toBe('#t')
})

test('mkCtor operation raising', () => {
  const result = raiseBlk([
    LPM.mkLit(1),
    LPM.mkLit(2),
    LPM.mkCtor('pair', ['fst', 'snd']),
  ])
  expect(result).toBe('(pair 1 2)')
})

test('mkCls operation raising', () => {
  const result = raiseBlk([LPM.mkCls(['x'], [LPM.mkVar('x')], 'identity')])
  expect(result).toBe('identity')
})

test('mkAp operation raising', () => {
  const result = raiseBlk([
    LPM.mkVar('f'),
    LPM.mkLit(10),
    LPM.mkLit(20),
    LPM.mkAp(2),
  ])
  expect(result).toBe('(f 10 20)')
})

test('mkMatch operation raising', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(1), [LPM.mkLit('one')]],
    [LPM.mkPWild(), [LPM.mkLit('other')]],
  ]
  const result = raiseBlk([LPM.mkLit(1), LPM.mkMatch(branches)])
  expect(result).toBe('(match 1 [1 "one"] [_ "other"])')
})

test('nested operations raising', () => {
  const result = raiseBlk([
    LPM.mkVar('+'),
    LPM.mkVar('*'),
    LPM.mkLit(2),
    LPM.mkLit(3),
    LPM.mkAp(2),
    LPM.mkLit(4),
    LPM.mkAp(2),
  ])
  expect(result).toBe('(+ (* 2 3) 4)')
})

test('complex constructor with multiple fields', () => {
  const result = raiseBlk([
    LPM.mkLit('John'),
    LPM.mkLit(30),
    LPM.mkLit('Engineer'),
    LPM.mkCtor('person', ['name', 'age', 'job']),
  ])
  expect(result).toBe('(person "John" 30 "Engineer")')
})

test('lambda with multiple parameters', () => {
  const result = raiseBlk([LPM.mkCls(['x', 'y'], [LPM.mkVar('x')], 'add')])
  expect(result).toBe('add')
})

test('pattern matching with multiple branches', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPLit(0), [LPM.mkLit('zero')]],
    [LPM.mkPLit(1), [LPM.mkLit('one')]],
    [LPM.mkPVar('n'), [LPM.mkVar('n')]],
  ]
  const result = raiseBlk([LPM.mkLit(5), LPM.mkMatch(branches)])
  expect(result).toBe('(match 5 [0 "zero"] [1 "one"] [n n])')
})

test('pattern matching with constructor pattern', () => {
  const branches: [LPM.Pat, LPM.Blk][] = [
    [LPM.mkPCtor('pair', [LPM.mkPVar('x'), LPM.mkPVar('y')]), [LPM.mkVar('x')]],
  ]
  const result = raiseBlk([LPM.mkLit('test'), LPM.mkMatch(branches)])
  expect(result).toBe('(match "test" [(pair x y) x])')
})

test('var operation raising resolves a bound function to its name', () => {
  const fn = () => 42
  const env = LPM.Env.empty.extendWithLocals(['f', fn])
  const fiber = makeTestFiber([])
  fiber.pushFrame(new Frame('f1', env, [LPM.mkVar('f')]))
  const result = expToString(raiseFiber(fiber))
  expect(result).toBe('f')
})

test('var operation raising resolves a bound non-function value as a literal', () => {
  const env = LPM.Env.empty.extendWithLocals(['n', 99])
  const fiber = makeTestFiber([])
  fiber.pushFrame(new Frame('f1', env, [LPM.mkVar('n')]))
  const result = expToString(raiseFiber(fiber))
  expect(result).toBe('99')
})

test('mkCls operation raising without a name produces a lambda expression', () => {
  const result = raiseBlk([LPM.mkCls(['x'], [LPM.mkVar('x')])])
  expect(result).toBe('(lambda (x) x)')
})

test('ap-spread operation raising', () => {
  const result = raiseBlk([LPM.mkVar('f'), LPM.mkVar('args'), LPM.mkApSpread()])
  expect(result).toBe('(apply f args)')
})

test('Frame.canTailCall is true only when nothing but pop-scope ops remain', () => {
  // The TCO predicate that preserves tail calls through let/match bodies.
  expect(new Frame('f', LPM.Env.empty, []).canTailCall()).toBe(true)
  expect(
    new Frame('f', LPM.Env.empty, [LPM.mkPopScope(), LPM.mkPopScope()]).canTailCall(),
  ).toBe(true)
  expect(
    new Frame('f', LPM.Env.empty, [LPM.mkLit(1), LPM.mkPopScope()]).canTailCall(),
  ).toBe(false)
})

test('if operation raising', () => {
  const result = raiseBlk([
    LPM.mkVar('b'),
    LPM.mkIf([LPM.mkLit(1)], [LPM.mkLit(2)]),
  ])
  expect(result).toBe('(if b 1 2)')
})

test('let operation raising', () => {
  const result = raiseBlk([
    LPM.mkLit(1),
    LPM.mkLet([LPM.mkPVar('x')], [LPM.mkVar('x')]),
    LPM.mkPopScope(),
  ])
  expect(result).toBe('(let ([x 1]) x)')
})

test('let with a constructor pattern raising', () => {
  const result = raiseBlk([
    LPM.mkVar('p'),
    LPM.mkLet(
      [LPM.mkPCtor('pair', [LPM.mkPVar('a'), LPM.mkPVar('b')])],
      [LPM.mkVar('a')],
    ),
    LPM.mkPopScope(),
  ])
  expect(result).toBe('(let ([(pair a b) p]) a)')
})

test('let with multiple bindings raising', () => {
  const result = raiseBlk([
    LPM.mkLit(1),
    LPM.mkLit(2),
    LPM.mkLet([LPM.mkPVar('x'), LPM.mkPVar('y')], [LPM.mkVar('x')]),
    LPM.mkPopScope(),
  ])
  expect(result).toBe('(let ([x 1] [y 2]) x)')
})

test('pop-scope is transparent to raising', () => {
  const result = raiseBlk([LPM.mkLit(5), LPM.mkPopScope()])
  expect(result).toBe('5')
})

test('a let binder shadowing an outer local raises as a name, not its value', () => {
  // x=99 is in the frame env; the let rebinds x, so the body's `x` must render
  // as the variable itself, not the substituted outer value.
  const fiber = makeTestFiber([])
  const env = LPM.Env.empty.extendWithLocals(['x', 99])
  fiber.pushFrame(
    new Frame('f1', env, [
      LPM.mkLit(1),
      LPM.mkLet([LPM.mkPVar('x')], [LPM.mkVar('x')]),
      LPM.mkPopScope(),
    ]),
  )
  expect(expToString(raiseFiber(fiber))).toBe('(let ([x 1]) x)')
})

test('raiseFrames throws when there are no frames to raise', () => {
  const fiber = makeTestFiber([])
  expect(() => raiseFiber(fiber)).toThrow(LPM.ICE)
})

test('raiseFrames folds partial frames across the call stack', () => {
  const fn = (x: number, y: number) => x + y
  LPM.nameFn('+', fn)
  const fiber = makeTestFiber([])
  const outer = new Frame('outer', LPM.Env.empty, [LPM.mkAp(2)])
  outer.values.push(fn, 1)
  const inner = new Frame('inner', LPM.Env.empty, [
    LPM.mkVar('*'),
    LPM.mkLit(2),
    LPM.mkLit(3),
    LPM.mkAp(2),
  ])
  fiber.pushFrame(outer)
  fiber.pushFrame(inner)
  const result = expToString(raiseFiber(fiber))
  expect(result).toBe('(+ 1 (* 2 3))')
})

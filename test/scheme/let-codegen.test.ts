import { expect, test, describe } from "@jest/globals"
import * as A from '../../src/scheme/ast.js'
import { lowerProgram } from '../../src/scheme/codegen.js'
import * as L from '../../src/lpm'
import * as U from '../../src/lpm/util.js'
import { LoggingOutputChannel, LoggingErrorChannel } from "../../src/lpm/output.js"

describe('Let codegen and execution tests', () => {
  test('let bindings should be parallel - codegen structure', () => {
    // Create AST for: (let ([x 1] [y 2]) (list x y))
    const letStmt = A.mkStmtExp(A.mkLet([
      { name: 'x', value: A.mkLit(1) },
      { name: 'y', value: A.mkLit(2) }
    ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')])))
    
    const prog = [letStmt]
    const lowered = lowerProgram(prog, false)
    
    // The lowered code should evaluate both literals first, then bind them
    // First two operations should be the literals (parallel evaluation)
    expect(lowered[0].tag).toBe('lit')
    expect(lowered[0].value).toBe(1)
    expect(lowered[1].tag).toBe('lit')
    expect(lowered[1].value).toBe(2)
    
    // Then should be match operations for binding
    expect(lowered[2].tag).toBe('match')
  })

  test('let bindings should be parallel - runtime behavior', () => {
    // Test that demonstrates the bug: (let ([x (+ x 1)] [y (+ x 1)]) (list x y))
    // with outer x = 10, should produce (11 11), not (11 12)
    
    // Create AST 
    const letStmt = A.mkStmtExp(A.mkLet([
      { name: 'x', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) },
      { name: 'y', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) }
    ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')])))
    
    const prog = [letStmt]
    const lowered = lowerProgram(prog, true) // Display result
    
    // Set up environment with necessary functions
    const env = new L.Env()
    env.set('x', 10)  // outer x
    env.set('+', (a: number, b: number) => a + b)
    env.set('list', (...args: any[]) => args)
    
    // Create and run machine
    const builtinLibs: Map<string, L.Library> = new Map()
    const out = new LoggingOutputChannel()
    const err = new LoggingErrorChannel()
    const machine = new L.Machine(builtinLibs, env, lowered, out, err)
    
    machine.evaluate()
    
    // Should get (11 11) for parallel semantics, not (11 12) for sequential
    // Check the output log instead of machine.result
    expect(out.log.length).toBe(1)
    expect(out.log[0]).toEqual([11, 11])
  })

  test('let bindings should be parallel - issue example', () => {
    // Test the exact example from the issue:
    // (define sample3 (lambda (x) (list x (let ([x (+ x 1)] [y (+ x 1)]) (list x y)))))
    // (sample3 10) should produce (10 (11 11))
    
    const defineStmt = A.mkDefine('sample3', A.mkLam(['x'], 
      A.mkApp(A.mkVar('list'), [
        A.mkVar('x'),
        A.mkLet([
          { name: 'x', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) },
          { name: 'y', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) }
        ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')]))
      ])
    ))
    
    const callStmt = A.mkStmtExp(A.mkApp(A.mkVar('sample3'), [A.mkLit(10)]))
    
    const prog = [defineStmt, callStmt]
    const lowered = lowerProgram(prog, true) // Display result
    
    // Set up environment with necessary functions
    const env = new L.Env()
    env.set('+', (a: number, b: number) => a + b)
    env.set('list', (...args: any[]) => args)
    
    // Create and run machine
    const builtinLibs: Map<string, L.Library> = new Map()
    const out = new LoggingOutputChannel()
    const err = new LoggingErrorChannel()
    const machine = new L.Machine(builtinLibs, env, lowered, out, err)
    
    machine.evaluate()
    
    // Should get (10 (11 11)) for parallel semantics, not (10 (11 12)) for sequential
    expect(out.log.length).toBe(1)
    expect(out.log[0]).toEqual([10, [11, 11]])
  })
})
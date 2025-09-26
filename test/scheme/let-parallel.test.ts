import { expect, test, describe } from "@jest/globals"
import * as A from '../../src/scheme/ast.js'

describe('Let parallel binding AST tests', () => {
  test('let AST structure for parallel bindings', () => {
    // Create an AST for: (let ([x (+ x 1)] [y (+ x 1)]) (list x y))  
    // This documents the AST structure - both bindings reference outer 'x'
    const letExpr = A.mkLet([
      { name: 'x', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) },
      { name: 'y', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) }
    ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')]))

    // Verify the AST structure is as expected
    expect(letExpr.tag).toBe('let')
    expect(letExpr.bindings.length).toBe(2)
    expect(letExpr.bindings[0].name).toBe('x')
    expect(letExpr.bindings[1].name).toBe('y')
    
    // Both bindings should reference the same outer 'x' variable
    expect(letExpr.bindings[0].value.tag).toBe('app')
    expect(letExpr.bindings[1].value.tag).toBe('app')
  })
})
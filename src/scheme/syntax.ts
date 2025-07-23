import { ScamperError, Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import { unpackSyntax } from './ast.js'

const reservedWords = [
  'and',
  'begin',
  'cond',
  'define',
  'if',
  'import',
  'lambda',
  'let',
  'let*',
  'letrec',
  'match',
  'or',
  'quote',
  'section',
  'struct',
]

function checkIdentifier (errors: ScamperError[], v: Value, errorMsg: string = 'Expected an identifier') {
  let { range, value } = unpackSyntax(v)
  v = value
  if (!R.isSym(v)) {
    errors.push(new ScamperError('Parser', errorMsg, undefined, range))
  }
  const s = (v as R.Sym).value 
  if (reservedWords.includes(s)) {
    errors.push(new ScamperError('Parser', `The identifier "${s}" is a reserved word and cannot be used as a variable name`, undefined, range))
  }
}

function checkIdentifierList (errors: ScamperError[], v: Value,
                             listErrorMsg: string = 'Expected a list of identifiers',
                             identErrorMsg: string = 'Expected an identifier') {
  let { range, value } = unpackSyntax(v)
  v = value
  if (!R.isList(v)) {
    errors.push(new ScamperError('Parser', listErrorMsg, undefined, range))
  } else {
    const arr = R.listToVector(v as R.List)
    arr.forEach((v) => {
      checkIdentifier(errors, v, identErrorMsg)
    })
  }
}

function checkBranch (errors: ScamperError[], v: Value) {
  let { range, value } = unpackSyntax(v)
  v = value
  if (!R.isPair(v)) {
    errors.push(new ScamperError('Parser', 'Expected a pair for a branch', undefined, range))
  } else {
    const p = v as R.Pair
    checkSyntaxExpr(errors, p.fst)
    checkSyntaxExpr(errors, p.snd)
  }
}

function checkLetBinder (errors: ScamperError[], v: Value) {
  let { range, value } = unpackSyntax(v)
  v = value
  if (!R.isPair(v)) {
    errors.push(new ScamperError('Parser', 'Expected a pair for a binder', undefined, range))
  } else {
    const p = v as R.Pair
    checkIdentifier(errors, p.fst, 'Binding pair expects an identifier in the first position')
    checkSyntaxExpr(errors, p.snd)
  }
}

function checkLetBinders (errors: ScamperError[], v: Value) {
  let { range, value } = unpackSyntax(v)
  v = value
  if (!R.isList(v)) {
    errors.push(new ScamperError('Parser', 'Expected a list of binding pairs', undefined, range))
  } else {
    const arr = R.listToVector(v as R.List)
    arr.forEach((v) => {
      checkLetBinder(errors, v)
    })
  }
}

function checkSyntaxExprList (errors: ScamperError[], vs: Iterable<Value>) {
  for (const v of vs) {
    checkSyntaxExpr(errors, v)
  }
}

///// Main entry points ////////////////////////////////////////////////////////

export function checkSyntaxSingle (errors: ScamperError[], v: Value) {
  // N.B., only need to check for syntactic correctness of identifier
  // expressions. Other atomic expression' are checked at parsing time.
  if (R.isSym(v)) {
    checkIdentifier(errors, v, 'Expected an identifier')
  }
}

export function checkSyntaxExpr (errors: ScamperError[], v: Value) {
  let { range, value } = unpackSyntax(v)
  v = value

  if (!R.isList(v)) {
    checkSyntaxSingle(errors, v)
  } else {
    const arr = R.listToVector(v as R.List)
    if (arr.length > 0) {
      let { range: hr, value: hv } = unpackSyntax(arr[0])
      if (R.isSym(hv)) {
        const head = (hv as R.Sym).value
        switch (head) {
          case 'lambda': {
            if (arr.length !== 3) {
              errors.push(new ScamperError('Parser', 'Lambda expressions must have 2 sub-components, a list of identifiers and a body', undefined, range))
            }
            checkIdentifierList(errors, arr[1],
              'The first component of a lambda expression must be a list of identifiers',
              'The parameters of a lambda expression must be identifiers')
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'let': {
            if (arr.length !== 3) {
              errors.push(new ScamperError('Parser', 'Let expressions must have 2 sub-components, a list of binding pairs and a body', undefined, range))
            }
            checkLetBinders(errors, arr[1])
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'let*': {
            if (arr.length !== 3) {
              errors.push(new ScamperError('Parser', 'Let* expressions must have 2 sub-components, a list of binding pairs and a body', undefined, range))
            }
            checkLetBinders(errors, arr[1])
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'and': {
            checkSyntaxExprList(errors, arr.slice(1))
            break
          }

          case 'or': {
            checkSyntaxExprList(errors, arr.slice(1))
            break
          }

          case 'if': {
            if (arr.length !== 4) {
              errors.push(new ScamperError('Parser', 'If expressions must have 3 sub-components: a guard, an if-branch, and an else-branch', undefined, range))
            }
            checkSyntaxExpr(errors, arr[1])
            checkSyntaxExpr(errors, arr[2])
            checkSyntaxExpr(errors, arr[3])
            break
          }

          case 'begin': {
            checkSyntaxExprList(errors, arr.slice(1))
            break
          }

          case 'match': {
            if (arr.length < 2) {
              errors.push(new ScamperError('Parser', 'Match expressions must have at least 1 sub-component: a scrutine', undefined, range))
            }
            checkSyntaxExpr(errors, arr[1])
            for (const v of arr.slice(2)) {
              checkBranch(errors, v)
            }
            break
          }

          case 'cond': {
            for (const v of arr.slice(1)) {
              checkBranch(errors, v)
            }
            break
          }

          case 'quote': {
            if (arr.length === 1) {
              errors.push(new ScamperError('Parser', 'Quote expressions must have at least one sub-component', undefined, range))
            }
            checkSyntaxExprList(errors, arr.slice(1))
            break
          }

          case 'section': {
            if (arr.length === 1) {
              errors.push(new ScamperError('Parser', 'Section expressions must have at least one sub-component', undefined, range))
            }
            checkSyntaxExprList(errors, arr.slice(1))
            break
          }

          default: {
            checkSyntaxExprList(errors, arr)
            break
          }
        }
      }
    }
  }
}

export function checkSyntaxStmt (errors: ScamperError[], v: Value) {
  let { range, value } = unpackSyntax(v)
  const orig = v
  v = value
  if (!R.isList(v)) {
    checkSyntaxExpr(errors,v)
  } else {
    const arr = R.listToVector(v as R.List)
    if (arr.length > 0) {
      let { range: _hr, value: hv } = unpackSyntax(arr[0])
      if (R.isSym(hv)) {
        const head = (hv as R.Sym).value
        switch (head) {
          case 'import': {
            if (arr.length !== 2) {
              errors.push(new ScamperError('Parser', 'Import statements must have 2 sub-components, a module name and an alias', undefined, range))
            }
            checkIdentifier(errors, arr[1], 'The first component of an import statement must be an identifier')
            break
          }

          case 'define': {
            if (arr.length !== 3) {
              errors.push(new ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, range))
            }
            checkIdentifier(errors, arr[1], 'The first component of a define statement must be an identifier')
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'display': {
            if (arr.length !== 2) {
              errors.push(new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, range))
            }
            checkSyntaxExpr(errors, arr[1])
            break
          }

          case 'struct': {
            if (arr.length !== 3) {
              errors.push(new ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, range))
            }
            checkIdentifier(errors, arr[1], 'The first component of a struct statement must be an identifier')
            checkIdentifierList(errors, arr[2],
              'The second component of a struct statement must be a list of identifiers',
              'The fields of a struct must be identifiers')
            break
          }

          default: {
            checkSyntaxExpr(errors, orig)
            break
          }
        }
      } else {
        checkSyntaxExpr(errors, orig)
      }
    }
  }
}

export function checkSyntaxProgram (program: Value[]): ScamperError[] {
  const errors: ScamperError[] = []
  program.forEach((v, _) => {
    checkSyntaxStmt(errors, v)
  })
  return errors
}
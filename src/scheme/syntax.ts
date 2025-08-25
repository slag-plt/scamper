import * as L from '../lpm'
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

function checkIdentifier (errors: L.ScamperError[], v: L.Value, errorMsg: string = 'Expected an identifier') {
  let { value, metadata } = unpackSyntax(v)
  v = value
  if (!L.isSym(v)) {
    errors.push(new L.ScamperError('Parser', errorMsg, undefined, metadata.get('range')))
  }
  const s = (v as L.Sym).value 
  if (reservedWords.includes(s)) {
    errors.push(new L.ScamperError('Parser', `The identifier "${s}" is a reserved word and cannot be used as a variable name`, undefined, metadata.get('range')))
  }
}

function checkIdentifierList (errors: L.ScamperError[], v: L.Value,
                             listErrorMsg: string = 'Expected a list of identifiers',
                             identErrorMsg: string = 'Expected an identifier') {
  let { value, metadata } = unpackSyntax(v)
  v = value
  if (!L.isList(v)) {
    errors.push(new L.ScamperError('Parser', listErrorMsg, undefined, metadata.get('range')))
  } else {
    const arr = L.listToVector(v as L.List)
    arr.forEach((v) => {
      checkIdentifier(errors, v, identErrorMsg)
    })
  }
}

function checkBranch (errors: L.ScamperError[], v: L.Value) {
  let { value, metadata } = unpackSyntax(v)
  v = value
  if (!L.isPair(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a pair for a branch', undefined, metadata.get('range')))
  } else {
    const p = v as L.Pair
    checkSyntaxExpr(errors, p.fst)
    checkSyntaxExpr(errors, p.snd)
  }
}

function checkLetBinder (errors: L.ScamperError[], v: L.Value) {
  let { value, metadata } = unpackSyntax(v)
  v = value
  if (!L.isPair(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a pair for a binder', undefined, metadata.get('range')))
  } else {
    const p = v as L.Pair
    checkIdentifier(errors, p.fst, 'Binding pair expects an identifier in the first position')
    checkSyntaxExpr(errors, p.snd)
  }
}

function checkLetBinders (errors: L.ScamperError[], v: L.Value) {
  let { metadata, value } = unpackSyntax(v)
  v = value
  if (!L.isList(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a list of binding pairs', undefined, metadata.get('range')))
  } else {
    const arr = L.listToVector(v as L.List)
    arr.forEach((v) => {
      checkLetBinder(errors, v)
    })
  }
}

function checkSyntaxExprList (errors: L.ScamperError[], vs: Iterable<L.Value>) {
  for (const v of vs) {
    checkSyntaxExpr(errors, v)
  }
}

///// Main entry points ////////////////////////////////////////////////////////

export function checkSyntaxSingle (errors: L.ScamperError[], v: L.Value) {
  // N.B., only need to check for syntactic correctness of identifier
  // expressions. Other atomic expressions are checked at parsing time.
  if (L.isSym(v)) {
    checkIdentifier(errors, v, 'Expected an identifier')
  }
}

export function checkSyntaxExpr (errors: L.ScamperError[], v: L.Value) {
  let { metadata, value } = unpackSyntax(v)
  v = value

  if (!L.isList(v)) {
    checkSyntaxSingle(errors, v)
  } else {
    const arr = L.listToVector(v as L.List)
    if (arr.length > 0) {
      let { value: hv, metadata: _hm } = unpackSyntax(arr[0])
      if (L.isSym(hv)) {
        const head = (hv as L.Sym).value
        switch (head) {
          case 'lambda': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Lambda expressions must have 2 sub-components, a list of identifiers and a body', undefined, metadata.get('range')))
            }
            checkIdentifierList(errors, arr[1],
              'The first component of a lambda expression must be a list of identifiers',
              'The parameters of a lambda expression must be identifiers')
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'let': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Let expressions must have 2 sub-components, a list of binding pairs and a body', undefined, metadata.get('range')))
            }
            checkLetBinders(errors, arr[1])
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'let*': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Let* expressions must have 2 sub-components, a list of binding pairs and a body', undefined, metadata.get('range')))
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
              errors.push(new L.ScamperError('Parser', 'If expressions must have 3 sub-components: a guard, an if-branch, and an else-branch', undefined, metadata.get('range')))
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
              errors.push(new L.ScamperError('Parser', 'Match expressions must have at least 1 sub-component: a scrutine', undefined, metadata.get('range')))
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
              errors.push(new L.ScamperError('Parser', 'Quote expressions must have at least one sub-component', undefined, metadata.get('range')))
            }
            // N.B., We _don't_ check the syntax of the quoted expression!
            // We treat the syntax as-is, even if it is malformed. It is only
            // when we evaluate the syntax would we then check its validity.
            break
          }

          case 'section': {
            if (arr.length === 1) {
              errors.push(new L.ScamperError('Parser', 'Section expressions must have at least one sub-component', undefined, metadata.get('range')))
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

export function checkSyntaxStmt (errors: L.ScamperError[], v: L.Value) {
  let { value, metadata } = unpackSyntax(v)
  const orig = v
  v = value
  if (!L.isList(v)) {
    checkSyntaxExpr(errors,v)
  } else {
    const arr = L.listToVector(v as L.List)
    if (arr.length > 0) {
      let { value: hv, metadata: hm } = unpackSyntax(arr[0])
      if (L.isSym(hv)) {
        const head = (hv as L.Sym).value
        switch (head) {
          case 'import': {
            if (arr.length !== 2) {
              errors.push(new L.ScamperError('Parser', 'Import statements must have 2 sub-components, a module name and an alias', undefined, metadata.get('range')))
            }
            checkIdentifier(errors, arr[1], 'The first component of an import statement must be an identifier')
            break
          }

          case 'define': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, metadata.get('range')))
            }
            checkIdentifier(errors, arr[1], 'The first component of a define statement must be an identifier')
            checkSyntaxExpr(errors, arr[2])
            break
          }

          case 'display': {
            if (arr.length !== 2) {
              errors.push(new L.ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, metadata.get('range')))
            }
            checkSyntaxExpr(errors, arr[1])
            break
          }

          case 'struct': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, metadata.get('range')))
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

export function checkSyntaxProgram (program: L.Value[]): L.ScamperError[] {
  const errors: L.ScamperError[] = []
  program.forEach((v, _) => {
    checkSyntaxStmt(errors, v)
  })
  return errors
}
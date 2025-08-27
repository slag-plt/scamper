import * as A from './ast.js'
import * as L from '../lpm'
import * as S from './syntax.js'

// TODO: need to check whether _ is used correctly here, i.e., only under a section

///// Syntax objects (to be moved to syntax.ts after the port) /////////////////

////////////////////////////////////////////////////////////////////////////////

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

// Placeholders for incomplete programs to allow parsing to continue in the presence of errors
const phExp = A.mkLit(undefined)
const phStmt = A.mkStmtExp(A.mkLit(undefined))

function parseIdentifier (errors: L.ScamperError[], v: L.Value, errorMsg: string = 'Expected an identifier'): string {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isSym(v)) {
    errors.push(new L.ScamperError('Parser', errorMsg, undefined, range))
    return '<error>'
  }
  const ret = v.value 
  if (reservedWords.includes(ret)) {
    errors.push(new L.ScamperError('Parser', `The identifier "${ret}" is a reserved word and cannot be used as a variable name`, undefined, range))
    return '<error>'
  }
  return ret
}

function parsePat (errors: L.ScamperError[], v: L.Value): A.Pat {
  let { value, range } = S.unpackSyntax(v)
  const orig = v
  v = value
  if (L.isList(v)) {
    const arr = L.listToVector(v)
    if (arr.length === 0) {
      return A.mkPLit(null, range)
    }
    const head = parseIdentifier(errors, arr[0], 'The first element of a pattern list must be a constructor name')
    const args = arr.slice(1).map((v) => parsePat(errors, v))
    return A.mkPCtor(head, args, range)
  } else if (L.isSym(v)) {
    const name = parseIdentifier(errors, orig, 'Expected a valid constructor name')
    return A.mkPVar(name, range)
  } else {
    return L.mkPLit(v)
  }
}

function parseIdentifierList (errors: L.ScamperError[], v: L.Value,
                             listErrorMsg: string = 'Expected a list of identifiers',
                             identErrorMsg: string = 'Expected an identifier'): string[] {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isList(v)) {
    errors.push(new L.ScamperError('Parser', listErrorMsg, undefined, range))
    return ['<error>']
  } else {
    const arr = L.listToVector(v as L.List)
    const idents = arr.map((v) => parseIdentifier(errors, v, identErrorMsg))
    return idents
  }
}

function parseBranch (errors: L.ScamperError[], v: L.Value): { pat: A.Pat, body: A.Exp } {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isArray(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a vector for a match branch', undefined, range))
    return { pat: L.mkPLit('<error>'), body: phExp }
  } else {
    if (v.length !== 2) {
      errors.push(new L.ScamperError('Parser', 'Expected a vecotr of size 2 for a match branch', undefined, range))
      return { pat: L.mkPLit('<error>'), body: phExp }
    }
    const pat = parsePat(errors, v[0])
    const body = parseExp(errors, v[1])
    return { pat, body }
  }
}

function parseCondBranch (errors: L.ScamperError[], v: L.Value): { test: A.Exp, body: A.Exp } {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isArray(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a vector for a cond branch', undefined, range))
    return { test: L.mkLit('<error>'), body: phExp }
  } else {
    if (v.length !== 2) {
      errors.push(new L.ScamperError('Parser', 'Expected a vector of size 2 for a cond branch', undefined, range))
      return { test: L.mkLit('<error>'), body: phExp }
    }
    const test = parseExp(errors, v[0])
    const body = parseExp(errors, v[1])
    return { test, body }
  }
}

function parseLetBinder (errors: L.ScamperError[], v: L.Value): { name: string, value: A.Exp } {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isArray(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a vector for a binder', undefined, range))
    return { name: '<error>', value: phExp }
  } else {
    if (v.length !== 2) {
      errors.push(new L.ScamperError('Parser', 'Expected a vector of size 2 for a binder', undefined, range))
      return { name: '<error>', value: phExp }
    }
    const name = parseIdentifier(errors, v[0], 'Binding pair expects an identifier in the first position')
    const value = parseExp(errors, v[1])
    return { name, value }
  }
}

function parseLetBinders (errors: L.ScamperError[], v: L.Value): { name: string, value: A.Exp }[] {
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isList(v)) {
    errors.push(new L.ScamperError('Parser', 'Expected a list of binding pairs', undefined, range))
    return [{ name: '<error>', value: phExp }]
  } else {
    const arr = L.listToVector(v)
    return arr.map((v) => parseLetBinder(errors, v))
  }
}

///// Main entry points ////////////////////////////////////////////////////////

export function parseSingle (errors: L.ScamperError[], v: L.Value, range: L.Range): A.Exp {
  // N.B., only need to check for syntactic correctness of identifier
  // expressions. Other atomic expressions are checked at parsing time.
  if (L.isSym(v)) {
    const name = parseIdentifier(errors, v, 'Expected an identifier')
    return A.mkVar(name, range)
  } else {
    // This covers numbers, booleans, strings, null, undefined, and tagged objects (e.g., chars)
    return A.mkLit(v, range)
  }
}

export function parseExp (errors: L.ScamperError[], v: L.Value): A.Exp {
  let { value, range } = S.unpackSyntax(v)
  v = value

  if (!L.isList(v)) {
    return parseSingle(errors, v, range)
  } else {
    const arr = L.listToVector(v)
    if (arr.length == 0) {
      return A.mkLit(null, range)
    } else {
      let { value: hv, range: hr } = S.unpackSyntax(arr[0])
      if (L.isSym(hv)) {
        const head = hv.value
        switch (head) {
          case 'lambda': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Lambda expressions must have 2 sub-components, a list of identifiers and a body', undefined, hr))
              return phExp
            }
            const params = parseIdentifierList(errors, arr[1],
              'The first component of a lambda expression must be a list of identifiers',
              'The parameters of a lambda expression must be identifiers')
            const body = parseExp(errors, arr[2])
            return A.mkLam(params, body, range)
          }

          case 'let': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Let expressions must have 2 sub-components, a list of binding pairs and a body', undefined, hr))
              return phExp
            }
            const binders = parseLetBinders(errors, arr[1])
            const body = parseExp(errors, arr[2])
            return A.mkLet(binders, body, range)
          }

          case 'let*': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Let* expressions must have 2 sub-components, a list of binding pairs and a body', undefined, hr))
              return phExp
            }
            const binders = parseLetBinders(errors, arr[1])
            const body = parseExp(errors, arr[2])
            return A.mkLetS(binders, body, range)
          }

          case 'and': {
            return A.mkAnd(arr.slice(1).map((v) => parseExp(errors, v)), range)
          }

          case 'or': {
            return A.mkOr(arr.slice(1).map((v) => parseExp(errors, v)), range)
          }

          case 'if': {
            if (arr.length !== 4) {
              errors.push(new L.ScamperError('Parser', 'If expressions must have 3 sub-components: a guard, an if-branch, and an else-branch', undefined, hr))
              return phExp
            }
            const guard = parseExp(errors, arr[1])
            const ifBranch = parseExp(errors, arr[2])
            const elseBranch = parseExp(errors, arr[3])
            return A.mkIf(guard, ifBranch, elseBranch, range)
          }

          case 'begin': {
            if (arr.length === 1) {
              errors.push(new L.ScamperError('Parser', 'Begin expressions must have at least one sub-component', undefined, hr))
              return phExp
            }
            return A.mkBegin(arr.slice(1).map((v) => parseExp(errors, v)), range)
          }

          case 'match': {
            if (arr.length < 2) {
              errors.push(new L.ScamperError('Parser', 'Match expressions must have at least 1 sub-component: a scrutine', undefined, hr))
              return phExp
            }
            const scrutinee = parseExp(errors, arr[1])
            const branches = arr.slice(2).map((v) => parseBranch(errors, v))
            return A.mkMatch(scrutinee, branches, range)
          }

          case 'cond': {
            return A.mkCond(arr.slice(1).map((v) => parseCondBranch(errors, v)), range)
          }

          case 'quote': {
            if (arr.length === 1) {
              errors.push(new L.ScamperError('Parser', 'Quote expressions must have at least one sub-component', undefined, hr))
              return phExp
            } else {
              // N.B., We _don't_ check the syntax of the quoted expression!
              // We treat the syntax as-is, even if it is malformed. It is only
              // when we evaluate the syntax would we then check its validity.
              return A.mkQuote(S.stripSyntax(arr[1]), range)
            }
          }

          case 'section': {
            if (arr.length === 1) {
              errors.push(new L.ScamperError('Parser', 'Section expressions must have at least one sub-component', undefined, hr))
              return phExp
            }
            return A.mkSection(arr.slice(1).map((v) => parseExp(errors, v)), range)
          }

          default: {
            // N.B., otherwise, we have a function application
            const fn = parseExp(errors, arr[0])
            const args = arr.slice(1).map((v) => parseExp(errors, v))
            return A.mkApp(fn, args, range)
          }
        }
      } else {
        // N.B., otherwise, we have a function application
        const fn = parseExp(errors, arr[0])
        const args = arr.slice(1).map((v) => parseExp(errors, v))
        return A.mkApp(fn, args, range)
      }
    }
  }
}

export function parseStmt (errors: L.ScamperError[], v: L.Value): A.Stmt {
  const orig = v
  let { value, range } = S.unpackSyntax(v)
  v = value
  if (!L.isList(v)) {
    return A.mkStmtExp(parseExp(errors, orig), range)
  } else {
    const arr = L.listToVector(v as L.List)
    if (arr.length == 0) {
      return A.mkStmtExp(A.mkLit(null), range)
    } else {
      let { value: hv, range: hr } = S.unpackSyntax(arr[0])
      if (L.isSym(hv)) {
        const head = hv.value
        switch (head) {
          case 'import': {
            if (arr.length !== 2) {
              errors.push(new L.ScamperError('Parser', 'Import statements must have 2 sub-components, a module name and an alias', undefined, hr))
              return phStmt
            }
            const name = parseIdentifier(errors, arr[1], 'The first component of an import statement must be an identifier')
            return A.mkImport(name, range)
          }

          case 'define': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, hr))
              return phStmt
            }
            const name = parseIdentifier(errors, arr[1], 'The first component of a define statement must be an identifier')
            const body = parseExp(errors, arr[2])
            return A.mkDefine(name, body, range)
          }

          case 'display': {
            if (arr.length !== 2) {
              errors.push(new L.ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, hr))
              return phStmt
            }
            const body = parseExp(errors, arr[1])
            return A.mkDisp(body, range)
          }

          case 'struct': {
            if (arr.length !== 3) {
              errors.push(new L.ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, hr))
              return phStmt
            }
            const name = parseIdentifier(errors, arr[1], 'The first component of a struct statement must be an identifier')
            const fields = parseIdentifierList(errors, arr[2],
              'The second component of a struct statement must be a list of identifiers',
              'The fields of a struct must be identifiers')
            return A.mkStruct(name, fields, range)
          }

          default: {
            // N.B., in this case, the identifier at the front is assumed to
            // be a variable, so parse this like a statement-expression
            return A.mkStmtExp(parseExp(errors, orig), range)
          }
        }
      } else {
        // N.B., otherwise, we have a statement-expression
        return A.mkStmtExp(parseExp(errors, orig), range)
      }
    }
  }
}

export function parseProgram (errors: L.ScamperError[], values: L.Value[]): A.Prog {
  return values.map((v, _) => parseStmt(errors, v))
}
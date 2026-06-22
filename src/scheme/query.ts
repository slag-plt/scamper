import {
  ICE,
  isList,
  isSym,
  listToVector,
  Loc,
  mkList,
  mkSym,
  ScamperError,
} from "../lpm"
import { isSyntax, mkSyntax, Syntax } from "./syntax"
import { reservedWords } from "./parser"

/**
 * @returns [ast] with the node at [queryLoc] wrapped in a Report expression
 * @throws ScamperError if there is no valid node to report at [queryLoc]
 */
export function getQueriedAST(ast: Syntax[], queryLoc: Loc): Syntax[] {
  const queriedI = ast.findIndex((sexp) => sexp.range.contains(queryLoc))
  if (queriedI < 0) {
    throw new ScamperError(
      "Parser",
      `Received invalid query location: ${queryLoc.toString()}`,
    )
  }
  return ast.map((sexp, i) =>
    i === queriedI ? getReportedSyntax(sexp, queryLoc) : sexp,
  )
}

/**
 * Precondition: sexp.range contains queryLoc
 */
export function getReportedSyntax(sexp: Syntax, queryLoc: Loc): Syntax {
  const { value, range, comments } = sexp
  if (!isList(value)) {
    return makeWrappedSyntax(value, range)
  }

  // otherwise, we queried something inside a function application
  // find the exact value that was queried
  const elems = listToVector(value)

  // we broke because we either queried the head or we encountered a null
  if (elems.length === 0) {
    return makeWrappedSyntax(value, range)
  }

  let queriedI = -1
  for (let i = 0; i < elems.length; i++) {
    const head = elems[i]
    if (!isSyntax(head)) {
      throw new ICE(
        "getReportedSyntax",
        "Encountered a non-syntax value while traversing the AST",
      )
    }
    if (head.range.contains(queryLoc)) {
      // we found it, break
      queriedI = i
      break
    }
    // otherwise keep looking
  }

  if (queriedI < 0) {
    // we hit the end of the list without finding something to query,
    // but we still know that this list was queried.
    // thus, we must have queried the ending bracket of the list.
    // so, query the original syntax
    // TODO: the edge case is if someone was working with a cons-like list, and
    //  for some odd reason decided to query the final null in the cons. then, our
    //  assumption that they queried the ending bracket doesn't hold.
    //  let's consider that later.
    return makeWrappedSyntax(value, range)
  }

  // note: if queriedI >= 0, we know it must be a syntax node
  // (since our traversal should have caught it otherwise)
  const head = elems[queriedI] as Syntax
  if (queriedI !== 0) {
    // this was not the first thing in the application,
    // so we're free to just go deeper
    return mkSyntax(
      mkList(
        ...elems.map((elem, i) =>
          i === queriedI ? getReportedSyntax(head, queryLoc) : elem,
        ),
      ),
      range,
      comments,
    )
  }

  // we queried the head of the function application.
  const { value: hValue, range: hRange } = head
  if (isSym(hValue) && reservedWords.includes(hValue.value)) {
    // we can't report deeper into this reserved function application
    // so we'll just attempt to report the resulting value from this instead
    // so wrap the overall expression and return
    return makeWrappedSyntax(value, range)
  }

  let replacement: Syntax
  if (isSym(hValue) || !isList(hValue) || hValue === null) {
    // application of a defined function OR attempted application of a non-function value
    // just wrap the head in a report
    replacement = makeWrappedSyntax(hValue, hRange)
  } else {
    // this is an anonymous function, we can go deeper
    replacement = getReportedSyntax(head, queryLoc)
  }
  // return our initial sexp which owned this list
  return mkSyntax(
    mkList(...elems.map((elem, i) => (i === 0 ? replacement : elem))),
    range,
    comments,
  )
}

function makeWrappedSyntax(
  value: Syntax["value"],
  range: Syntax["range"],
): Syntax {
  return mkSyntax(mkList(mkSym("report"), value), range)
}

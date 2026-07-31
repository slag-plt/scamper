import { Range } from '../../lpm'

/**
 * Internal control-flow exception for the docstring parser's backtracking
 * (see param.ts's catchIf usage). It never escapes the docstring subsystem:
 * parseFunctionDocFromComments catches it at the boundary and yields a
 * ScamperDiagnostic. Deliberately NOT a ScamperError -- the static front-end
 * does not construct runtime errors.
 */
export class DocstringError extends Error {
  constructor(
    message: string,
    public range?: Range,
  ) {
    super(message)
  }
}

/** @returns a DocstringError carrying the given message and source range */
export function mkDocError(message: string, range?: Range): DocstringError {
  return new DocstringError(message, range)
}

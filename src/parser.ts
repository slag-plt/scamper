import { ICE, ScamperError } from './lang.js'
import { Bracket, Range, mkRange, Sexp, Exp, Stmt, Pat, Prog, Value } from './lang.js'

class Token {
  text: string
  range: Range

  constructor (text: string, range: Range) {
    this.text = text
    this.range = range
  }

  public toString (): string {
    return `["${this.text}" ${this.range.toString()}]`
  }
}

const isWhitespace = (c: string): boolean => /\s/.test(c)
const isOpeningBracket = (ch: string): boolean =>
  ['(', '#(', '{', '['].includes(ch)
const isClosingBracket = (ch: string): boolean =>
  [')', '}', ']'].includes(ch)
const isBracket = (ch: string): boolean =>
  isOpeningBracket(ch) || isClosingBracket(ch)
const areMatchingBrackets = (open: string, close: string): boolean => {
  return (open === '(' && close === ')') ||
         (open === '#(' && close === ')') ||
         (open === '[' && close === ']') ||
         (open === '{' && close === '}')
}

class Tokenizer {
  private src: string
  private idx: number
  private row: number
  private col: number

  private startIdx: number
  private startRow: number
  private startCol: number
  private endRow: number
  private endCol: number
  private endIdx: number
  private tokenLen: number

  constructor (src: string) {
    this.src = src
    this.idx = 0
    this.row = 1
    this.col = 1

    // N.B., can't call resetTracking because Typescript can't see through
    // the function call to see that resetTracking initializes values.
    this.startIdx = -1
    this.startRow = -1
    this.startCol = -1
    this.endRow = -1
    this.endCol = -1
    this.endIdx = -1
    this.tokenLen = 0

    // N.B., chomp whitespace so we maintain the invariant that the
    // tokenizer is always pointing to a valid token if there are tokens
    // left to process
    this.chompWhitespaceAndComments()
  }

  isEmpty (): boolean { return this.idx >= this.src.length }
  peek () : string { return this.src[this.idx] }
  isTracking (): boolean { return this.startIdx !== -1 }

  resetTracking (): void {
    this.startIdx = -1
    this.startRow = -1
    this.startCol = -1
    this.endRow = -1
    this.endCol = -1
    this.endIdx = -1
    this.tokenLen = 0
  }

  beginTracking (): void {
    if (this.isTracking()) {
      throw new ICE('parser.beginTracking', 'Already tracking a token')
    } else {
      this.startIdx = this.idx
      this.startRow = this.row
      this.startCol = this.col
      this.endRow   = this.row
      this.endCol   = this.col
      this.endIdx   = this.idx
      // N.B., the first call to advance() will capture the first character
      // by incrementing tokenLen
    }
  }

  emitToken (): Token {
    if (!this.isTracking()) {
      throw new ICE('parser.emitToken', 'Not tracking a token')
    } else {
      const token = new Token(
        this.src.slice(this.startIdx, this.startIdx + this.tokenLen),
        new Range(this.startRow, this.startCol, this.startIdx, this.endRow, this.endCol, this.endIdx)
      )
      this.resetTracking()
      // N.B., also chomp whitespace here to ensure that the tokenizer is
      // always pointing to a valid token if there are any left
      this.chompWhitespaceAndComments()
      return token
    }
  }

  advance (): void {
    if (this.isTracking()) {
      this.tokenLen += 1
      this.endRow = this.row
      this.endCol = this.col
      this.endIdx = this.idx
    }
    if (this.peek() === '\n') {
      this.row += 1
      this.col = 1
    } else {
      this.col += 1
    }
    this.idx += 1
  }

  chompWhitespaceAndComments(): void {
    let inComment = false
    while (!this.isEmpty() && (inComment || isWhitespace(this.peek()) || this.peek() === ';')) {
      if (this.peek() === ';') {
        inComment = true
      } else if (inComment && this.peek() === '\n') {
        inComment = false
      }
      this.advance()
    }
  }

  // TODO: need to handle comments and whether they appear in the token stream
  next (): Token {
    let ch = this.peek()
    // Case: brackets
    if (isBracket(ch)) {
      this.beginTracking()
      this.advance()
      return this.emitToken()
    // Case: string literals
    } else if (ch === '"') {
      this.beginTracking() 
      this.advance()
      while (!this.isEmpty()) {
        ch = this.peek()
        if (this.peek() === '"') {
          this.advance()
          return this.emitToken()
        // N.B., since any escape sequence that does not have a meaning is
        // the identity escape sequence, we can simply advance past the
        // the entire sequence and let Javascript handle interpreting the
        // sequence for us!
        } else if (this.peek() === '\\') {
          this.advance()  // advance past '\\
          this.advance()  // advance past the escaped character
        } else {
          this.advance()
        }
      }
      // NOTE: error is localized to the open quote to, presumably, the end of the
      // file. Depending on error reporting, it may make sense to report only the
      // starting quote or try to approx. where the string should end.
      throw new ScamperError('Parser', 'Unterminated string literal.',
        undefined, new Range(this.startRow, this.startCol, this.startIdx, this.endRow, this.endCol, this.endIdx))
    // Case: any other sequence of non-whitespace, non-delimiting characters
    } else {
      this.beginTracking()
      this.advance()
      while (!this.isEmpty()) {
        ch = this.peek()
        if (isWhitespace(ch) || isBracket(ch) || ch === ';') {
          // N.B., don't include the terminating char in this token!
          return this.emitToken()
        } else {
          this.advance()
        }
      }
      // N.B., should only get here if a complete token ends the file
      return this.emitToken()
    }
  }
}

export function stringToTokens (src: string): Token[] {
  const tokenizer = new Tokenizer(src)
  const tokens: Token[] = []
  while (!tokenizer.isEmpty()) {
    tokens.push(tokenizer.next())
  }
  return tokens 
}

function puffRange(r: Range): Range {
  return new Range(
    r.begin.line,
    r.begin.col === 1 ? r.begin.col : r.begin.col - 1,
    r.begin.col === 1 ? r.begin.idx : r.begin.idx - 1,
    r.end.line,
    r.end.col,
    r.end.idx + 1
  )
}

export function tokensToSexp (tokens: Token[]): Sexp.T {
  const beg = tokens.shift()!
  if (isOpeningBracket(beg.text)) {
    const exps = []
    while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
      exps.push(tokensToSexp(tokens))
    }
    if (tokens.length === 0) {
      // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
      throw new ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, puffRange(beg.range))
    } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
      throw new ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
        undefined, mkRange(beg.range.begin, tokens[0].range.end))
    } else {
      const end = tokens.shift()!
      return Sexp.mkList(exps, beg.text as Bracket, mkRange(beg.range.begin, end.range.end))
    }
  } else {
    return Sexp.mkAtom(beg.text, beg.range)
  }
}

export function tokensToSexps (tokens: Token[]): Sexp.T[] {
  const ret = []
  while (tokens.length > 0) {
    ret.push(tokensToSexp(tokens))
  }
  return ret
}

const intRegex = /^[+-]?\d+$/
const floatRegex = /^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/

export const reservedWords = [
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
  'section',
  'struct',
]

function parseStringLiteral (src: string, range: Range): string {
  if (src.length === 0) {
    throw new ICE('parseStringLiteral', 'Empty string literal (with no quote!)')
  } else if (src[0] !== '"') {
    throw new ScamperError('Parser', 'String literal must begin with a quote', undefined, range)
  }

  let ret = ''
  for (let i = 1; i < src.length; i++) {
    // A quote closes this string literal
    if (src[i] === '"') {
      return ret
    // Escape characters require us to consume the next character
    } else if (src[i] === '\\') {
      if (i+1 >= src.length) {
        throw new ScamperError('Parser', 'Escape character "\\" cannot occur at the end of a string.', undefined, range)
      }
      const ch = src[i + 1]
      switch (ch) {
        // Alarm: ASCII 7
        case 'a': ret += '\u0007'; break
        // Backspace: ASCII 8
        case 'b': ret += '\u0008'; break
        // Tab: ASCII 9
        case 't': ret += '\u0009'; break
        // Linefeed: ASCII 10
        case 'n': ret += '\u000A'; break
        // Vertical tab: ASCII 11
        case 'v': ret += '\u000B'; break
        // Form feed: ASCII 12
        case 'f': ret += '\u000C'; break
        // Carriage return: ASCII 13
        case 'r': ret += '\u000D'; break
        // Escape: ASCII 27
        case 'e': ret += '\u001B'; break
        case '"': ret += '"'; break
        case "'": ret += "'"; break
        case '\\': ret += '\\'; break
        default:
          // NOTE: Extended escape codes are currently not supported
          if (ch >= '0' && ch <= '9') {
            throw new ScamperError('Parser', 'Octal escape codes not supported', undefined, range)
          } else if (ch === 'x') {
            throw new ScamperError('Parser', 'Hex escape codes not supported', undefined, range)
          } else if (ch === 'u' || ch === 'U') {
            throw new ScamperError('Parser', 'Unicode escape codes not supported', undefined, range)
          } else if (ch === '\n') {
            // Skip over newline characters but continue processing the literal
          }
      }
      // NOTE: skip the extra \ that we parsed in this case. If/when we support
      // extended escape codes, the size of the jump will obviously grow!
      i += 1
    // Any other character is simply appended onto the result.
    } else {
      ret += src[i]
    }
  }
  return ret
}

export function atomToValue (e: Sexp.Atom, wildAllowed: boolean): Value.Syntax {
  const text = e.value
  if (intRegex.test(text)) {
    return Value.mkSyntax(e.range, parseInt(text))
  } else if (floatRegex.test(e.value)) {
    return Value.mkSyntax(e.range, parseFloat(e.value))
  } else if (e.value === '#t') {
    return Value.mkSyntax(e.range, true)
  } else if (e.value === '#f') {
    return Value.mkSyntax(e.range, false)
  } else if (e.value === 'null') {
    return Value.mkSyntax(e.range, null)
  } else if (e.value.startsWith('"')) {
    return Value.mkSyntax(e.range, parseStringLiteral(e.value, e.range))
  } else if (e.value.startsWith('#\\')) {
    const escapedChar = e.value.slice(2)
    if (escapedChar.length === 1) {
      return Value.mkSyntax(e.range, Value.mkChar(escapedChar))
    } else if (namedCharValues.has(escapedChar)) {
      return Value.mkSyntax(e.range, Value.mkChar(namedCharValues.get(escapedChar)!))
    } else {
      throw new ScamperError('Parser', `Invalid character literal: ${e.value}`, undefined, e.range)
    }
  } else {
    // TODO: ensure identifiers don't have invalid characters, i.e., #
    // Probably should be done in the lexer, not the parser...
    if (e.value.startsWith('_') && !wildAllowed) {
      throw new ScamperError('Parser', 'Identifiers cannot begin with "_" unless inside of "section" or patterns', undefined, e.range)
    }
    return Value.mkSyntax(e.range, Value.mkSym(e.value))
  }
}

// export function atomToExp (e: Sexp.Atom, inSection: boolean): Exp.T {
//   const text = e.value
//   if (intRegex.test(text)) {
//     return Exp.mkVal(parseInt(text), e.range)
//   } else if (floatRegex.test(e.value)) {
//     return Exp.mkVal(parseFloat(e.value), e.range)
//   } else if (e.value === '#t') {
//     return Exp.mkVal(true, e.range)
//   } else if (e.value === '#f') {
//     return Exp.mkVal(false, e.range)
//   } else if (e.value.startsWith('"')) {
//     return Exp.mkVal(parseStringLiteral(e.value, e.range), e.range)
//   } else if (reservedWords.includes(e.value)) {
//     throw new ScamperError('Parser', `Cannot use reserved word as identifier name: ${e.value}`, undefined, e.range)
//   } else if (e.value.startsWith('#\\')) {
//     const escapedChar = e.value.slice(2)
//     if (escapedChar.length === 1) {
//       return Exp.mkVal(Value.mkChar(escapedChar), e.range)
//     } else if (namedCharValues.has(escapedChar)) {
//       return Exp.mkVal(Value.mkChar(namedCharValues.get(escapedChar)!), e.range)
//     } else {
//       throw new ScamperError('Parser', `Invalid character literal: ${e.value}`, undefined, e.range)
//     }
//   } else {
//     // TODO: ensure identifiers don't have invalid characters, i.e., #
//     // Probably should be done in the lexer, not the parser...
//     if (e.value.startsWith('_') && !inSection) {
//       throw new ScamperError('Parser', 'Identifiers cannot begin with "_" unless inside of "section"', undefined, e.range)
//     }
//     return Exp.mkVar(e.value, e.range)
//   }
// }

export function sexpToBinding (e: Sexp.T): Value.Syntax {
  if (e.kind === 'atom' || e.value.length !== 2 || e.value[0].kind !== 'atom') {
    throw new ScamperError('Parser', `Bindings must given as pairs of names and values: ${Sexp.sexpToString(e)}`, undefined, e.range)
  } else {
    return Value.mkSyntax(e.range, [e.value[0].value, sexpToExp(e.value[1], false)])
  }
}

export function sexpToMatchBranch (e: Sexp.T): Value.Syntax {
  if (e.kind === 'atom' || e.value.length !== 2) {
    throw new ScamperError('Parser', `Match branches must be given a pair of a pattern and an expression: ${Sexp.sexpToString(e)}`, undefined, e.range)
  } else {
    return Value.mkSyntax(e.range, [sexpToPat(e.value[0]), sexpToExp(e.value[1])])
  }
}

export function sexpToCondBranch (e: Sexp.T): Value.Syntax {
  if (e.kind === 'atom' || e.value.length !== 2) {
    throw new ScamperError('Parser', `Cond branches must be given a pair expressions: ${Sexp.sexpToString(e)}`, undefined, e.range)
  } else {
    return Value.mkSyntax(e.range, [sexpToExp(e.value[0]), sexpToExp(e.value[1])])
  }
}

export const namedCharValues = new Map([
  ['alarm', String.fromCharCode(7)],
  ['backspace', String.fromCharCode(8)],
  ['delete', String.fromCharCode(127)],
  ['escape', String.fromCharCode(27)],
  ['newline', String.fromCharCode(10)],
  ['null', String.fromCharCode(0)],
  ['return', String.fromCharCode(13)],
  ['space', ' '],
  ['tab', String.fromCharCode(9)]
])

// export function atomToPat (e: Sexp.Atom): SyntaxError.{
//   const text = e.value
//   if (intRegex.test(text)) {
//     return Pat.mkNum(parseInt(text), e.range)
//   } else if (floatRegex.test(e.value)) {
//     return Pat.mkNum(parseFloat(e.value), e.range)
//   } else if (e.value === '#t') {
//     return Pat.mkBool(true, e.range)
//   } else if (e.value === '#f') {
//     return Pat.mkBool(false, e.range)
//   } else if (e.value.startsWith('"')) {
//     return Pat.mkStr(e.value.slice(1, -1), e.range)
//   } else if (e.value === '_') {
//     return Pat.mkWild(e.range)
//   } else if (e.value === 'null') {
//     return Pat.mkNull(e.range)
//   } else if (reservedWords.includes(e.value)) {
//     throw new ScamperError('Parser', `Cannot use reserved word as identifier name: ${e.value}`, undefined, e.range)
//   } else if (e.value.startsWith('#\\')) {
//     const escapedChar = e.value.slice(2)
//     if (escapedChar.length === 1) {
//       return Pat.mkChar(escapedChar, e.range)
//     } else if (namedCharValues.has(escapedChar)) {
//       return Pat.mkChar(namedCharValues.get(escapedChar)!, e.range)
//     } else {
//       throw new ScamperError('Parser', `Invalid character literal: ${e.value}`, undefined, e.range)
//     }
//   } else {
//     return Pat.mkVar(e.value, e.range)
//   }
// }

export function sexpToPat (e: Sexp.T): Value.Syntax {
  switch (e.kind) {
    case 'atom':
      return atomToValue(e, true)
    case 'list': {
      if (e.value.length === 0) {
        throw new ScamperError('Parser', 'The empty list is not a valid pattern', undefined, e.range)
      }
      const head = e.value[0]
      if (head.kind !== 'atom') {
        throw new ScamperError('Parser', 'Constructor patterns must start with an identifier', undefined, head.range)
      }
      const args = e.value.slice(1)
      return Value.mkSyntax(e.range, Value.mkList(Value.mkSym(head.value), ...args.map(sexpToPat)))
    }
  }
}

export function holesToVars (e: Sexp.T): Sexp.T {
  let counter = 1
  let holesAreNamed: boolean | undefined = undefined
  function rec (e: Sexp.T): Sexp.T {
    switch (e.kind) {
      case 'atom':
        if (e.value === '_') {
          if (holesAreNamed) {
            throw new ScamperError('Parser', 'Cannot mixed named and anonymous holes in a section or anonymous function', undefined, e.range)
          } else {
            holesAreNamed = false
            return Sexp.mkAtom(`_${counter++}`, e.range)
          }
        } else if (e.value.startsWith('_')) {
          if (holesAreNamed === false) {
            throw new ScamperError('Parser', 'Cannot mixed named and anonymous holes in a section or anonymous function', undefined, e.range)
          } else {
            holesAreNamed = true
            return Sexp.mkAtom(e.value, e.range)
          }
        } else {
          return e
        }
      case 'list':
        return Sexp.mkList(e.value.map(rec), e.bracket, e.range)
    }
  }
  return rec(e)
}

export function getNamedHoles (e: Sexp.T): string[] {
  const vars: Set<string> = new Set()
  function rec (e: Sexp.T): void {
    switch (e.kind) {
      case 'atom':
        if (e.value.startsWith('_')) {
          vars.add(e.value)
        }
        break
      case 'list':
        e.value.forEach(rec)
        break
    }
  }
  rec(e)
  const ret = []
  for (const v of vars.values()) {
    ret.push(v)
  }
  return ret
}

export function sexpToExp (e: Sexp.T, inSection: boolean = false): Value.Syntax {
  switch (e.kind) {
    case 'atom':
      return atomToValue(e, inSection)
    case 'list': {
      if (e.value.length === 0) {
        return Value.mkSyntax(e.range, Value.mkList())
      }
      const head = e.value[0]
      const args = e.value.slice(1)
      if (head.kind === 'atom' && head.value === 'lambda') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Lambda expression must have 2 sub-components, an parameter list and a body', undefined, e.range)
        }
        const es = args[0]
        if (es.kind !== 'list') {
         throw new ScamperError('Parser', 'The first component of a lambda expression must be a parameter list', undefined, es.range)
        }
        const params: string[] = []
        es.value.forEach(arg => {
          if (arg.kind !== 'atom') {
            throw new ScamperError('Parser', 'Parameters must only be identifiers but a list was given instead', undefined, arg.range)
          }
          params.push(arg.value)
        })
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('lambda'),
          Value.mkList(...params.map(Value.mkSym)),
          sexpToExp(args[1])
        ))
      } else if (head.kind === 'atom' && head.value === 'let') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, e.range)
        }
        const binds = args[0]
        if (binds.kind !== 'list') {
          throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, binds.range)
        }
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('let'),
          Value.mkList(...binds.value.map(sexpToBinding)),
          sexpToExp(args[1])
        ))
      } else if (head.kind === 'atom' && head.value === 'let*') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Let* expression must have 2 sub-components, a binding list and a body', undefined, e.range)
        }
        const binds = args[0]
        if (binds.kind !== 'list') {
          throw new ScamperError('Parser', 'Let* expression bindings must be given as a list', undefined, binds.range)
        }
        if (binds.value.length === 0) {
          return Value.mkSyntax(e.range, Value.mkList(sexpToExp(args[1])))
        } else {
          let ret = Value.mkSyntax(e.range, Value.mkList(
              Value.mkSym('let'),
              Value.mkList(sexpToBinding(binds.value[binds.value.length - 1])),
              sexpToExp(args[1])
          ))
          for (let i = binds.value.length - 2; i >= 0; i--) {
            ret = Value.mkSyntax(e.range, Value.mkList(
              Value.mkSym('let'),
              [sexpToBinding(binds.value[i])],
              ret
            ))
          }
          return ret
        }
      } else if (head.kind === 'atom' && head.value === 'and') {
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('and'),
          ...args.map((e) => sexpToExp(e, inSection))
        ))
      } else if (head.kind === 'atom' && head.value === 'or') {
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('or'),
          ...args.map((e) => sexpToExp(e, inSection))
        ))
      } else if (head.kind === 'atom' && head.value === 'if') {
        if (args.length !== 3) {
          throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, e.range)
        } else {
          return Value.mkSyntax(e.range, Value.mkList(
            Value.mkSym('if'),
            sexpToExp(args[0], inSection),
            sexpToExp(args[1], inSection),
            sexpToExp(args[2], inSection)
          ))
        }
      } else if (head.kind === 'atom' && head.value === 'begin') {
        if (args.length === 0) {
          throw new ScamperError('Parser', 'Begin expression must have at least 1 sub-expression', undefined, e.range)
        } else {
          return Value.mkSyntax(e.range,
            Value.mkList(Value.mkSym('begin'), ...args.map((e) => sexpToExp(e, inSection))))
        }
      } else if (head.kind === 'atom' && head.value === 'match') {
        if (args.length < 2) {
          throw new ScamperError('Parser', 'Match expression must have at least two sub-expressions, a scrutinee at least one branch', undefined, e.range)
        }
        const scrutinee = args[0]
        const branches = args.slice(1)
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('match'),
          sexpToExp(scrutinee, inSection),
          ...branches.map(sexpToMatchBranch)
        ))
      } else if (head.kind === 'atom' && head.value === 'cond') {
        if (args.length < 1) {
          throw new ScamperError('Parser', 'Cond expression must have at least one branch', undefined, e.range)
        }
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('cond'),
          ...args.map(sexpToCondBranch)
        ))
      } else if (head.kind === 'atom' && head.value === 'section') {
        if (args.length < 1) {
          throw new ScamperError('Parser', 'Section expression must have at least one sub-expression', undefined, e.range)
        }
        const body = holesToVars(Sexp.mkList(args, e.bracket, e.range))
        const params = getNamedHoles(body)
        // N.B., place the params in numeric/alphabetical order
        params.sort()
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('lambda'),
          Value.mkList(...params.map(Value.mkSym)),
          sexpToExp(body, true)
        ))
      } else {
        return Value.mkSyntax(e.range, Value.mkList(
          sexpToExp(head),
          ...args.map((s) => sexpToExp(s, inSection))
        ))
      }
    }
  }
}

export function sexpToStmt (e: Sexp.T): Value.Syntax {
  switch (e.kind) {
    case 'atom':
      return Value.mkSyntax(e.range, sexpToExp(e))
    case 'list': {
      if (e.value.length === 0) {
        throw new ScamperError('Parser', 'The empty list is not a valid statement', undefined, e.range)
      }
      const head = e.value[0]
      const args = e.value.slice(1)
      if (head.value === 'define') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, e.range)
        }
        const name = args[0]
        if (name.kind !== 'atom') {
          throw new ScamperError('Parser', 'The first component of a define statement must be an identifier', undefined, name.range)
        }
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('define'),
          Value.mkSym(name.value),
          sexpToExp(args[1])
        ))
      } else if (head.value === 'import') {
        if (args.length !== 1) {
          throw new ScamperError('Parser', 'Import statements must have 1 argument, the name of a module', undefined, e.range)
        }
        const name = args[0]
        if (name.kind !== 'atom') {
          throw new ScamperError('Parser', 'The argument of an import statement must be a module name', undefined, args[0].range)
        }
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('import'),
          Value.mkSym(name.value)
        ))
      } else if (head.value === 'display') {
        if (args.length !== 1) {
          throw new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, e.range)
        }
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('display'),
          sexpToExp(args[0])
        ))
      } else if (head.value === 'struct') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, e.range)
        } 
        const name = args[0]
        if (name.kind !== 'atom') {
          throw new ScamperError('Parser', 'The first argument of a struct statement must be a struct name', undefined, name.range)
        }
        const sfields = args[1]
        if (sfields.kind !== 'list') {
          throw new ScamperError('Parser', 'The second argument of a struct statement must be a list of fields', undefined, args[1].range)
        }
        const fields: string[] = []
        sfields.value.forEach((f) => {
          if (f.kind !== 'atom') {
            throw new ScamperError('Parser', 'Struct fields must be identifiers', undefined, f.range)
          }
          fields.push(f.value)
        })
        if (fields.length === 0) {
          return Value.mkSyntax(e.range, Value.mkList(
            Value.mkSym('struct'),
            Value.mkSym(name.value)
          ))
        } else {
        return Value.mkSyntax(e.range, Value.mkList(
          Value.mkSym('struct'),
          Value.mkSym(name.value),
          Value.mkList(...fields.map(Value.mkSym))
        ))
        }
      } else {
        return sexpToExp(e)
      }
    }
  }
}

export function sexpsToProg (exps: Sexp.T[]): Prog {
  return exps.map(sexpToStmt)
}

export function parseProgram (src: string): Prog {
  const tokens = stringToTokens(src)
  const sexps = tokensToSexps(tokens)
  return sexpsToProg(sexps)
}
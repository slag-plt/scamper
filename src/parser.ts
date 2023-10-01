import { ICE, ScamperError } from './lang.js'
import { Range, mkRange, Prog, Stmt, Op, Value } from './lang.js'

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
    // Case: quotation
    } else if (ch === "'") {
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
        if (isWhitespace(ch) || isBracket(ch) || ch === ';' || ch === "'") {
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

export function tokensToValue (tokens: Token[]): Value.Syntax {
  const beg = tokens.shift()!
  if (isOpeningBracket(beg.text)) {
    const values = []
    while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
      values.push(tokensToValue(tokens))
    }
    if (tokens.length === 0) {
      // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
      throw new ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, puffRange(beg.range))
    } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
      throw new ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
        undefined, mkRange(beg.range.begin, tokens[0].range.end))
    } else {
      const end = tokens.shift()!
      return Value.mkSyntax(
        mkRange(beg.range.begin, end.range.end),
        // N.B., non '[' brackets are lists, i.e., '('. Will need to change if
        // we ever allow '{' to imply an dictionary/object.
        beg.text === '[' ? values : Value.mkList(...values))
    }
  } else if (beg.text === "'") {
    return Value.mkSyntax(beg.range, Value.mkList(Value.mkSym('quote'), tokensToValue(tokens)))
  } else {
    return tokenToValue(beg, true)
  }
}

export function tokensToValues (tokens: Token[]): Value.Syntax[] {
  const ret = []
  while (tokens.length > 0) {
    ret.push(tokensToValue(tokens))
  }
  return ret
}

// export function tokensToSexp (tokens: Token[]): Sexp.T {
//   const beg = tokens.shift()!
//   if (isOpeningBracket(beg.text)) {
//     const exps = []
//     while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
//       exps.push(tokensToSexp(tokens))
//     }
//     if (tokens.length === 0) {
//       // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
//       throw new ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, puffRange(beg.range))
//     } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
//       throw new ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
//         undefined, mkRange(beg.range.begin, tokens[0].range.end))
//     } else {
//       const end = tokens.shift()!
//       return Sexp.mkList(exps, beg.text as Bracket, mkRange(beg.range.begin, end.range.end))
//     }
//   } else {
//     return Sexp.mkAtom(beg.text, beg.range)
//   }
// }

// export function tokensToSexps (tokens: Token[]): Sexp.T[] {
//   const ret = []
//   while (tokens.length > 0) {
//     ret.push(tokensToSexp(tokens))
//   }
//   return ret
// }

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

export function tokenToValue (t: Token, wildAllowed: boolean): Value.Syntax {
  const text = t.text
  if (intRegex.test(text)) {
    return Value.mkSyntax(t.range, parseInt(text))
  } else if (floatRegex.test(text)) {
    return Value.mkSyntax(t.range, parseFloat(text))
  } else if (text === '#t') {
    return Value.mkSyntax(t.range, true)
  } else if (text === '#f') {
    return Value.mkSyntax(t.range, false)
  } else if (text === 'null') {
    return Value.mkSyntax(t.range, null)
  } else if (text.startsWith('"')) {
    return Value.mkSyntax(t.range, parseStringLiteral(text, t.range))
  } else if (text.startsWith('#\\')) {
    const escapedChar = text.slice(2)
    if (escapedChar.length === 1) {
      return Value.mkSyntax(t.range, Value.mkChar(escapedChar))
    } else if (namedCharValues.has(escapedChar)) {
      return Value.mkSyntax(t.range, Value.mkChar(namedCharValues.get(escapedChar)!))
    } else {
      throw new ScamperError('Parser', `Invalid character literal: ${text}`, undefined, t.range)
    }
  } else {
    // TODO: ensure identifiers don't have invalid characters, i.e., #
    // Probably should be done in the lexer, not the parser...
    if (text.startsWith('_') && !wildAllowed) {
      throw new ScamperError('Parser', 'Identifiers cannot begin with "_" unless inside of "section" or patterns', undefined, t.range)
    }
    return Value.mkSyntax(t.range, Value.mkSym(text))
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

// export function sexpToBinding (e: Sexp.T): Value.Syntax {
//   if (e.kind === 'atom' || e.value.length !== 2 || e.value[0].kind !== 'atom') {
//     throw new ScamperError('Parser', `Bindings must given as pairs of names and values: ${Sexp.sexpToString(e)}`, undefined, e.range)
//   } else {
//     return Value.mkSyntax(e.range, [e.value[0].value, sexpToExp(e.value[1], false)])
//   }
// }

function valueToBinding (v: Value.T): { name: string, ops: Op.T[] } {
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (!Value.isArray(v)) {
    throw new ScamperError('Parser', 'Binding pair must be given as a vector', undefined, range)
  }
  const vec = v as Value.Vector
  if (vec.length !== 2 || !Value.isSym(Value.stripSyntax(vec[0]))) {
    throw new ScamperError('Parser', `Binding must be a pair of a name and value`, undefined, Value.rangeOf(vec[0]))
  }
  return { name: (Value.stripSyntax(vec[0]) as Value.Sym).value
         , ops: valueToOps(vec[1]) }
}

function valueToMatchBranch (v: Value.T): Op.MatchBranch {
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (!Value.isArray(v)) {
    throw new ScamperError('Parser', 'Match branches must be given as a vector', undefined, range)
  }
  const vec = v as Value.Vector
  if (vec.length !== 2 || !Value.isSym(Value.stripSyntax(vec[0]))) {
    throw new ScamperError('Parser', 'Match branches must be given as a pair of a pattern and an expression', undefined, Value.rangeOf(vec[0]))
  }
  return { pattern: Value.stripSyntax(vec[0]), body: valueToOps(vec[1]) }
}

function valueToCondBranch (v: Value.T): { cond: Op.T[], body: Op.T[]} {
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (!Value.isArray(v)) {
    throw new ScamperError('Parser', 'Cond branch must be given as a vector', undefined, range)
  }
  const vec = v as Value.Vector
  if (vec.length !== 2) {
    throw new ScamperError('Parser', `Cond branch must be a pair of expressions`, undefined, range)
  }
  return { cond: valueToOps(vec[0]), body: valueToOps(vec[1]) }
}

// export function sexpToMatchBranch (e: Sexp.T): Value.Syntax {
//   if (e.kind === 'atom' || e.value.length !== 2) {
//     throw new ScamperError('Parser', `Match branches must be given a pair of a pattern and an expression: ${Sexp.sexpToString(e)}`, undefined, e.range)
//   } else {
//     return Value.mkSyntax(e.range, [sexpToPat(e.value[0]), sexpToExp(e.value[1])])
//   }
// }

// export function sexpToCondBranch (e: Sexp.T): Value.Syntax {
//   if (e.kind === 'atom' || e.value.length !== 2) {
//     throw new ScamperError('Parser', `Cond branches must be given a pair expressions: ${Sexp.sexpToString(e)}`, undefined, e.range)
//   } else {
//     return Value.mkSyntax(e.range, [sexpToExp(e.value[0]), sexpToExp(e.value[1])])
//   }
// }


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

// export function sexpToPat (e: Sexp.T): Value.Syntax {
//   switch (e.kind) {
//     case 'atom':
//       return atomToValue(e, true)
//     case 'list': {
//       if (e.value.length === 0) {
//         throw new ScamperError('Parser', 'The empty list is not a valid pattern', undefined, e.range)
//       }
//       const head = e.value[0]
//       if (head.kind !== 'atom') {
//         throw new ScamperError('Parser', 'Constructor patterns must start with an identifier', undefined, head.range)
//       }
//       const args = e.value.slice(1)
//       return Value.mkSyntax(e.range, Value.mkList(Value.mkSym(head.value), ...args.map(sexpToPat)))
//     }
//   }
// }

// export function holesToVars (e: Sexp.T): Sexp.T {
//   let counter = 1
//   let holesAreNamed: boolean | undefined = undefined
//   function rec (e: Sexp.T): Sexp.T {
//     switch (e.kind) {
//       case 'atom':
//         if (e.value === '_') {
//           if (holesAreNamed) {
//             throw new ScamperError('Parser', 'Cannot mixed named and anonymous holes in a section or anonymous function', undefined, e.range)
//           } else {
//             holesAreNamed = false
//             return Sexp.mkAtom(`_${counter++}`, e.range)
//           }
//         } else if (e.value.startsWith('_')) {
//           if (holesAreNamed === false) {
//             throw new ScamperError('Parser', 'Cannot mixed named and anonymous holes in a section or anonymous function', undefined, e.range)
//           } else {
//             holesAreNamed = true
//             return Sexp.mkAtom(e.value, e.range)
//           }
//         } else {
//           return e
//         }
//       case 'list':
//         return Sexp.mkList(e.value.map(rec), e.bracket, e.range)
//     }
//   }
//   return rec(e)
// }

// export function getNamedHoles (e: Sexp.T): string[] {
//   const vars: Set<string> = new Set()
//   function rec (e: Sexp.T): void {
//     switch (e.kind) {
//       case 'atom':
//         if (e.value.startsWith('_')) {
//           vars.add(e.value)
//         }
//         break
//       case 'list':
//         e.value.forEach(rec)
//         break
//     }
//   }
//   rec(e)
//   const ret = []
//   for (const v of vars.values()) {
//     ret.push(v)
//   }
//   return ret
// }

export function valueToOps (v: Value.T): Op.T[] {
  let { range, value } = Value.unpackSyntax(v)
  v = value

  if (Value.isSym(v)) {
    return [Op.mkVar((v as Value.Sym).value, range)]
  } else if (!Value.isList(v)) {
    return [Op.mkValue(v)]
  } else {
    const values = Value.listToVector(v as Value.List)
    if (values.length === 0) {
      return [Op.mkValue(null)]
    }
    const head = values[0]
    const args = values.slice(1)
    if (Value.isSymName(Value.stripSyntax(head), 'lambda')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Lambda expression must have 2 sub-components, an parameter list and a body', undefined, range)
      }
      const { range: esr, value: es } = Value.unpackSyntax(args[0])
      if (!Value.isList(es)) {
        throw new ScamperError('Parser', 'The first component of a lambda expression must be a parameter list', undefined, esr)
      }
      const params: string[] = []
      Value.listToVector(es as Value.List).forEach(arg => {
        let { range: r, value: x } = Value.unpackSyntax(arg)
        if (!Value.isSym(x)) {
          throw new ScamperError('Parser', 'Parameters must only be identifiers', undefined, r)
        }
        params.push((x as Value.Sym).value)
      })
      return [Op.mkCls(params, valueToOps(args[1]))]
    } else if (Value.isSymName(Value.stripSyntax(head), 'let')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
      }
      const { range: bsr, value: bs } = Value.unpackSyntax(args[0])
      if (!Value.isList(bs)) {
        throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
      }
      // TODO: problem will need to unwrap syntax for each individual binding
      const bindings = Value.listToVector(bs as Value.List).map(valueToBinding)
      const valOps = bindings.flatMap((b) => b.ops)
      return valOps.concat([Op.mkLet(bindings.map((b) => b.name), valueToOps(args[1]))])
    } else if (Value.isSymName(Value.stripSyntax(head), 'and')) {
      const label = Op.freshLabel()
      return args
        .flatMap((arg) => valueToOps(arg))
        .concat([Op.mkAnd(label, range)])
        .concat([Op.mkValue(true), Op.mkLbl(label)])
    } else if (Value.isSymName(Value.stripSyntax(head), 'or')) {
      const label = Op.freshLabel()
      return args
        .flatMap((arg) => valueToOps(arg))
        .concat([Op.mkOr(label, range)])
        .concat([Op.mkValue(false), Op.mkLbl(label)])
    } else if (Value.isSymName(Value.stripSyntax(head), 'if')) {
      if (args.length !== 3) {
        throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, range)
      } else {
        return valueToOps(args[0]).concat([
          Op.mkIf(valueToOps(args[1]), valueToOps(args[2]), range)
        ])
      }
    } else if (Value.isSymName(Value.stripSyntax(head), 'begin')) {
      if (args.length === 0) {
        throw new ScamperError('Parser', 'Begin expression must have at least 1 sub-expression', undefined, range)
      } else {
        return args.flatMap((arg) => valueToOps(arg)).concat([Op.mkSeq(args.length)])
      }
    } else if (Value.isSymName(Value.stripSyntax(head), 'match')) {
      if (args.length < 2) {
        throw new ScamperError('Parser', 'Match expression must have at least two sub-expressions, a scrutinee at least one branch', undefined, range)
      }
      const scrutinee = args[0]
      const branches = args.slice(1).map(valueToMatchBranch)
      return valueToOps(scrutinee).concat([Op.mkMatch(branches, range)])
    } else if (Value.isSymName(Value.stripSyntax(head), 'cond')) {
      if (args.length < 1) {
        throw new ScamperError('Parser', 'Cond expression must have at least one branch', undefined, range)
      }
      const label = Op.freshLabel()
      const branches = args.map(valueToCondBranch)
      return branches
        .flatMap((b) => b.cond.concat([Op.mkCond(b.body, label, range)]))
        .concat([
          Op.mkExn('No branches of "cond" expression matched', undefined, range),
          Op.mkLbl(label)
        ])
    } else if (Value.isSymName(Value.stripSyntax(head), 'quote')) {
      if (args.length !== 1) {
        throw new ScamperError('Parser', 'Quote expression must have exactly one sub-expression', undefined, range)
      }
      return [Op.mkValue(Value.stripAllSyntax(args[0]))]
    } else {
      return values.flatMap(valueToOps).concat([
        Op.mkAp(args.length, range)
      ])
    }
  }
}

export function valueToStmt (v: Value.T): Stmt.T {
  let { range, value } = Value.unpackSyntax(v)
  v = value

  if (!Value.isList(v)) {
    return Stmt.mkStmtExp(valueToOps(v), v, range)
  } else {
    const values = Value.listToVector(v as Value.List)
    if (values.length === 0) {
      return Stmt.mkStmtExp([Op.mkValue(null)], v, range)
    }

    const head = values[0]
    const args = values.slice(1)

    if (Value.isSymName(Value.stripSyntax(head), 'define')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Define statements must have 2 sub-components, an identifier and a body', undefined, range)
      }
      const { range: r, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
        throw new ScamperError('Parser', 'The first component of a define statement must be an identifier', undefined, r)
      }
      return Stmt.mkStmtBinding((name as Value.Sym).value, valueToOps(args[1]), v, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'import')) {
      if (args.length !== 1) {
        throw new ScamperError('Parser', 'Import statements must have 1 argument, the name of a module', undefined, range)
      }
      const { range: r, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
          throw new ScamperError('Parser', 'The argument of an import statement must be a module name', undefined, r)
      }
      return Stmt.mkImport((name as Value.Sym).value, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'display')) {
      if (args.length !== 1) {
        throw new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, range)
      }
      return Stmt.mkDisplay(valueToOps(args[0]), v, range)

    } else if (Value.isSymName(Value.stripSyntax(head), 'struct')) {
      if (args.length !== 2) {
        throw new ScamperError('Parser', 'Struct statements must have 2 arguments, the name of the struct and a list of fields', undefined, range)
      } 
      const { range: nr, value: name } = Value.unpackSyntax(args[0])
      if (!Value.isSym(name)) {
        throw new ScamperError('Parser', 'The first argument of a struct statement must be a struct name', undefined, nr )
      }
      const { range: sfr, value: sfields } = Value.unpackSyntax(args[1])
      if (!Value.isList(sfields)) {
        throw new ScamperError('Parser', 'The second argument of a struct statement must be a list of fields', undefined, range)
      }
      const fields: string[] = []
      Value.listToVector(sfields as Value.List).forEach((fld) => {
        const { range: r, value: f } = Value.unpackSyntax(fld)
        if (!Value.isSym(f)) {
          throw new ScamperError('Parser', 'Struct fields must be identifiers', undefined, r)
        }
        fields.push((f as Value.Sym).value)
      })
      return Stmt.mkStruct((name as Value.Sym).value, fields, range)
    } else {
      return Stmt.mkStmtExp(valueToOps(v), v, range)
    }
  }
}

export function valuesToProg (exps: Value.T[]): Prog {
   return exps.map(valueToStmt)
}

export function parseProgram (src: string): Prog {
  const tokens = stringToTokens(src)
  const values = tokensToValues(tokens)
  return valuesToProg(values)
}
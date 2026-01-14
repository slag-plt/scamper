import * as L from '../lpm'
import { mkSyntax, Syntax } from './syntax.js'

///// Tokenization /////////////////////////////////////////////////////////////

class Token {
  text: string
  range: L.Range

  constructor (text: string, range: L.Range) {
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
      throw new L.ICE('parser.beginTracking', 'Already tracking a token')
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
      throw new L.ICE('parser.emitToken', 'Not tracking a token')
    } else {
      const token = new Token(
        this.src.slice(this.startIdx, this.startIdx + this.tokenLen),
        L.Range.of(this.startRow, this.startCol, this.startIdx, this.endRow, this.endCol, this.endIdx)
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
      throw new L.ScamperError('Parser', 'Unterminated string literal.',
        undefined, L.Range.of(this.startRow, this.startCol, this.startIdx, this.endRow, this.endCol, this.endIdx))
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

///// Parsing //////////////////////////////////////////////////////////////////

function puffRange(r: L.Range): L.Range {
  return L.Range.of(
    r.begin.line,
    r.begin.col === 1 ? r.begin.col : r.begin.col - 1,
    r.begin.col === 1 ? r.begin.idx : r.begin.idx - 1,
    r.end.line,
    r.end.col,
    r.end.idx + 1
  )
}

const intRegex = /^[+-]?\d+$/
const floatRegex = /^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/

function parseStringLiteral (src: string, range: L.Range): string {
  if (src.length === 0) {
    throw new L.ICE('parseStringLiteral', 'Empty string literal (with no quote!)')
  } else if (src[0] !== '"') {
    throw new L.ScamperError('Parser', 'String literal must begin with a quote', undefined, range)
  }

  let ret = ''
  for (let i = 1; i < src.length; i++) {
    // A quote closes this string literal
    if (src[i] === '"') {
      return ret
    // Escape characters require us to consume the next character
    } else if (src[i] === '\\') {
      if (i+1 >= src.length) {
        throw new L.ScamperError('Parser', 'Escape character "\\" cannot occur at the end of a string.', undefined, range)
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
            throw new L.ScamperError('Parser', 'Octal escape codes not supported', undefined, range)
          } else if (ch === 'x') {
            throw new L.ScamperError('Parser', 'Hex escape codes not supported', undefined, range)
          } else if (ch === 'u' || ch === 'U') {
            throw new L.ScamperError('Parser', 'Unicode escape codes not supported', undefined, range)
          } else if (ch === '\n') {
            // Skip over newline characters but continue processing the literal
          } else {
            // Any other escape sequence is the identity escape sequence
            ret += ch
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

export function readSingle (t: Token, wildAllowed: boolean): Syntax {
  const text = t.text
  if (intRegex.test(text)) {
    return mkSyntax(parseInt(text), t.range)
  } else if (floatRegex.test(text)) {
    return mkSyntax(parseFloat(text), t.range)
  } else if (text === '#t') {
    return mkSyntax(true, t.range)
  } else if (text === '#f') {
    return mkSyntax(false, t.range)
  } else if (text === 'null') {
    return mkSyntax(null, t.range)
  } else if (text.startsWith('"')) {
    return mkSyntax(parseStringLiteral(text, t.range), t.range)
  } else if (text.startsWith('#\\')) {
    const escapedChar = text.slice(2)
    if (escapedChar.length === 1) {
      return mkSyntax(L.mkChar(escapedChar), t.range)
    } else if (L.namedCharValues.has(escapedChar)) {
      return mkSyntax(L.mkChar(L.namedCharValues.get(escapedChar)!), t.range)
    } else {
      throw new L.ScamperError('Parser', `Invalid character literal: ${text}`, undefined, t.range)
    }
  } else {
    // TODO: ensure identifiers don't have invalid characters, i.e., #
    // Probably should be done in the lexer, not the parser...
    if (text.startsWith('_') && !wildAllowed) {
      throw new L.ScamperError('Parser', 'Identifiers cannot begin with "_" unless inside of "section" or patterns', undefined, t.range)
    }
    return mkSyntax(L.mkSym(text), t.range)
  }
}

export function readValue (tokens: Token[]): Syntax {
  const beg = tokens.shift()!
  if (isOpeningBracket(beg.text)) {
    const values = []
    while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
      values.push(readValue(tokens))
    }
    if (tokens.length === 0) {
      // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
      throw new L.ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, puffRange(beg.range))
    } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
      throw new L.ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
        undefined, new L.Range(beg.range.begin, tokens[0].range.end))
    } else {
      const end = tokens.shift()!
      return mkSyntax(
        // N.B., non '[' brackets are lists, i.e., '('. Will need to change if
        // we ever allow '{' to imply an dictionary/object.
        beg.text === '[' ? values : L.mkList(...values),
        new L.Range(beg.range.begin, end.range.end))
    }
  } else if (beg.text === "'") {
    return mkSyntax(L.mkList(L.mkSym('quote'), readValue(tokens)), beg.range)
  } else {
    return readSingle(beg, true)
  }
}

export function readValues (tokens: Token[]): Syntax[] {
  const ret = []
  while (tokens.length > 0) {
    ret.push(readValue(tokens))
  }
  return ret
}

export function read (src: string): Syntax[] {
  const tokens = stringToTokens(src)
  return readValues(tokens)
}
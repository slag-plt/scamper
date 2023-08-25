import { ICE, ScamperError } from './lang.js'
import * as S from './lang.js'

class Token {
  text: string
  range: S.Range

  constructor (text: string, range: S.Range) {
    this.text = text
    this.range = range
  }

  public toString (): string {
    return `["${this.text}" ${this.range.toString()}]`
  }
}

const isWhitespace = (c: string): boolean => /\s/.test(c)
const isOpeningBracket = (ch: string): boolean =>
  ['(', '{', '['].includes(ch)
const isClosingBracket = (ch: string): boolean =>
  [')', '}', ']'].includes(ch)
const isBracket = (ch: string): boolean =>
  isOpeningBracket(ch) || isClosingBracket(ch)
const areMatchingBrackets = (open: string, close: string): boolean => {
  return (open === '(' && close === ')') ||
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
    this.tokenLen = 0

    // N.B., chomp whitespace so we maintain the invariant that the
    // tokenizer is always pointing to a valid token if there are tokens
    // left to process
    this.chompWhitespace()
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
        new S.Range(this.startRow, this.startCol, this.endRow, this.endCol)
      )
      this.resetTracking()
      // N.B., also chomp whitespace here to ensure that the tokenizer is
      // always pointing to a valid token if there are any left
      this.chompWhitespace()
      return token
    }
  }

  advance (): void {
    if (this.isTracking()) {
      this.tokenLen += 1
      this.endRow = this.row
      this.endCol = this.col
    }
    if (this.peek() === '\n') {
      this.row += 1
      this.col = 1
    } else {
      this.col += 1
    }
    this.idx += 1
  }

  chompWhitespace(): void {
    while (isWhitespace(this.peek())) {
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
        undefined, new S.Range(this.startRow, this.startCol, this.endRow, this.endCol))
    // Case: any other sequence of non-whitespace, non-delimiting characters
    } else {
      this.beginTracking()
      this.advance()
      while (!this.isEmpty()) {
        ch = this.peek()
        if (isWhitespace(ch) || isBracket(ch)) {
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

export function tokensToSexp (tokens: Token[]): S.Sexp {
  const beg = tokens.shift()!
  if (isOpeningBracket(beg.text)) {
    const exps = []
    while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
      exps.push(tokensToSexp(tokens))
    }
    if (tokens.length === 0) {
      // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
      throw new ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, beg.range)
    } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
      throw new ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
        undefined, S.mkRange(beg.range.begin, tokens[0].range.end))
    } else {
      const end = tokens.shift()!
      return S.mkList(exps, beg.text as S.Bracket, S.mkRange(beg.range.begin, end.range.end))
    }
  } else {
    return S.mkAtom(beg.text, beg.range)
  }
}

export function tokensToSexps (tokens: Token[]): S.Sexp[] {
  const ret = []
  while (tokens.length > 0) {
    ret.push(tokensToSexp(tokens))
  }
  return ret
}

const intRegex = /^[+-]?\d+$/
const floatRegex = /^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/

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
  'struct',
]

export function atomToExp (e: S.Atom): S.Exp {
  const text = e.value
  if (intRegex.test(text)) {
    return S.mkNum(parseInt(text), e.range)
  } else if (floatRegex.test(e.value)) {
    return S.mkNum(parseFloat(e.value), e.range)
  } else if (e.value === '#t') {
    return S.mkBool(true, e.range)
  } else if (e.value === '#f') {
    return S.mkBool(false, e.range)
  } else if (e.value.startsWith('"')) {
    return S.mkStr(e.value.slice(1, -1), e.range)
  } else if (reservedWords.includes(e.value)) {
    throw new ScamperError('Parser', `Cannot use reserved word as identifier name: ${e.value}`, undefined, e.range)
  } else {
    return S.mkVar(e.value, e.range)
  }
}

export function sexpToBinding (e: S.Sexp): S.Binding {
  if (e.tag === 'atom' || e.value.length !== 2 || e.value[0].tag !== 'atom') {
    throw new ScamperError('Parser', `Bindings must given as pairs of names and values: ${S.sexpToString(e)}`, undefined, e.range)
  } else {
    return { name: e.value[0].value, body: sexpToExp(e.value[1]) }
  }
}

export function sexpToExp (e: S.Sexp): S.Exp {
  switch (e.tag) {
    case 'atom':
      return atomToExp(e)
    case 'list': {
      if (e.value.length === 0) {
        throw new ScamperError('Parser', 'The empty list is not a valid expression', undefined, e.range)
      }
      const head = e.value[0]
      const args = e.value.slice(1)
      if (head.tag === 'atom' && head.value === 'lambda') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Lambda expression must have 2 sub-components, an parameter list and a body', undefined, e.range)
        }
        const es = args[0]
        if (es.tag !== 'list') {
          throw new ScamperError('Parser', 'The first component of a lambda expression must be a parameter list', undefined, es.range)
        }
        const params: string[] = []
        es.value.forEach(arg => {
          if (arg.tag !== 'atom') {
            throw new ScamperError('Parser', 'Parameters must only be identifiers but a list was given instead', undefined, arg.range)
          }
          params.push(arg.value)
        })
        return S.mkLam(params, sexpToExp(args[1]), e.bracket, e.range)
      } else if (head.tag === 'atom' && head.value === 'let') {
        if (args.length !== 2) {
          throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, e.range)
        }
        const binds = args[0]
        if (binds.tag !== 'list') {
          throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, binds.range)
        }
        return S.mkLet(binds.value.map(sexpToBinding), sexpToExp(args[1]), e.bracket, e.range)
      } else if (head.tag === 'atom' && head.value === 'if') {
        if (args.length !== 3) {
          throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, e.range)
        } else {
          return S.mkIf(
            sexpToExp(args[0]),
            sexpToExp(args[1]),
            sexpToExp(args[2]),
            e.bracket,
            e.range
          )
        }
      } else {
        return S.mkApp(sexpToExp(head), args.map(sexpToExp), e.bracket, e.range)
      }
    }
  }
}

export function sexpToStmt (e: S.Sexp): S.Stmt {
  switch (e.tag) {
    case 'atom':
      return S.mkStmtExp(sexpToExp(e))
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
        if (name.tag !== 'atom') {
          throw new ScamperError('Parser', 'The first component of a define statement must be an identifier', undefined, name.range)
        }
        return S.mkStmtBinding(name.value, sexpToExp(args[1]), e.bracket, e.range)
      } else if (head.value === 'import') {
        if (args.length !== 1) {
          throw new ScamperError('Parser', 'Import statements must have 1 argument, the name of a module', undefined, e.range)
        }
        const name = args[0]
        if (name.tag !== 'atom') {
          throw new ScamperError('Parser', 'The argument of an import statement must be a module name', undefined, args[0].range)
        }
        return S.mkImport(name.value, e.bracket, e.range)
      } else if (head.value === 'display') {
        if (args.length !== 1) {
          throw new ScamperError('Parser', 'Display statements must have 1 argument, the expression to display', undefined, e.range)
        }
        return S.mkDisplay(sexpToExp(args[0]), e.bracket, e.range)
      } else {
        return S.mkStmtExp(sexpToExp(e))
      }
    }
  }
}

export function sexpsToProg (exps: S.Sexp[]): S.Prog {
  return exps.map(sexpToStmt)
}

export function parseProgram (src: string): S.Prog {
  const tokens = stringToTokens(src)
  const sexps = tokensToSexps(tokens)
  return sexpsToProg(sexps)
}
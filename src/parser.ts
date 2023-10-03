import { ICE, ScamperError } from './lang.js'
import { Range, mkRange, Prog, Stmt, Op, Value } from './lang.js'

///// Tokenization /////////////////////////////////////////////////////////////

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

///// Parsing //////////////////////////////////////////////////////////////////

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
  'quote',
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

export function parseSingle (t: Token, wildAllowed: boolean): Value.Syntax {
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

function parseBinding (v: Value.T): { name: string, ops: Op.T[] } {
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
         , ops: lower(vec[1]) }
}

function parseMatchBranch (v: Value.T): Op.MatchBranch {
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (!Value.isArray(v)) {
    throw new ScamperError('Parser', 'Match branches must be given as a vector', undefined, range)
  }
  const vec = v as Value.Vector
  // TODO: should we be checking to see if the value is a valid pattern here?
  // Or do we defer to runtime at this point...? Probably depends on the
  // syntax of valid patterns and whether the set is small enough to warrant
  // a static check.
  if (vec.length !== 2) {
    throw new ScamperError('Parser', 'Match branches must be given as a pair of a pattern and an expression', undefined, Value.rangeOf(vec[0]))
  }
  return { pattern: Value.stripAllSyntax(vec[0]), body: lower(vec[1]) }
}

function parseCondBranch (v: Value.T): { cond: Op.T[], body: Op.T[]} {
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (!Value.isArray(v)) {
    throw new ScamperError('Parser', 'Cond branch must be given as a vector', undefined, range)
  }
  const vec = v as Value.Vector
  if (vec.length !== 2) {
    throw new ScamperError('Parser', `Cond branch must be a pair of expressions`, undefined, range)
  }
  return { cond: lower(vec[0]), body: lower(vec[1]) }
}

export function parseValue (tokens: Token[]): Value.Syntax {
  const beg = tokens.shift()!
  if (isOpeningBracket(beg.text)) {
    const values = []
    while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
      values.push(parseValue(tokens))
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
    return Value.mkSyntax(beg.range, Value.mkList(Value.mkSym('quote'), parseValue(tokens)))
  } else {
    return parseSingle(beg, true)
  }
}

export function parseValues (tokens: Token[]): Value.Syntax[] {
  const ret = []
  while (tokens.length > 0) {
    ret.push(parseValue(tokens))
  }
  return ret
}

///// Lowering /////////////////////////////////////////////////////////////////

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function collectSectionHoles (bvars: string[], v: Value.T): Value.T {
  const orig = v
  let { range, value } = Value.unpackSyntax(v)
  v = value
  if (Value.isSymName(Value.stripSyntax(v), '_')) {
    const x = genHoleSym()
    bvars.push(x)
    return Value.mkSyntax(range, Value.mkSym(x))
  } else if (v === null) {
    return orig
  } else if (Value.isList(v)) {
    const values = Value.listToVector(v as Value.List)
    // N.B., do _not_ recursively collect holes in enclosed section forms
    if (Value.isSymName(Value.stripSyntax(values[0]), 'section')) {
      return orig
    } else {
      return Value.mkSyntax(range, Value.mkList(...values.map((v) => collectSectionHoles(bvars, v))))
    }
  } else if (Value.isPair(v)) {
    return Value.mkSyntax(range, Value.mkPair(
      collectSectionHoles(bvars, (v as Value.Pair).fst),
      collectSectionHoles(bvars, (v as Value.Pair).snd)))
  } else if (Value.isArray(v)) {
    return Value.mkSyntax(range, (v as Value.T[]).map((v) => collectSectionHoles(bvars, v)))
  } else {
    return orig
  }
}

const specialForms: Map<string, (args: Value.T[], range: Range) => Op.T[]> = new Map([
  ['lambda', (args, range) => {
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
    return [Op.mkCls(params, lower(args[1]))]
  }],
  
  ['let', (args, range) => {
    if (args.length !== 2) {
      throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
    }
    const { range: bsr, value: bs } = Value.unpackSyntax(args[0])
    if (!Value.isList(bs)) {
      throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
    }
    const bindings = Value.listToVector(bs as Value.List).map(parseBinding)
    const valOps = bindings.flatMap((b) => b.ops)
    return valOps.concat([Op.mkLet(bindings.map((b) => b.name), lower(args[1]))])    
  }],

  ['let*', (args, range) => {
    if (args.length !== 2) {
      throw new ScamperError('Parser', 'Let expression must have 2 sub-components, a binding list and a body', undefined, range)
    }
    const { range: bsr, value: bs } = Value.unpackSyntax(args[0])
    if (!Value.isList(bs)) {
      throw new ScamperError('Parser', 'Let expression bindings must be given as a list', undefined, bsr)
    }
    const bindings = Value.listToVector(bs as Value.List)
    let val = Value.mkSyntax(range, Value.mkList(
      Value.mkSym('let'),
      Value.mkList(bindings[bindings.length - 1]),
      args[1]))
    for (let i = bindings.length-2; i >= 0; i--) {
      val = Value.mkSyntax(range, Value.mkList(
        Value.mkSym('let'),
        Value.mkList(bindings[i]),
        val))
    } 
    return lower(val)
  }],

  ['and', (args, range) => {
    const label = Op.freshLabel()
    return args
      .flatMap((arg) => lower(arg).concat([Op.mkAnd(label, range)]))
      .concat([Op.mkValue(true), Op.mkLbl(label)]) 
  }],

  ['or', (args, range) => {
    const label = Op.freshLabel()
    return args
      .flatMap((arg) => lower(arg).concat([Op.mkOr(label, range)]))
      .concat([Op.mkValue(false), Op.mkLbl(label)]) 
  }],

  ['if', (args, range) => {
    if (args.length !== 3) {
      throw new ScamperError('Parser', 'If expression must have 3 sub-expressions, a guard, if-branch, and else-branch', undefined, range)
    } else {
      return lower(args[0]).concat([
        Op.mkIf(lower(args[1]), lower(args[2]), range)
      ])
    }  
  }],

  ['begin', (args, range) => {
    if (args.length === 0) {
      throw new ScamperError('Parser', 'Begin expression must have at least 1 sub-expression', undefined, range)
    } else {
      return args.flatMap((arg) => lower(arg)).concat([Op.mkSeq(args.length)])
    } 
  }],

  ['match', (args, range) => {
    if (args.length < 2) {
      throw new ScamperError('Parser', 'Match expression must have at least two sub-expressions, a scrutinee at least one branch', undefined, range)
    }
    const scrutinee = args[0]
    const branches = args.slice(1).map(parseMatchBranch)
    return lower(scrutinee).concat([Op.mkMatch(branches, range)])  
  }],

  ['cond', (args, range) => {
    if (args.length < 1) {
      throw new ScamperError('Parser', 'Cond expression must have at least one branch', undefined, range)
    }
    const label = Op.freshLabel()
    const branches = args.map(parseCondBranch)
    return branches
      .flatMap((b) => b.cond.concat([Op.mkCond(b.body, label, range)]))
      .concat([
        Op.mkExn('No branches of "cond" expression matched', undefined, range),
        Op.mkLbl(label)
      ])  
  }],

  ['quote', (args, range) => {
    if (args.length !== 1) {
      throw new ScamperError('Parser', 'Quote expression must have exactly one sub-expression', undefined, range)
    }
    return [Op.mkValue(Value.stripAllSyntax(args[0]))] 
  }],

  ['section', (args, range) => {
    if (args.length === 0) {
      throw new ScamperError('Parser', 'Section expression must have at least one sub-expression', undefined, range)
    }
    const params: string[] = []
    const app = Value.mkList(...args.map((arg) => collectSectionHoles(params, arg)))
    return lower(Value.mkSyntax(range, Value.mkList(
      Value.mkSym('lambda'),
      Value.mkList(...params.map((p) => Value.mkSym(p))),
      app,
    )))
  }]
])

export function lower (v: Value.T): Op.T[] {
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
    const head = Value.stripSyntax(values[0])
    const args = values.slice(1)
    if (Value.isSym(head) && specialForms.has((head as Value.Sym).value)) {
      return specialForms.get((head as Value.Sym).value)!(args, range)
    } else {
      return values.flatMap(lower).concat([
        Op.mkAp(args.length, range)
      ])
    }
  }
}

///// Top-level/program parsing ////////////////////////////////////////////////

export function parseStmt (v: Value.T): Stmt.T {
  let { range, value: uv } = Value.unpackSyntax(v)

  if (!Value.isList(uv)) {
    return Stmt.mkStmtExp(lower(v), v, range)
  } else {
    const values = Value.listToVector(uv as Value.List)
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
      return Stmt.mkStmtBinding((name as Value.Sym).value, lower(args[1]), v, range)

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
      return Stmt.mkDisplay(lower(args[0]), v, range)

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
      return Stmt.mkStmtExp(lower(v), v, range)
    }
  }
}

export function parseProgram (src: string): Prog {
  const tokens = stringToTokens(src)
  const values = parseValues(tokens)
  return values.map(parseStmt)
}
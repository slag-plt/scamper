// Text -> L.Value conversion for the primitive literal forms (numbers,
// strings, chars, booleans, null). Used by lezer-bridge.ts once it already
// knows a leaf node's kind (Number/String/Char/Boolean/Identifier) from the
// grammar, and by docstring/param.ts, which only has a bare snippet of text
// (no parse tree) and needs to guess whether it looks like an identifier.
import * as L from '../lpm/index.js'

const intRegex = /^[+-]?\d+$/
const floatRegex = /^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/

export function parseNumberLiteral(text: string): number {
  return intRegex.test(text) ? parseInt(text) : parseFloat(text)
}

export function parseStringLiteral(text: string, range: L.Range): string {
  if (text.length === 0) {
    throw new L.ICE(
      'parseStringLiteral',
      'Empty string literal (with no quote!)',
    )
  } else if (!text.startsWith('"')) {
    throw new L.ScamperError(
      'Parser',
      'String literal must begin with a quote',
      undefined,
      range,
    )
  }

  let ret = ''
  for (let i = 1; i < text.length; i++) {
    // A quote closes this string literal
    if (text[i] === '"') {
      return ret
      // Escape characters require us to consume the next character
    } else if (text[i] === '\\') {
      if (i + 1 >= text.length) {
        throw new L.ScamperError(
          'Parser',
          'Escape character "\\" cannot occur at the end of a string.',
          undefined,
          range,
        )
      }
      const ch = text[i + 1]
      switch (ch) {
        // Alarm: ASCII 7
        case 'a':
          ret += String.fromCharCode(7)
          break
        // Backspace: ASCII 8
        case 'b':
          ret += String.fromCharCode(8)
          break
        // Tab: ASCII 9
        case 't':
          ret += String.fromCharCode(9)
          break
        // Linefeed: ASCII 10
        case 'n':
          ret += String.fromCharCode(10)
          break
        // Vertical tab: ASCII 11
        case 'v':
          ret += String.fromCharCode(11)
          break
        // Form feed: ASCII 12
        case 'f':
          ret += String.fromCharCode(12)
          break
        // Carriage return: ASCII 13
        case 'r':
          ret += String.fromCharCode(13)
          break
        // Escape: ASCII 27
        case 'e':
          ret += String.fromCharCode(27)
          break
        case '"':
          ret += '"'
          break
        case "'":
          ret += "'"
          break
        case '\\':
          ret += '\\'
          break
        default:
          // NOTE: Extended escape codes are currently not supported
          if (ch >= '0' && ch <= '9') {
            throw new L.ScamperError(
              'Parser',
              'Octal escape codes not supported',
              undefined,
              range,
            )
          } else if (ch === 'x') {
            throw new L.ScamperError(
              'Parser',
              'Hex escape codes not supported',
              undefined,
              range,
            )
          } else if (ch === 'u' || ch === 'U') {
            throw new L.ScamperError(
              'Parser',
              'Unicode escape codes not supported',
              undefined,
              range,
            )
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
      ret += text[i]
    }
  }
  return ret
}

export function parseCharLiteral(text: string, range: L.Range): L.Value {
  const escapedChar = text.slice(2)
  if (escapedChar.length === 1) {
    return L.mkChar(escapedChar)
  } else if (L.namedCharValues.has(escapedChar)) {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    return L.mkChar(L.namedCharValues.get(escapedChar)!)
  } else {
    throw new L.ScamperError(
      'Parser',
      `Invalid character literal: ${text}`,
      undefined,
      range,
    )
  }
}

// N.B., used only by docstring/param.ts, which has to guess whether a bare
// snippet of docstring text (no parse tree available) looks like a valid
// identifier, mirroring the same literal-vs-symbol disambiguation the
// grammar's token rules otherwise handle for real source positions.
export function looksLikeIdentifier(text: string): boolean {
  return (
    !intRegex.test(text) &&
    !floatRegex.test(text) &&
    text !== '#t' &&
    text !== '#f' &&
    text !== 'null' &&
    !text.startsWith('"') &&
    !text.startsWith('#\\') &&
    text.length > 0
  )
}

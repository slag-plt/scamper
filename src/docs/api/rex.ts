import { ArgDoc, Doc } from './docs.js'

export const rexQ: Doc = new Doc(
  'rex?',
  'boolean',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if `v` is a regex, `#f` otherwise.'
)

export const rexEmpty: Doc = new Doc(
  'rex-empty',
  'rex',
  [],
  'Returns a regex that matches the empty string.'
)

export const rexString: Doc = new Doc(
  'rex-string',
  'rex',
  [ new ArgDoc('s', 'string?') ],
  'Returns a regex that matches the exact string `s`.'
)

export const rexRepeat: Doc = new Doc(
  'rex-repeat',
  'rex',
  [
    new ArgDoc('r', 'rex'),
  ],
  'Returns a regex that matches one or more repetitions of the regex `r`.'
)

export const rexRepeat0: Doc = new Doc(
  'rex-repeat-0',
  'rex',
  [
    new ArgDoc('r', 'rex'),
  ],
  'Returns a regex that matches zero or more repetitions of the regex `r`.'
)

export const rexConcat: Doc = new Doc(
  'rex-concat',
  'rex',
  [
    new ArgDoc('...rs', 'rex...'),
  ],
  'Returns a regex that matches the concatenation of the regexes `rs` in order.'
)

export const rexAnyChar: Doc = new Doc(
  'rex-any-char',
  'rex',
  [],
  'Returns a regex that matches any single character.'
)

export const rexCharSet: Doc = new Doc(
  'rex-char-set',
  'rex',
  [
    new ArgDoc('s', 'string?'),
  ],
  'Returns a regex that matches any single character in the string `s`.'
)

export const rexCharAntiset: Doc = new Doc(
  'rex-char-antiset',
  'rex',
  [
    new ArgDoc('s', 'string?'),
  ],
  'Returns a regex that matches any single character not in the string `s`.'
)

export const rexCharRange: Doc = new Doc(
  'rex-char-range',
  'rex',
  [
    new ArgDoc('start', 'char?'),
    new ArgDoc('end', 'char?'),
  ],
  'Returns a regex that matches any single character in the inclusive range from `start` to `end`.'
)

export const rexAnyOf: Doc = new Doc(
  'rex-any-of',
  'rex',
  [
    new ArgDoc('...rs', 'rex...'),
  ],
  'Returns a regex that matches any one of the regexes `rs`.'
)

export const rexOptional: Doc = new Doc(
  'rex-optional',
  'rex',
  [
    new ArgDoc('r', 'rex'),
  ],
  'Returns a regex that matches either the regex `r` or the empty string.'
)

export const rexRegex: Doc = new Doc(
  'regex',
  'rex',
  [
    new ArgDoc('pattern', 'string?'),
    new ArgDoc('flags', 'string?'),
  ],
  'Returns a regex that matches a Javascript regex `pattern`. See [the MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions) for more details.'
)

export const rexFindMatches: Doc = new Doc(
  'rex-find-matches',
  'list?',
  [
    new ArgDoc('r', 'rex'),
    new ArgDoc('s', 'string?'),
  ],
  'Returns a list of all non-overlapping matches of the regex `r` in the string `s`.'
)

export const rexMatches: Doc = new Doc(
  'rex-matches?',
  'boolean',
  [
    new ArgDoc('r', 'rex'),
    new ArgDoc('s', 'string?'),
  ],
  'Returns `#t` if the regex `r` matches the entire string `s`, `#f` otherwise.'
)

export const rexSplitString: Doc = new Doc(
  'rex-split-string',
  'list?',
  [
    new ArgDoc('r', 'rex'),
    new ArgDoc('s', 'string?'),
  ],
  'Splits the string `s` at each match of the regex `r` and returns a list of the resulting substrings.'
)

export const rexToString: Doc = new Doc(
  'rex->string',
  'string?',
  [
    new ArgDoc('r', 'rex'),
  ],
  'Returns the Javascript regex string representation of the regex `r`.'
)
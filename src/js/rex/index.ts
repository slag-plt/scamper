import * as L from '../../lpm'

// From: https://simonwillison.net/2006/Jan/20/escape/
// TODO: replace with RegEx.escape once it is commonly supported
// `^` and `-` matter only inside a character class, but that is where
// rex-char-set and rex-char-antiset put them: unescaped, `(rex-char-set "^a")`
// renders `[^a]` and matches everything *except* `a`, and `(rex-char-set
// "a-z")` renders a range. Every combinator here promises its characters are
// taken literally, so both are escaped everywhere (#600).
const specials = [
  '/', '.', '*', '+', '?', '|', '^', '-',
  '(', ')', '[', ']', '{', '}', '\\'
]
const escapeRegexp = new RegExp(
  '(\\' + specials.join('|\\') + ')', 'g'
)

function regexpEscape (s: string): string {
  return s.replace(escapeRegexp, '\\$1')
}

interface Re {
  toRegexString(): string
}

class RexEmpty implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-empty'
  toRegexString(): string {
    return ''
  }
}

class RexNone implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-none'
  toRegexString(): string {
    return '(?!)'
  }
}

class RexString implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-string'
  value: string
  constructor (value: string) {
    this.value = value
  }

  toRegexString(): string {
    return regexpEscape(this.value)
  }
}

class RexRepeat implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-repeat'
  value: Re
  constructor (value: Re) {
    this.value = value
  }

  toRegexString(): string {
    return `(?:${this.value.toRegexString()})+`
  }
}

class RexRepeat0 implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-repeat-0'
  value: Re
  constructor (value: Re) {
    this.value = value
  }

  toRegexString(): string {
    return `(?:${this.value.toRegexString()})*`
  }
}

class RegExConcat implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-concat'
  values: Re[]
  constructor (values: Re[]) {
    this.values = values
  }

  toRegexString(): string {
    return this.values.map(v => v.toRegexString()).join('')
  }
}

class RexAnyChar implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-any-char'
  toRegexString(): string {
    return '.'
  }
}

class RegCharSet implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-char-set'
  chars: string
  constructor (chars: string) {
    this.chars = chars
  }

  toRegexString(): string {
    return `[${regexpEscape(this.chars)}]`
  }
}

class RegCharAntiset implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-char-antiset'
  chars: string
  constructor (chars: string) {
    this.chars = chars
  }

  toRegexString(): string {
    // Excluding nothing admits every character, newline included -- which `.`
    // does not, so `rex-any-char` is not the answer. `[^]` means the same in
    // Javascript, but only as a quirk of the empty character class.
    return this.chars.length === 0
      ? '[\\s\\S]'
      : `[^${regexpEscape(this.chars)}]`
  }
}

class RegCharRange implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-char-range'
  start: L.Char
  end: L.Char
  constructor (start: L.Char, end: L.Char)
  {
    this.start = start
    this.end = end
  }

  toRegexString(): string {
    return `[${regexpEscape(this.start.value)}-${regexpEscape(this.end.value)}]`
  }
}

class RexAnyOf implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-any-of'
  // Never empty: `rex_rexAnyOf` returns a `RexNone` for that case, since
  // `(?:)` would be the empty string and match everywhere (#533).
  values: Re[]
  constructor (values: Re[]) {
    this.values = values
  }

  toRegexString(): string {
    return `(?:${this.values.map(v => v.toRegexString()).join('|')})`
  }
}

class RexOptional implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-optional'
  value: Re
  constructor (value: Re) {
    this.value = value
  }

  toRegexString(): string {
    return `(?:${this.value.toRegexString()})?`
  }
}

class RexRegex implements L.Struct, Re {
  [key: number]: never;
  [key: string]: L.Value;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'regex'
  pattern: string
  constructor (pattern: string) {
    this.pattern = pattern
  }

  toRegexString(): string {
    return this.pattern
  }
}

export function rex_isRegex (value: L.Value): value is Re {
  return L.isStructKind(value, 'rex-empty') ||
    L.isStructKind(value, 'rex-none') ||
    L.isStructKind(value, 'rex-string') ||
    L.isStructKind(value, 'rex-repeat') ||
    L.isStructKind(value, 'rex-repeat-0') ||
    L.isStructKind(value, 'rex-concat') ||
    L.isStructKind(value, 'rex-any-char') ||
    L.isStructKind(value, 'rex-char-set') ||
    L.isStructKind(value, 'rex-char-antiset') ||
    L.isStructKind(value, 'rex-char-range') ||
    L.isStructKind(value, 'rex-any-of') ||
    L.isStructKind(value, 'rex-optional') ||
    L.isStructKind(value, 'regex')
}

export function rex_rexEmpty(): Re {
  return new RexEmpty()
}

export function rex_rexNone(): Re {
  return new RexNone()
}

export function rex_rexString(s: string): Re {
  return new RexString(s)
}

export function rex_rexRepeat(r: Re): Re {
  return new RexRepeat(r)
}

export function rex_rexRepeat0(r: Re): Re {
  return new RexRepeat0(r)
}

export function rex_rexConcat(...args: Re[]): Re {
  return new RegExConcat(args)
}

export function rex_rexAnyChar(): Re {
  return new RexAnyChar()
}

export function rex_rexCharSet(s: string): Re {
  // A set admitting no characters is exactly the pattern matching nothing.
  // `[]` happens to do the same in Javascript, but says nothing of the intent.
  return s.length === 0 ? new RexNone() : new RegCharSet(s)
}

export function rex_rexCharAntiset(s: string): Re {
  return new RegCharAntiset(s)
}

export function rex_rexCharRange(start: L.Char, end: L.Char): Re {
  return new RegCharRange(start, end)
}

export function rex_rexAnyOf(...args: Re[]): Re {
  // Alternation's unit: with no branch to succeed with, the result must fail on
  // every input. `(?:)` would be the empty string, which matches everywhere.
  return args.length === 0 ? new RexNone() : new RexAnyOf(args)
}

export function rex_rexOptional(r: Re): Re {
  return new RexOptional(r)
}

export function rex_rexRegex(pattern: string): Re {
  return new RexRegex(pattern)
}

export function rex_rexFindMatches (r: Re, s: string): L.List {
  const regex = new RegExp(r.toRegexString(), 'g')
  const matches: string[] = []
  let match: RegExpExecArray | null
  while ((match = regex.exec(s)) !== null) {
    matches.push(match[0])
    if (match.index === regex.lastIndex) {
      regex.lastIndex++
    }
  }
  return L.vectorToList(matches)
}

export function rex_rexMatches (r: Re, s: string): boolean {
  const regex = new RegExp(`^${r.toRegexString()}$`)
  return regex.test(s)
}

export function rex_rexSplitString (r: Re, s: string): L.List {
  const regex = new RegExp(r.toRegexString(), 'g')
  const parts = s.split(regex)
  return L.vectorToList(parts)
}

export function rex_rexToString (r: Re): string {
  return r.toRegexString()
}

////////////////////////////////////////////////////////////////////////////////


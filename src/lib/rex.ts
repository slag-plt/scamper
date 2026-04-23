import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'

const Re: L.Library = new L.Library()

// From: https://simonwillison.net/2006/Jan/20/escape/
// TODO: replace with RegEx.escape once it is commonly supported
const specials = [
  '/', '.', '*', '+', '?', '|',
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

class RexEmpty implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-empty';
  toRegexString(): string {
    return ''
  }
}

class RexString implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-string';
  value: string
  constructor (value: string) {
    this.value = value
  }
  toRegexString(): string {
    return regexpEscape(this.value)
  }
}

class RexRepeat implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-repeat';
  value: Re
  constructor (value: Re) {
    this.value = value
  }
  toRegexString(): string {
    return `(?:${this.value.toRegexString()})+`
  }
}

class RexRepeat0 implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-repeat-0';
  value: Re
  constructor (value: Re) {
    this.value = value
  }
  toRegexString(): string {
    return `(?:${this.value.toRegexString()})*`
  }
}

class RegExConcat implements Re {
  [key: number]: never;
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

class RexAnyChar implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-any-char';
  toRegexString(): string {
    return `.`
  }
}

class RegCharSet implements Re {
  [key: number]: never;
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

class RegCharAntiset implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-char-antiset'
  chars: string
  constructor (chars: string) {
    this.chars = chars
  }
  toRegexString(): string {
    return `[^${regexpEscape(this.chars)}]`
  }
}

class RegCharRange implements Re {
  [key: number]: never;
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

class RexAnyOf implements Re {
  [key: number]: never;
  [L.scamperTag] = 'struct' as const;
  [L.structKind] = 'rex-any-of'
  values: Re[]
  constructor (values: Re[]) {
    this.values = values
  }
  toRegexString(): string {
    return `(?:${this.values.map(v => v.toRegexString()).join('|')})`
  }
}

class RexOptional implements Re {
  [key: number]: never;
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

class RexRegex implements Re {
  [key: number]: never;
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

export function isRegex (value: L.Value): value is Re {
  return L.isStructKind(value, 'rex-empty') ||
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

const specRex: C.Spec = ({
  predicate: isRegex,
  errorMsg: (actual: L.Value) => `expected a regex, received ${L.typeOf(actual)}`
})

export function rexEmpty (...args: []): Re {
  checkContract(args, contract('rex-empty', []))
  return new RexEmpty()
}

export function rexString (...args: [string]): Re {
  checkContract(args, contract('rex-string', [C.string]))
  const [s] = args
  return new RexString(s)
}

export function rexRepeat (...args: [Re]): Re {
  checkContract(args, contract('rex-repeat', [specRex]))
  const [r] = args
  return new RexRepeat(r)
}

export function rexRepeat0 (...args: [Re]): Re {
  checkContract(args, contract('rex-repeat-0', [specRex]))
  const [r] = args
  return new RexRepeat0(r)
}

export function rexConcat (...args: Re[]): Re {
  checkContract(args, contract('rex-concat', [], specRex))
  return new RegExConcat(args)
}

export function rexAnyChar (...args: []): Re {
  checkContract(args, contract('rex-any-char', []))
  return new RexAnyChar()
}

export function rexCharSet (...args: [string]): Re {
  checkContract(args, contract('rex-char-set', [C.string]))
  const [s] = args
  return new RegCharSet(s)
}

export function rexCharAntiset (...args: [string]): Re {
  checkContract(args, contract('rex-char-antiset', [C.string]))
  const [s] = args
  return new RegCharAntiset(s)
}

export function rexCharRange (...args: [L.Char, L.Char]): Re {
  checkContract(args, contract('rex-char-range', [C.char, C.char]))
  const [start, end] = args
  return new RegCharRange(start, end)
}

export function rexAnyOf (...args: Re[]): Re {
  checkContract(args, contract('rex-any-of', [], specRex))
  return new RexAnyOf(args)
}

export function rexOptional (...args: [Re]): Re {
  checkContract(args, contract('rex-optional', [specRex]))
  const [r] = args
  return new RexOptional(r)
}

export function rexRegex (...args: [string]): Re {
  checkContract(args, contract('regex', [C.string]))
  const [pattern] = args
  return new RexRegex(pattern)
}

export function rexFindMatches (...args: [Re, string]): L.List {
  checkContract(args, contract('rex-find-matches', [specRex, C.string]))
  const [r, s] = args
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

export function rexMatches (...args: [Re, string]): boolean {
  checkContract(args, contract('rex-matches?', [specRex, C.string]))
  const [r, s] = args
  const regex = new RegExp(`^${r.toRegexString()}$`)
  return regex.test(s)
}

export function rexSplitString (...args: [Re, string]): L.List {
  checkContract(args, contract('rex-split-string', [specRex, C.string]))
  const [r, s] = args
  const regex = new RegExp(r.toRegexString(), 'g')
  const parts = s.split(regex)
  return L.vectorToList(parts)
}

export function rexToString (...args: [Re]): string {
  checkContract(args, contract('rex->string', [specRex]))
  const [r] = args
  return r.toRegexString()
}

////////////////////////////////////////////////////////////////////////////////

Re.registerValue('rex?', isRegex)
Re.registerValue('rex-empty', rexEmpty)
Re.registerValue('rex-string', rexString)
Re.registerValue('rex-repeat', rexRepeat)
Re.registerValue('rex-repeat-0', rexRepeat0)
Re.registerValue('rex-concat', rexConcat)
Re.registerValue('rex-any-char', rexAnyChar)
Re.registerValue('rex-char-set', rexCharSet)
Re.registerValue('rex-char-antiset', rexCharAntiset)
Re.registerValue('rex-char-range', rexCharRange)
Re.registerValue('rex-any-of', rexAnyOf)
Re.registerValue('rex-optional', rexOptional)
Re.registerValue('regex', rexRegex)

Re.registerValue('rex-find-matches', rexFindMatches)
Re.registerValue('rex-matches?', rexMatches)
Re.registerValue('rex-split-string', rexSplitString)
Re.registerValue('rex->string', rexToString)

export default Re
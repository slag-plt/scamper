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
const escapeRegexp: RegExp = new RegExp(
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-empty';
  toRegexString(): string {
    return ''
  }
}

class RexString implements L.Struct, Re {
  [key: number]: never;
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-string';
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-repeat';
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-repeat-0';
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-concat'
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-any-char';
  toRegexString(): string {
    return `.`
  }
}

class RegCharSet implements L.Struct, Re {
  [key: number]: never;
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-char-set'
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-char-antiset'
  chars: string
  constructor (chars: string) {
    this.chars = chars
  }
  toRegexString(): string {
    return `[^${regexpEscape(this.chars)}]`
  }
}

class RegCharRange implements L.Struct, Re {
  [key: number]: never;
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-char-range'
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-any-of'
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'rex-optional'
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
  [L.scamperTag]: 'struct' = 'struct';
  [L.structKind]: string = 'regex'
  pattern: string
  constructor (pattern: string) {
    this.pattern = pattern
  }
  toRegexString(): string {
    return this.pattern
  }
}

export function isRegex (value: any): value is Re {
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
  errorMsg: (actual: any) => `expected a regex, received ${L.typeOf(actual)}` 
})

export function rexEmpty(): Re {
  checkContract(arguments, contract('rex-empty', []))
  return new RexEmpty()
}

export function rexString(s: string): Re {
  checkContract(arguments, contract('rex-string', [C.string]))
  return new RexString(s)
}

export function rexRepeat(r: Re): Re {
  checkContract(arguments, contract('rex-repeat', [specRex] ))
  return new RexRepeat(r)
}

export function rexRepeat0(r: Re): Re {
  checkContract(arguments, contract('rex-repeat-0', [specRex] ))
  return new RexRepeat0(r)
}

export function rexConcat(...args: Re[]): Re {
  checkContract(arguments, contract('rex-concat', [], specRex))
  return new RegExConcat(args)
}

export function rexAnyChar(): Re {
  checkContract(arguments, contract('rex-any-char', []))
  return new RexAnyChar()
}

export function rexCharSet(s: string): Re {
  checkContract(arguments, contract('rex-char-set', [C.string]))
  return new RegCharSet(s)
}

export function rexCharAntiset(s: string): Re {
  checkContract(arguments, contract('rex-char-antiset', [C.string]))
  return new RegCharAntiset(s)
}

export function rexCharRange(start: L.Char, end: L.Char): Re {
  checkContract(arguments, contract('rex-char-range', [C.char, C.char]))
  return new RegCharRange(start, end)
}

export function rexAnyOf(...args: Re[]): Re {
  checkContract(arguments, contract('rex-any-of', [], specRex))
  return new RexAnyOf(args)
}

export function rexOptional(r: Re): Re {
  checkContract(arguments, contract('rex-optional', [specRex] ))
  return new RexOptional(r)
}

export function rexRegex(pattern: string): Re {
  checkContract(arguments, contract('regex', [C.string]))
  return new RexRegex(pattern)
}

export function rexFindMatches (r: Re, s: string): L.List {
  checkContract(arguments, contract('rex-find-matches', [specRex, C.string]))
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

export function rexMatches (r: Re, s: string): boolean {
  checkContract(arguments, contract('rex-matches?', [specRex, C.string]))
  const regex = new RegExp(`^${r.toRegexString()}$`)
  return regex.test(s)
}

export function rexSplitString (r: Re, s: string): L.List {
  checkContract(arguments, contract('rex-split-string', [specRex, C.string]))
  const regex = new RegExp(r.toRegexString(), 'g')
  const parts = s.split(regex)
  return L.vectorToList(parts)
}

export function rexToString (r: Re): string {
  checkContract(arguments, contract('rex->string', [specRex]))
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
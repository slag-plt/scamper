export type Bracket = '(' | '[' | '{'
export type T = Atom | List
export type Atom = { tag: 'atom', value: string }
export type List = { tag: 'list', bracket: Bracket, value: T[] }

// TODO: add location info to sexps!
export const mkAtom = (value: string): Atom => ({ tag: 'atom', value })
export const mkList = (value: T[], bracket: Bracket = '('): List => ({ tag: 'list', bracket, value }) 

function surround(s: string, bracket: Bracket): string {
  switch (bracket) {
    case '(':
      return `(${s})`
    case '[':
      return `[${s}]`
    case '{':
      return `{${s}}`
  }
}

export function sexpToString(s: T): string {
  switch (s.tag) {
    case 'atom':
      return s.value
    case 'list':
      return `${surround(s.value.map(sexpToString).join(' '), s.bracket)}`
  }
}
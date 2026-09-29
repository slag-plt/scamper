// The keyword source of truth: every word here must have a matching kw<>
// production in syntax.grammar (enforced by
// test/scheme/parsing/grammar-keyword-parity.test.ts), and none of them can
// be used as a variable/identifier name.
export const reservedWords = [
  // The hole, `??` -- exactly two question marks, and an atomic special form
  // rather than a parenthesized one. Reserved so that `(define ?? 5)` is a
  // parse error; `???` is not reserved and reads as an ordinary identifier.
  '??',
  'and',
  'begin',
  'cond',
  'define',
  'define-export',
  // `else` names a `cond`'s fall-through clause and nothing else. Reserved so
  // that `(define else #f)` is a parse error where the mistake is, rather than
  // a silent break of every later `cond` (#639).
  'else',
  'export',
  'if',
  'import',
  'display',
  'lambda',
  'let',
  'match',
  'or',
  'struct',
]

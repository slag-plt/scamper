// The keyword source of truth: every word here must have a matching kw<>
// production in syntax.grammar (enforced by
// test/scheme/parsing/grammar-keyword-parity.test.ts), and none of them can
// be used as a variable/identifier name.
export const reservedWords = [
  'and',
  'apply',
  'begin',
  'cond',
  'define',
  'error',
  'if',
  'import',
  'display',
  'js-var',
  'lambda',
  'let',
  'let*',
  'match',
  'or',
  'quote',
  'section',
  'struct',
  'report',
]

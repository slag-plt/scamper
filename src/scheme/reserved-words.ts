// The keyword source of truth: every word here must have a matching kw<>
// production in syntax.grammar (enforced by
// test/scheme/grammar-keyword-parity.test.ts), and none of them can be used
// as a variable/identifier name.
export const reservedWords = [
  "and",
  "begin",
  "cond",
  "define",
  "if",
  "import",
  "display",
  "lambda",
  "let",
  "let*",
  "match",
  "or",
  "quote",
  "section",
  "struct",
  "report",
]

import { ExternalTokenizer } from '@lezer/lr'
import { AnonHash } from './generated/parser.terms.js'

// The `#` that opens a Clojure-style anonymous function `#(...)`. It is emitted
// only when the `#` is immediately followed by `(` or `{`, so `#t`/`#f`/`#\c`
// and `#`-containing identifiers (e.g. `##report##`) still tokenize normally.
// Declared *before* the main `@tokens` block in syntax.grammar so it wins the
// tie against Identifier (a bare `#` is otherwise a valid identifier char).
export const anonHash = new ExternalTokenizer((input) => {
  if (input.next === 35 /* # */) {
    const after = input.peek(1)
    if (after === 40 /* ( */ || after === 123 /* { */) {
      input.acceptToken(AnonHash, 1)
    }
  }
})

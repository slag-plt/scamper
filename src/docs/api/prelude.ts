import { ArgDoc, Doc } from './docs.js'

export const equal: Doc = new Doc(
  'equal?',
  'boolean?',
  [
    new ArgDoc('v1', 'any'),
    new ArgDoc('v2', 'any')
  ],
  'Returns `#t` if and only `v1` and `v2` are (structurally) equal values.'
)

export const number: Doc = new Doc(
  'number?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a number.'
)

export const real: Doc = new Doc(
  'real?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a real number.'
)

export const integer: Doc = new Doc(
  'integer?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is an integer.'
)

export const nanQ: Doc = new Doc(
  'nan?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is the number `NaN`.'
)

export const lt: Doc = new Doc(
  '<',
  'boolean?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `#t` if and only `v1` is strictly less than `v2`.'
)

export const leq: Doc = new Doc(
  '<=',
  'boolean?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `#t` if and only `v1` is less than or equal to `v2`.'
)

export const gt: Doc = new Doc(
  '>',
  'boolean?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `#t` if and only `v1` is strictly greater than `v2`.'
)

export const geq: Doc = new Doc(
  '>=',
  'boolean?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `#t` if and only `v1` is greater than or equal to `v2`.'
)

export const numeq: Doc = new Doc(
  '=',
  'boolean?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `#t` if and only `v1` is equal to `v2`.'
)

export const numeqEps: Doc = new Doc(
  '=-eps',
  'function?',
  [
    new ArgDoc('epsilon', 'number?')
  ],
  'Returns an equality function that tests two numbers to see if the absolute value of their difference is no greater than `epsilon`, _i.e._, `(< (- x y) epsilon)`.'
)

export const zero: Doc = new Doc(
  'zero?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is zero.'
)

export const positive: Doc = new Doc(
  'positive?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is positive.'
)

export const negative: Doc = new Doc(
  'negative?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is negative.'
)

export const odd: Doc = new Doc(
  'odd?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is odd.'
)

export const even: Doc = new Doc(
  'even?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is even.'
)

export const max: Doc = new Doc(
  'max',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the maximum of the given numbers.'
)

export const min: Doc = new Doc(
  'min',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the minimum of the given numbers.'
)

export const plus: Doc = new Doc(
  '+',
  'number?',
  [ new ArgDoc('v1, v2, ...', 'number?') ],
  'Returns the sum of `v1`, `v2`, ... .'
)

export const minus: Doc = new Doc(
  '-',
  'number?',
  [ new ArgDoc('v1, v2, ...', 'number?') ],
  'Returns the difference of `v1`, `v2`, ... .'
)

export const times: Doc = new Doc(
  '*',
  'number?',
  [ new ArgDoc('v1, v2, ...', 'number?') ],
  'Returns the product of `v1`, `v2`, ... .'
)

export const div: Doc = new Doc(
  '/',
  'number?',
  [ new ArgDoc('v1, v2, ...', 'number?') ],
  'Returns the quotient of `v1`, `v2`, ... .'
)

export const abs: Doc = new Doc(
  'abs',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the absolute value of `v`.'
)

export const quotient: Doc = new Doc(
  'quotient',
  'number?',
  [
    new ArgDoc('v1', 'integer?'),
    new ArgDoc('v2', 'integer?')
  ],
  'Returns the quotient of `v1` and `v2`, _i.e._, the whole number part of `v1 / v2`.'
)

export const remainder: Doc = new Doc(
  'remainder',
  'number?',
  [
    new ArgDoc('v1', 'integer?'),
    new ArgDoc('v2', 'integer?')
  ],
  'Returns the remainder of `v1` and `v2`, _i.e._, the remainder of `v1 / v2`.'
)

export const modulo: Doc = new Doc(
  'modulo',
  'number?',
  [
    new ArgDoc('v1', 'number?'),
    new ArgDoc('v2', 'number?')
  ],
  'Returns `k = n - d * q` where `q` is the integer such that `k` has the same sign as the divisor `d` while being as close to 0 as possible. (Source: [MDN docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder).)'
)

export const floor: Doc = new Doc(
  'floor',
  'integer?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the largest integer less than or equal to `v`.'
)

export const ceiling: Doc = new Doc(
  'ceiling',
  'integer?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the smallest integer greater than or equal to `v`.'
)

export const truncate: Doc = new Doc(
  'truncate',
  'integer?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the integer closest to `v` less than or equal to `v`.'
)

export const round: Doc = new Doc(
  'round',
  'integer?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the integer closest to `v`.'
)

export const square: Doc = new Doc(
  'square',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the square of `v`.'
)

export const sqrt: Doc = new Doc(
  'sqrt',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the square root of `v`.'
)

export const expt: Doc = new Doc(
  'expt',
  'number?',
  [
    new ArgDoc('x', 'number?'),
    new ArgDoc('y', 'number?')
  ],
  'Returns `x` raised to the power of `y`.'
)

export const numberString: Doc = new Doc(
  'number->string',
  'string?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the string representation of `v`.'
)

export const stringNumber: Doc = new Doc(
  'string->number',
  'number?',
  [ new ArgDoc('s', 'string?, presumed to be a number') ],
  'Returns the number denoted by `s` as a `number`.'
)

export const exp: Doc = new Doc(
  'exp',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the exponential of `v`.'
)

export const log: Doc = new Doc(
  'log',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the natural logarithm of `v`.'
)

export const sin: Doc = new Doc(
  'sin',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the sine of `v`.'
)

export const cos: Doc = new Doc(
  'cos',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the cosine of `v`.'
)

export const tan: Doc = new Doc(
  'tan',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the tangent of `v`.'
)

export const asin: Doc = new Doc(
  'asin',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the arc sine of `v`.'
)

export const acos: Doc = new Doc(
  'acos',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the arc cosine of `v`.'
)

export const atan: Doc = new Doc(
  'atan',
  'number?',
  [ new ArgDoc('v', 'number?') ],
  'Returns the arc tangent of `v`.'
)

export const equalsEps: Doc = new Doc(
  '=-eps',
  'procedure?',
  [ new ArgDoc('n', 'number?') ],
  'Returns a function that takes two numbers `x` and `y` as input returns `#t` if `|x - y| < n`.'
)

export const not: Doc = new Doc(
  'not',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is `#f`.'
)

export const boolean: Doc = new Doc(
  'boolean?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a boolean.'
)

export const nand: Doc = new Doc(
  'nand',
  'boolean?',
  [ new ArgDoc('v1, v2, ...', 'boolean?') ],
  'Equivalent to `(not (and v1 v2 ...))`.'
)

export const nor: Doc = new Doc(
  'nor',
  'boolean?',
  [ new ArgDoc('v1, v2, ...', 'boolean?') ],
  'Equivalent to `(not (or v1 v2 ...))`.'
)

export const implies: Doc = new Doc(
  'implies',
  'boolean?',
  [
    new ArgDoc('v1', 'boolean?'),
    new ArgDoc('v2', 'boolean?')
  ],
  'Equivalent to `(if v1 v2 #t)`.'
)

export const xor: Doc = new Doc(
  'xor',
  'boolean?',
  [
    new ArgDoc('v1', 'boolean?'),
    new ArgDoc('v2', 'boolean?')
  ],
  'Equivalent to `(or (and v1 (not v2)) (and (not v1) v2))`.'
)

export const anyOf: Doc = new Doc(
  'any-of',
  'procedure?',
  [ new ArgDoc('f1, f2, ...', 'procedure? that takes a value as input and returns a boolean.') ],
  'Returns a unary function that returns `#t` if and only one of `f1`, `f2`, ... is `#t` for its argument.'
)

export const allOf: Doc = new Doc(
  'all-of',
  'procedure?',
  [ new ArgDoc('f1, f2, ...', 'procedure? that takes a value as input and returns a boolean.') ],
  'Returns a unary function that returns `#t` if and only all of `f1`, `f2`, ... are `#t` for its argument.'
)

export const pairQ: Doc = new Doc(
  'pair?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a pair.'
)

export const cons: Doc = new Doc(
  'cons',
  'pair?',
  [
    new ArgDoc('v1', 'any'),
    new ArgDoc('v2', 'any')
  ],
  'Returns a new cons cell containing `v1` and `v2`.'
)

export const pair: Doc = new Doc(
  'pair',
  'pair?',
  [
    new ArgDoc('v1', 'any'),
    new ArgDoc('v2', 'any')
  ],
  'Returns a new pair containing `v1` and `v2`.'
)

export const car: Doc = new Doc(
  'car',
  'any',
  [ new ArgDoc('v', 'pair? or list?') ],
  'Returns the first element of `v`.'
)

export const cdr: Doc = new Doc(
  'cdr',
  'any',
  [ new ArgDoc('v', 'pair? or list?') ],
  'Returns the second element of `v`.'
)

export const cxxr: Doc = new Doc(
  'c...r',
  'any',
  [ new ArgDoc('v', 'pair? or list?') ],
  'Chains together `car` and `cdr` operations according to the sequence of `a` and `d` characters in the function name, up to four levels deep. For example, `(caddr v)` is equivalent to `(car (cdr (cdr v)))`.'
)

export const nullQ: Doc = new Doc(
  'null?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is the empty list.'
)

export const listQ: Doc = new Doc(
  'list?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a list.'
)

export const listOf: Doc = new Doc(
  'list-of',
  'procedure?',
  [
    new ArgDoc('p', 'procedure?, returns `#t` if its argument is of the desired type'),
  ],
  'Returns a new predicate that tests whether its argument is a list of elements that satisfy the predicate `p`.'
)

export const list: Doc = new Doc(
  'list',
  'list?',
  [ new ArgDoc('v1, v2, ...', 'any') ],
  'Returns a new list containing `v1`, `v2`, ... .'
)

export const makeList: Doc = new Doc(
  'make-list',
  'list?',
  [
    new ArgDoc('n', 'integer?'),
    new ArgDoc('v', 'any')
  ],
  'Returns a new list containing `n` copies of `v`.'
)

export const length: Doc = new Doc(
  'length',
  'integer?',
  [ new ArgDoc('v', 'list?') ],
  'Returns the length of `v`.'
)

export const append: Doc = new Doc(
  'append',
  'list?',
  [ new ArgDoc('l1, l2, ...', 'list?') ],
  'Returns a new list containing the elements of lists `l1`, `l2`, ... in sequence.'
)

export const reverse: Doc = new Doc(
  'reverse',
  'list?',
  [ new ArgDoc('l', 'list?') ],
  'Returns a new list containing the elements of `l` in reverse order.'
)

export const listTail: Doc = new Doc(
  'list-tail',
  'list?',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('k', 'integer?, 0 <= k <= (length l)')
  ],
  'Returns `l` but with the first `k` elements of `l` omitted.'
)

export const listDrop: Doc = new Doc(
  'list-drop',
  'list?',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('k', 'integer?, 0 <= k <= (length l)')
  ],
  'An alias for `(list-tail l k)`.'
)

export const listTake: Doc = new Doc(
  'list-take',
  'list?',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('k', 'integer?, 0 <= k <= (length l)')
  ],
  'Returns a new list containing the first `k` elements of `l`.'
)

export const listRef: Doc = new Doc(
  'list-ref',
  'any',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('n', 'integer?, 0 <= n < (length l)')
  ],
  'Returns the `n`th element of `l`.'
)

export const indexOf: Doc = new Doc(
  'index-of',
  'integer?',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('v', 'any')
  ],
  'Returns the index of the first occurrence of `v` in `l` or `-1` if `v` is not in `l`.'
)

export const assocKey: Doc = new Doc(
  'assoc-key?',
  'any',
  [
    new ArgDoc('k', 'any'),
    new ArgDoc('l', 'list?, an association list')
  ],
  'Returns `#t` if `k` is a key in association list `l`.'
)

export const assocRef: Doc = new Doc(
  'assoc-ref',
  'any',
  [
    new ArgDoc('k', 'any'),
    new ArgDoc('l', 'list?, an association list')
  ],
  'Returns the value associated with key `k` in association list `l`.'
)

export const assocSet: Doc = new Doc(
  'assoc-set',
  'list?',
  [
    new ArgDoc('k', 'any'),
    new ArgDoc('v', 'any'),
    new ArgDoc('l', 'list?, an association list')
  ],
  'Returns a new association list containing the same key-value pairs as `l` except that `k` is associated with `v`.'
)

export const sort: Doc = new Doc(
  'sort',
  'list?',
  [
    new ArgDoc('l', 'list?'),
    new ArgDoc('lt?', 'procedure?, returns `#t` if the first arg is less than the second')
  ],
  'Returns a new list containing the elements of `l` sorted in ascending order according to the comparison function `lt?`.'
)

export const charQ: Doc = new Doc(
  'char?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a character.'
)

export const charEq: Doc = new Doc(
  'char=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters.'
)

export const charLt: Doc = new Doc(
  'char<?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values.'
)

export const charGt: Doc = new Doc(
  'char>?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values.'
)

export const charLeq: Doc = new Doc(
  'char<=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values.'
)

export const charGeq: Doc = new Doc(
  'char>=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values.'
)

export const charEqCi: Doc = new Doc(
  'char-ci=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters, ignoring case.'
)

export const charLtCi: Doc = new Doc(
  'char-ci<?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values, ignoring case.'
)

export const charGtCi: Doc = new Doc(
  'char-ci>?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values, ignoring case.'
)

export const charLeqCi: Doc = new Doc(
  'char-ci<=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values, ignoring case.'
)

export const charGeqCi: Doc = new Doc(
  'char-ci>=?',
  'boolean?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values, ignoring case.'
)

export const charAlphabetic: Doc = new Doc(
  'char-alphabetic?',
  'boolean?',
  [ new ArgDoc('c', 'char?') ],
  'Returns `#t` if and only `c` is an alphabetic character.'
)

export const charNumeric: Doc = new Doc(
  'char-numeric?',
  'boolean?',
  [ new ArgDoc('c', 'char?') ],
  'Returns `#t` if and only `c` is a numeric character.'
)

export const charWhitespace: Doc = new Doc(
  'char-whitespace?',
  'boolean?',
  [ new ArgDoc('c', 'char?') ],
  'Returns `#t` if and only `c` is a whitespace character.'
)

export const charUpperCase: Doc = new Doc(
  'char-upper-case?',
  'boolean?',
  [ new ArgDoc('c', 'char?') ],
  'Returns `#t` if and only `c` is an upper-case character.'
)

export const charLowerCase: Doc = new Doc(
  'char-lower-case?',
  'boolean?',
  [ new ArgDoc('c', 'char?') ],
  'Returns `#t` if and only `c` is a lower-case character.'
)

export const digitValue: Doc = new Doc(
  'digit-value',
  'integer?',
  [ new ArgDoc('c', 'char?') ],
  'Returns the numeric value of `c` if `c` is a decimal digit (0-10), otherwise raises an error.'
)

export const charToInteger: Doc = new Doc(
  'char->integer',
  'integer?',
  [ new ArgDoc('c', 'char?') ],
  'Returns the codepoint value of character `c`.'
)

export const integerToChar: Doc = new Doc(
  'integer->char',
  'char?',
  [ new ArgDoc('n', 'integer?') ],
  'Returns the character with codepoint value `n`.'
)

export const charUpcase: Doc = new Doc(
  'char-upcase',
  'char?',
  [ new ArgDoc('c', 'char?') ],
  'Returns the upper-case equivalent of `c`.'
)

export const charDowncase: Doc = new Doc(
  'char-downcase',
  'char?',
  [ new ArgDoc('c', 'char?') ],
  'Returns the lower-case equivalent of `c`.'
)

export const charFoldcase: Doc = new Doc(
  'char-foldcase',
  'char?',
  [ new ArgDoc('c', 'char?') ],
  'Returns the case-folded equivalent of `c`. This is a version of `c` that is appropriate for case-insensitive comparison.'
)

export const stringQ: Doc = new Doc(
  'string?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a string.'
)

export const makeString: Doc = new Doc(
  'make-string',
  'string?',
  [
    new ArgDoc('k', 'integer?'),
    new ArgDoc('c', 'char?')
  ],
  'Returns a string of length `k` with each character set to `c`.'
)

export const string: Doc = new Doc(
  'string',
  'string?',
  [ new ArgDoc('c1, c2, ...', 'char?') ],
  'Returns a string consisting of the characters `c1`, `c2`, ...'
)

export const stringLength: Doc = new Doc(
  'string-length',
  'integer?',
  [ new ArgDoc('v', 'string?') ],
  'Returns the length of `v`.'
)

export const stringRef: Doc = new Doc(
  'string-ref',
  'string?',
  [
    new ArgDoc('s', 'string?'),
    new ArgDoc('n', 'integer?')
  ],
  'Returns the character at index `n` of string `s`.'
)

export const stringEq: Doc = new Doc(
  'string=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are equivalent strings.'
)

export const stringLt: Doc = new Doc(
  'string<?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order.'
)

export const stringGt: Doc = new Doc(
  'string>?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order.'
)

export const stringLeq: Doc = new Doc(
  'string<=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order.'
)

export const stringGeq: Doc = new Doc(
  'string>=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order.'
)

export const stringEqCi: Doc = new Doc(
  'string-ci=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are equivalent strings, ignoring case.'
)

export const stringLtCi: Doc = new Doc(
  'string-ci<?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order, ignoring case.'
)

export const stringGtCi: Doc = new Doc(
  'string-ci>?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order, ignoring case.'
)

export const stringLeqCi: Doc = new Doc(
  'string-ci<=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order, ignoring case.'
)

export const stringGeqCi: Doc = new Doc(
  'string-ci>=?',
  'boolean?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order, ignoring case.'
)

export const stringUpcase: Doc = new Doc(
  'string-upcase',
  'string?',
  [ new ArgDoc('s', 'string?') ],
  'Returns the upper-case version of `s`.'
)

export const stringDowncase: Doc = new Doc(
  'string-downcase',
  'string?',
  [ new ArgDoc('s', 'string?') ],
  'Returns the lower-case version of `s`.'
)

export const stringFoldcase: Doc = new Doc(
  'string-foldcase',
  'string?',
  [ new ArgDoc('s', 'string?') ],
  'Returns the case-folded version of `s`. This is a version of `s` that is appropriate for case-insensitive comparison.'
)

export const substring: Doc = new Doc(
  'substring',
  'string?',
  [
    new ArgDoc('s', 'string?'),
    new ArgDoc('start', 'integer?'),
    new ArgDoc('end', 'integer?')
  ],
  'Returns the substring of `s` from index `start` (inclusive) to index `end` (exclusive).'
)

export const stringAppend: Doc = new Doc(
  'string-append',
  'string?',
  [ new ArgDoc('s1, s2, ...', 'string?') ],
  'Returns a string made by joining `s1`, `s2`, ... together.'
)

export const fileString: Doc = new Doc(
  'file->string',
  'string?',
  [ new ArgDoc('path', 'string?') ],
  'Returns the contents of the file at `path` as a string.'
)

export const fileLines: Doc = new Doc(
  'file->lines',
  'list?',
  [ new ArgDoc('path', 'string?') ],
  'Returns the contents of the file at `path` as a list of strings, one for each line.'
)

export const withFile: Doc = new Doc(
  'with-file',
  'void',
  [ new ArgDoc('filename', 'string?'), new ArgDoc('fn', 'procedure?') ],
  "Loads `filename` from browser storage and passes its contents to `fn` as input. The output of `fn` is then rendered to the screen."
)

export const withFileChooser: Doc = new Doc(
  'with-file-chooser',
  'void',
  [ new ArgDoc('fn', 'procedure?') ],
  "Renders a file chooser widget. When the user selects a file, its contents are passed to `fn` as input. The output of `fn` is then rendered to the screen."
)

export const vectorQ: Doc = new Doc(
  'vector?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a vector.'
)

export const vector: Doc = new Doc(
  'vector',
  'vector?',
  [ new ArgDoc('v1, v2, ...', 'any') ],
  'Returns a vector consisting of the values `v1`, `v2`, ...'
)

export const makeVector: Doc = new Doc(
  'make-vector',
  'vector?',
  [
    new ArgDoc('k', 'integer?'),
    new ArgDoc('v', 'any')
  ],
  'Returns a vector of length `k` with each element set to `v`.'
)

export const vectorLength: Doc = new Doc(
  'vector-length',
  'integer?',
  [ new ArgDoc('v', 'vector?') ],
  'Returns the length of vector `v`.'
)

export const vectorRef: Doc = new Doc(
  'vector-ref',
  'any',
  [
    new ArgDoc('v', 'vector?'),
    new ArgDoc('n', 'integer?, a valid index into v')
  ],
  'Returns the value at index `n` of vector `v`.'
)

export const vectorSet: Doc = new Doc(
  'vector-set!',
  'void',
  [
    new ArgDoc('v', 'vector?'),
    new ArgDoc('n', 'integer?, a valid index into v'),
    new ArgDoc('x', 'any')
  ],
  'Sets the value at index `n` of vector `v` to `x`.'
)

export const vectorFill: Doc = new Doc(
  'vector-fill!',
  'void',
  [
    new ArgDoc('v', 'vector?'),
    new ArgDoc('x', 'any')
  ],
  'Sets each element of vector `v` to `x`.'
)

export const vectorList: Doc = new Doc(
  'vector->list',
  'list?',
  [ new ArgDoc('v', 'vector?') ],
  'Returns a list consisting of the values in vector `v`.'
)

export const listVector: Doc = new Doc(
  'list->vector',
  'vector?',
  [ new ArgDoc('l', 'list?') ],
  'Returns a vector consisting of the values in list `l`.'
)

export const vectorRange: Doc = new Doc(
  'vector-range',
  'vector?',
  [
    new ArgDoc('beg', 'integer?, this argument can be omitted'),
    new ArgDoc('end', 'integer?'),
    new ArgDoc('step', 'integer?, step > 0, this argument can be omitted')
  ],
  'Returns a vector containing the numbers from `beg` to `end` (exclusive). If `beg` is not given, it defaults to 0. If step is not given, it defaults to 1.'
)

export const stringList: Doc = new Doc(
  'string->list',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Returns a list of the characters in `s`.'
)

export const listString: Doc = new Doc(
  'list->string',
  'string?',
  [ new ArgDoc('l', 'list?') ],
  'Returns a string made by joining the characters in `l` together.'
)

export const stringVector: Doc = new Doc(
  'string->vector',
  'vector?',
  [ new ArgDoc('s', 'string?') ],
  'Returns a vector of the characters in `s`.'
)

export const vectorString: Doc = new Doc(
  'vector->string',
  'string?',
  [ new ArgDoc('v', 'vector?') ],
  'Returns a string made by joining the characters in `v` together.'
)

export const stringContains: Doc = new Doc(
  'string-contains',
  'boolean?',
  [
    new ArgDoc('s1', 'string?'),
    new ArgDoc('s2', 'string?')
  ],
  'Returns `#t` if and only if string `s1` contains string `s2`.'
)

export const stringSplit: Doc = new Doc(
  'string-split',
  'list?',
  [
    new ArgDoc('s', 'string?'),
    new ArgDoc('sep', 'string?')
  ],
  'Returns a list of strings obtained by splitting `s` at occurrences of `sep`.'
)

export const stringSplitVector: Doc = new Doc(
  'string-split-vector',
  'vector?',
  [
    new ArgDoc('s', 'string?'),
    new ArgDoc('sep', 'string?')
  ],
  'Returns a vector of strings obtained by splitting `s` at occurrences of `sep`.'
)

export const procedure: Doc = new Doc(
  'procedure?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a procedure.'
)

export const apply: Doc = new Doc(
  'apply',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'list?')
  ],
  'Calls `f` with the values contained in `l`.'
)

export const stringMap: Doc = new Doc(
  'string-map',
  'string?',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('s', 'string?')
  ],
  'Returns a new string containing the results of applying `f` to each character of `s`.'
)

export const map: Doc = new Doc(
  'map',
  'list?',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'list?')
  ],
  'Returns a new list containing the results of applying `f` to each element of `l`.'
)

export const filter: Doc = new Doc(
  'filter',
  'list?',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'list?')
  ],
  'Returns a new list containing the elements of `l` for which `f` returns `#t`.'
)

export const fold: Doc = new Doc(
  'fold',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'any'),
    new ArgDoc('l', 'list?')
  ],
  'Returns the result of accumulating the result of applying `f` to each element of `l`, starting with initial value `v`. The function `f` takes two arguments, the first is the accumulated value and the second is the current element.'
)

export const reduce: Doc = new Doc(
  'reduce',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'list?')
  ],
  'Like `fold` but uses the first element of `l` as the initial value.'
)

export const foldLeft: Doc = new Doc(
  'fold-left',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'any'),
    new ArgDoc('l', 'list?')
  ],
  'An alias for `fold`.'
)

export const foldRight: Doc = new Doc(
  'fold-right',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'any'),
    new ArgDoc('l', 'list?')
  ],
  'Returns the result of accumulating the result of applying `f` to each element of `l` in reverse order, starting with initial value `v`. The function `f` takes two arguments, the first is the current element and the second is the accumulated value.'
)

export const reduceRight: Doc = new Doc(
  'reduce-right',
  'any',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'list?')
  ],
  'Like `fold-right` but uses the last element of `l` as the initial value.'
)

export const vectorMap: Doc = new Doc(
  'vector-map',
  'vector?',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'vector?')
  ],
  'Returns a new vector containing the results of applying `f` to each element of `v1`, ..., `vk` in a element-wise fashion.'
)

export const vectorMapBang: Doc = new Doc(
  'vector-map!',
  'void',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'vector?')
  ],
  'Mutates v1 with the results of results of applying `f` to each element of `v1`, ..., `vk` in a element-wise fashion.'
)

export const vectorForEach: Doc = new Doc(
  'vector-for-each',
  'void',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('v', 'vector?')
  ],
  'Runs `f` on each element of `v1`, ..., `vk` in a element-wise fashion. `f` takes `k+1` arguments where the first argument is the current index and the remaining arguments are the elements of each vector at that index.'
)

export const forRange: Doc = new Doc(
  'for-range',
  'void',
  [
    new ArgDoc('beg', 'number?'),
    new ArgDoc('end', 'number?'),
    new ArgDoc('f', 'procedure?')
  ],
  'Runs `f` on each integer in the range `[beg, end)`. `f` takes one argument, the current value of integer.'
)

export const voidQ: Doc = new Doc(
  'void?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only if `v` is the void value.'
)

export const vectorFilter: Doc = new Doc(
  'vector-filter',
  'list?',
  [
    new ArgDoc('f', 'procedure?'),
    new ArgDoc('l', 'vector?')
  ],
  'Returns a new vector containing the elements of `l` for which `f` returns `#t`.'
)

export const vectorAppend: Doc = new Doc(
  'vector-append',
  'vector?',
  [ new ArgDoc('v1, ..., vk', 'vector?') ],
  'Returns a new vector containing the elements of `v1`, ..., `vk` in order.'
)

export const error: Doc = new Doc(
  'error',
  'any',
  [ new ArgDoc('msg', 'string?') ],
  'Raises a runtime error with message `msg`.'
)

export const qq: Doc = new Doc(
  '??',
  'any',
  [],
  'A placeholder for an expression that is not yet implemented.'
)

export const compose: Doc = new Doc(
  'compose',
  'procedure?',
  [ new ArgDoc('f1, ..., fk', 'procedure?') ],
  'Returns a new procedure that is the composition of the given functions, _i.e._, `f(x) = f1(f2(...(fk(x))))`.'
)

export const o: Doc = new Doc(
  'o',
  'procedure?',
  [ new ArgDoc('f', 'procedure?') ],
  'A synonym for `compose`.'
)

export const pipe: Doc = new Doc(
  '|>',
  'any',
  [ new ArgDoc('v', 'any'), new ArgDoc('f1, ..., fk', 'procedure?') ],
  'Returns the result of applying the given function in sequence, starting with initial value `v`, _i.e._, `(fk (fk-1(...(f1 v)))`.'
)

export const range: Doc = new Doc(
  'range',
  'list?',
  [ new ArgDoc('beg', 'integer?, this argument can be omitted'), new ArgDoc('end', 'integer?'), new ArgDoc('step', 'integer?, step > 0, this argument can be omitted') ],
  'Returns a list containing the numbers from `beg` to `end` (exclusive). If `beg` is not given, it defaults to 0. If step is not given, it defaults to 1.'
)

export const random: Doc = new Doc(
  'random',
  'list?',
  [ new ArgDoc('n', 'integer?, n >= 0') ],
  'Returns a random number in the range 0 to n (exclusive).'
)

export const withHandler: Doc = new Doc(
  'with-handler',
  'any',
  [ new ArgDoc('h', 'procedure?, a handler'), new ArgDoc('f', 'procedure?, a function'), new ArgDoc('v', 'any') ],
  'Calls `(f v1 .. vk)` and if an error is occurred, calls `(h err)` where `err` is the string associated with the raised error.'
)

export const ignore: Doc = new Doc(
  'ignore',
  'void',
  [ new ArgDoc('v', 'any') ],
  'Suppresses the output of value `v` to the page.'
)

export const setMaximumRecursionDepth: Doc = new Doc(
  'set-maximum-recursion-depth!',
  'void',
  [ new ArgDoc('n', 'number? n >= 0') ],
  'Sets the maximum recursion depth of Scamper to n. Note that tail call-optimized functions do _not_ count towards this limit.'
)

export const stringToWords: Doc = new Doc(
  'string->words',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Returns a list of the words in `s`, stripping whitespace and punctuation.'
)

export const ref: Doc = new Doc(
  'ref',
  'ref?',
  [ new ArgDoc('v', 'any') ],
  'Returns a reference cell initially containing `v`.'
)

export const isRef: Doc = new Doc(
  'ref?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a reference cell.'
)

export const deref: Doc = new Doc(
  'deref',
  'any',
  [ new ArgDoc('r', 'ref?') ],
  'Returns the value contained in reference cell `r`.'
)

export const setRef: Doc = new Doc(
  'set-ref!',
  'void',
  [
    new ArgDoc('r', 'ref?'),
    new ArgDoc('v', 'any')
  ],
  'Sets the value contained in reference cell `r` to `v`.'
)

export const elseV: Doc = new Doc(
  'else',
  'boolean?',
  [],
  'A synonym for `#t` appropriate for use as the final guard of a `cond` expression.'
)

export const nullV: Doc = new Doc(
  'null',
  'list?',
  [],
  'The empty list.'
)

export const pi: Doc = new Doc(
  'pi',
  'number',
  [],
  'The constant π.'
)

export const piGreek: Doc = new Doc(
  'π',
  'number',
  [],
  'The constant π.'
)

export const voidV: Doc = new Doc(
  'void',
  'void',
  [],
  'The void value.'
)

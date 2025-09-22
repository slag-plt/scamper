import { ArgDoc, Doc } from './docs.js'

export const parseCv: Doc = new Doc(
  'parse-csv',
  'any',
  [ new ArgDoc('data', 'string?') ],
  'Parses `data` as a CSV-formatted string and returns a list of rows where each row is a list of fields as strings.'
)

export const stringToChars: Doc = new Doc(
  'string->chars',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Converts the string `s` into a list of char values.'
)

export const stringToWords: Doc = new Doc(
  'string->words',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Splits the string `s` into a list of words, using whitespace as the delimiter.'
)

export const charsToLines: Doc = new Doc(
  'chars->lines',
  'list?',
  [ new ArgDoc('chars', 'list?') ],
  'Converts a list of char values into a list of strings, where each string is a line of text.'
)
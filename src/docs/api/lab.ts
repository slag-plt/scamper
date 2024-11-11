import { ArgDoc, Doc } from './docs.js'

export const title: Doc = new Doc(
  'title',
  'text?',
  [ new ArgDoc('text', 'string?') ],
  'Returns a title element.'
)

export const part: Doc = new Doc(
  'part',
  'text?',
  [ new ArgDoc('text', 'string?') ],
  'Returns a part element.'
)

export const problem: Doc = new Doc(
  'problem',
  'text?',
  [ new ArgDoc('text', 'string?') ],
  'Returns a problem element.'
)

export const description: Doc = new Doc(
  'description',
  'text?',
  [ new ArgDoc('text', 'string?') ],
  'Returns a description element.'
)
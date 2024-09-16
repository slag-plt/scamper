import { ArgDoc, Doc } from './docs.js'

export const textArea: Doc = new Doc(
  'text-area',
  'text-area?',
  [ new ArgDoc('id', 'string?') ],
  'Creates a text area with the given id.'
)

export const textAreaGet: Doc = new Doc(
  'text-area-get',
  'string?',
  [ new ArgDoc('text-area', 'text-area?') ],
  'Returns the text in the given text area.'
)

export const button: Doc = new Doc(
  'button',
  'button?',
  [ new ArgDoc('label', 'string?'), new ArgDoc('fn', 'procedure?') ],
  'Creates a button with the given label and function that is called when the button is pressed.'
)

export const tag: Doc = new Doc(
  'tag',
  'element?',
  [ new ArgDoc('name', 'string?'), new ArgDoc('c', 'any') ],
  'Creates an HTML element with the given name and children.'
)

export const tagSetChildren: Doc = new Doc(
  'tag-set-children!',
  'element?',
  [ new ArgDoc('elt', 'an HTML element'), new ArgDoc('c', 'an HTML element or string') ],
  'Sets `elt`\'s children to be `c1`, `c2`, ..'
)

export const onKeydown: Doc = new Doc(
  'on-keydown!',
  'void?',
  [ new ArgDoc('fn', 'procedure?') ],
  'Calls `fn` whenever a key is pressed while the page is focused. `fn` takes a single argument, the key pressed by the user as a string.'
)
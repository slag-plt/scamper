import Doc from './docs.js'

export const textArea: Doc = new Doc(
  '(text-area id) -> text-area?', [
    'id: string?'
  ],
  'Creates a text area with the given id.'
)

export const textAreaGet: Doc = new Doc(
  '(text-area-get text-area): string?', [
    'text-area: text-area?'
  ],
  'Returns the text in the given text area.'
)

export const button: Doc = new Doc(
  '(button label fn) -> button?', [
    'label: string?',
    'fn: procedure?'
  ],
  'Creates a button with the given label and function that is called when the button is pressed.'
)

export const tag: Doc = new Doc(
  '(tag name c1 c2...) -> element?', [
    'name: string?',
    'c: any'
  ],
  'Creates an HTML element with the given name and children.'
)

export const tagSetChildren: Doc = new Doc(
  '(tag-set-children! name c1 c2...) -> element?', [
    'elt: an HTML element',
    'c: an HTML element or string'
  ],
  'Sets `elt`\'s children to be `c1`, `c2`, ..'
)

export const onKeydown: Doc = new Doc(
  '(on-keydown! fn) -> void?', [
    'fn: procedure?'
  ],
  'Calls `fn` whenever a key is pressed while the page is focused. `fn` takes a single argument, the key pressed by the user as a string.'
)
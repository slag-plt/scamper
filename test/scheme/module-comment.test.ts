import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { moduleDocOf } from '../../src/scheme/docstring/module-doc'
import { librarySources } from '../../src/lib/generated/sources'

/** The module blurb of `src`, or undefined if it has none. */
function blurbOf(src: string): string | undefined {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (program === undefined) {
    throw new Error(diagnostics.map((d) => d.message).join('; '))
  }
  return moduleDocOf(src, program)?.description
}

// A `;;;` block directly above a definition is that definition's docstring; one
// that is not is free-floating, and the first free-floating block is the module
// comment (#411). No new sigil -- the position is what distinguishes them.
describe('finding a module comment', () => {
  test('a header above a blank line is the module comment', () => {
    const src = [
      ';;; Regular expressions for pulling text apart.',
      '',
      ';;; (rex? v) -> boolean?',
      ';;;  v : any',
      ';;; Returns `#t` if `v` is a regex.',
      '(define rex? 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe('Regular expressions for pulling text apart.')
  })

  test('a block directly above a definition is its docstring, not the module comment', () => {
    // The whole of the syntax: with no blank line, the block belongs to the
    // define below it and the file has no module comment at all.
    const src = [
      ';;; (rex? v) -> boolean?',
      ';;;  v : any',
      ';;; Returns `#t` if `v` is a regex.',
      '(define rex? 1)',
    ].join('\n')
    expect(blurbOf(src)).toBeUndefined()
  })

  test("a definition's docstring still reaches the definition", () => {
    // The other side of the same case: taking the header must not cost the
    // first define its own documentation.
    const src = [
      ';;; What this module is for.',
      '',
      ';;; (f x) -> number?',
      ';;;  x : number?',
      ';;; Doubles `x`.',
      '(define f 1)',
    ].join('\n')
    const { program } = tokenizeAndParse(src)
    const define = program?.[0]
    expect(define?.tag).toBe('define')
    expect(
      define?.tag === 'define' ? define.docComments?.length : undefined,
    ).toBe(3)
  })

  test('the first free-floating block wins', () => {
    const src = [
      ';;; The module comment.',
      '',
      ';;; A later free-floating note.',
      '',
      '(define x 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe('The module comment.')
  })

  test('several lines become one paragraph', () => {
    const src = [
      ';;; Regular expressions: patterns for finding,',
      ';;; splitting, and pulling apart text.',
      '',
      '(define x 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe(
      'Regular expressions: patterns for finding, splitting, and pulling apart text.',
    )
  })

  test('ordinary `;;` comments are not a module comment', () => {
    const src = [';; Just a note to whoever opens this.', '', '(define x 1)'].join(
      '\n',
    )
    expect(blurbOf(src)).toBeUndefined()
  })

  test('a `;;` block is passed over rather than ending the search', () => {
    // It says nothing about the module, so the `;;;` block below it is still
    // the module comment.
    const src = [
      ';; A note about the file.',
      '',
      ';;; What this module is for.',
      '',
      '(define x 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe('What this module is for.')
  })

  test('a comment trailing code is not a module comment', () => {
    const src = ['(define x 1) ;;; not a header', '', '(define y 2)'].join('\n')
    expect(blurbOf(src)).toBeUndefined()
  })

  test('a file of nothing but a comment has one', () => {
    // Nothing below it, so it cannot be a docstring.
    expect(blurbOf(';;; A module with nothing in it yet.')).toBe(
      'A module with nothing in it yet.',
    )
  })

  test('an empty file has none', () => {
    expect(blurbOf('')).toBeUndefined()
  })

  test('a file with no comments has none', () => {
    expect(blurbOf('(define x 1)\n(define y 2)\n')).toBeUndefined()
  })

  test('a header at the end of a file is still free-floating', () => {
    const src = ['(define x 1)', '', ';;; Trailing thoughts.'].join('\n')
    expect(blurbOf(src)).toBe('Trailing thoughts.')
  })

  test('`;;;` inside a string is not a comment', () => {
    // collectComments uses the real tokenizer, so this is text, not a header.
    const src = ['(define s ";;; not a comment")', '', '(define y 2)'].join('\n')
    expect(blurbOf(src)).toBeUndefined()
  })

  test('a header directly above an import is still the module comment', () => {
    // Only a definition takes a docstring, so a block above any other statement
    // has nothing to attach to and stays free-floating -- and a file opening
    // with its imports is exactly where a header lands.
    const src = [';;; What this module is for.', '(import image)'].join('\n')
    expect(blurbOf(src)).toBe('What this module is for.')
  })

  test('a header directly above a non-definition statement is the module comment', () => {
    const src = [';;; What this module is for.', '(+ 1 2)'].join('\n')
    expect(blurbOf(src)).toBe('What this module is for.')
  })

  test('a header above a struct belongs to the struct, not the module', () => {
    // A struct takes no docstring yet, but a block above one is plainly about
    // that struct -- promoting it to the file's header would be a surprising
    // way to be wrong.
    const src = [';;; A point in the plane.', '(struct posn (x y))'].join('\n')
    expect(blurbOf(src)).toBeUndefined()
  })
})

// A comment trailing a line of code annotates that line and belongs to no
// block. Left in the run, it bridges the blocks either side of it into one --
// which is how a header came to be swallowed by the first definition below it.
describe('a comment trailing a line of code', () => {
  test('does not bridge a header to the docstring below it', () => {
    // The shape the whole feature targets: a file that opens with its imports,
    // one of which carries an ordinary end-of-line note.
    const src = [
      ';;; A drawing of a house.',
      '(import image) ; shapes and colours',
      ';;; (roof w) -> image?',
      ';;;  w : number?',
      ';;; The roof, `w` wide.',
      '(define roof 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe('A drawing of a house.')
  })

  test('is not spliced into the blurb', () => {
    const src = [';;; Images and shapes.', '(import image) ;;; hmm'].join('\n')
    expect(blurbOf(src)).toBe('Images and shapes.')
  })

  test('above a header does not hide it', () => {
    const src = [
      '(import image) ; note',
      ';;; What this module is for.',
      '',
      '(define x 1)',
    ].join('\n')
    expect(blurbOf(src)).toBe('What this module is for.')
  })

  test('still leaves a real docstring attached to its definition', () => {
    // The other half: dropping trailing comments must not cost the define the
    // block that is genuinely its own.
    const src = [
      '(import image) ; shapes',
      ';;; (f x) -> number?',
      ';;;  x : number?',
      ';;; Doubles `x`.',
      '(define f 1)',
    ].join('\n')
    expect(blurbOf(src)).toBeUndefined()
  })
})

// On a real library rather than a hand-written snippet: the shipped files are
// hundreds of `@category` lines and docstrings deep, and are the input the
// feature actually has to survive. Nothing ships with a header (see
// test/libs/docstrings.test.ts), so one is prepended here.
describe('a header on a real library source', () => {
  const HEADER = [
    ';;; Regular expressions: patterns for finding, splitting,',
    ';;; and pulling apart text.',
  ].join('\n')

  /** The `rex` library's source, as it is shipped. */
  function rexSource(): string {
    const found = librarySources.find(([name]) => name === 'rex')
    if (found === undefined) throw new Error('no rex library')
    return found[1]
  }

  test('is found, and the first definition keeps its docstring', () => {
    const src = `${HEADER}\n\n${rexSource()}`
    const { program } = tokenizeAndParse(src)
    if (program === undefined) throw new Error('rex did not parse')

    expect(moduleDocOf(src, program)?.description).toBe(
      'Regular expressions: patterns for finding, splitting, and pulling apart text.',
    )
    // The failure that would matter: the header swallowing the first define's
    // docstring costs it both its documentation and its contract.
    const first = program.find(
      (stmt) => stmt.tag === 'define' || stmt.tag === 'defexport',
    )
    expect(
      first?.tag === 'defexport' || first?.tag === 'define'
        ? first.docComments?.length
        : undefined,
    ).toBeGreaterThan(0)
  })

  test('without a blank line under it, it is the first definition’s docstring', () => {
    // The mistake the guard in test/libs/docstrings.test.ts exists to catch,
    // shown here on the real thing: no header, and the define keeps the block.
    const src = `${HEADER}\n${rexSource()}`
    const { program } = tokenizeAndParse(src)
    if (program === undefined) throw new Error('rex did not parse')
    expect(moduleDocOf(src, program)).toBeUndefined()
  })

  test('the shipped source has none', () => {
    const src = rexSource()
    const { program } = tokenizeAndParse(src)
    if (program === undefined) throw new Error('rex did not parse')
    expect(moduleDocOf(src, program)).toBeUndefined()
  })
})

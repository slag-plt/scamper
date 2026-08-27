/**
 * Markdown, as a notebook's prose cells are shown (#410).
 *
 * Parsed with `@lezer/markdown` -- the parser CodeMirror's Markdown mode
 * already uses, so the text is read the same way whether it is being edited or
 * being looked at -- and built into DOM nodes here, one element at a time.
 *
 * Never `innerHTML`, and no HTML dependency: raw HTML in the source is shown as
 * the text it is. A prose cell is a comment out of a student's file, which may
 * have come from a shared archive or a lab handout, and nothing in one should
 * be able to become markup on the page.
 */
import { parser } from '@lezer/markdown'
import type { SyntaxNode } from '@lezer/common'

/** Block nodes, whose children are laid out rather than run together. */
const BLOCKS: Record<string, string | undefined> = {
  Paragraph: 'p',
  ATXHeading1: 'h1',
  ATXHeading2: 'h2',
  ATXHeading3: 'h3',
  ATXHeading4: 'h4',
  ATXHeading5: 'h5',
  ATXHeading6: 'h6',
  SetextHeading1: 'h1',
  SetextHeading2: 'h2',
  Blockquote: 'blockquote',
  BulletList: 'ul',
  OrderedList: 'ol',
  ListItem: 'li',
}

/** Inline nodes, whose text between children is text of their own. */
const INLINES: Record<string, string | undefined> = {
  Emphasis: 'em',
  StrongEmphasis: 'strong',
}

/**
 * Syntax that is punctuation rather than content: the `#` of a heading, the
 * `*` of a list, the backticks of a code span. Dropped, since what they mean
 * is the element they produced.
 */
const MARKS = new Set([
  'HeaderMark',
  'QuoteMark',
  'ListMark',
  'LinkMark',
  'EmphasisMark',
  'CodeMark',
  'CodeInfo',
  'LinkTitle',
  'LinkLabel',
  'LinkReference',
  // Where a link points is an attribute of the link, not part of its label.
  'URL',
])

/** @returns `md` rendered as DOM nodes. */
export function renderMarkdown(md: string): DocumentFragment {
  const fragment = document.createDocumentFragment()
  render(parser.parse(md).topNode, md, fragment)
  return fragment
}

/**
 * Appends `node`'s rendering to `parent`.
 *
 * Anything unrecognised falls through to its own source text, so a construct
 * this does not know is shown rather than dropped.
 */
function render(node: SyntaxNode, md: string, parent: Node): void {
  const name = node.name
  if (MARKS.has(name)) return

  if (name === 'Document') {
    children(node, md, parent, false)
    return
  }

  const tag = BLOCKS[name] ?? INLINES[name]
  if (tag !== undefined) {
    const el = document.createElement(tag)
    children(node, md, el, !isContainer(name))
    if (name in BLOCKS) trimEdges(el)
    parent.appendChild(el)
    return
  }

  switch (name) {
    // A code block's text is its own: nothing in it is Markdown, and the
    // fences and language tag are not part of what was written.
    case 'FencedCode':
    case 'CodeBlock': {
      const pre = document.createElement('pre')
      const code = document.createElement('code')
      code.textContent = codeTextOf(node, md)
      pre.appendChild(code)
      parent.appendChild(pre)
      return
    }
    case 'InlineCode': {
      const code = document.createElement('code')
      code.textContent = codeTextOf(node, md)
      parent.appendChild(code)
      return
    }
    case 'Link':
    case 'Image': {
      parent.appendChild(link(node, md))
      return
    }
    case 'Autolink': {
      const url = md.slice(node.from, node.to).replace(/^<|>$/g, '')
      parent.appendChild(anchor(url, url))
      return
    }
    case 'HorizontalRule': {
      parent.appendChild(document.createElement('hr'))
      return
    }
    case 'HardBreak': {
      parent.appendChild(document.createElement('br'))
      return
    }
    case 'Escape': {
      // The escaped character itself: `\*` is an asterisk, not a backslash.
      parent.appendChild(text(md.slice(node.from + 1, node.to)))
      return
    }
    default:
      // Raw HTML, an entity, a table -- shown as the text it is.
      parent.appendChild(text(md.slice(node.from, node.to)))
  }
}

/**
 * Drops the whitespace at the two ends of a block.
 *
 * The space after a heading's `#` and the newline before a paragraph's end are
 * the marks' spacing rather than anything anyone wrote.
 */
function trimEdges(el: HTMLElement): void {
  const first = el.firstChild
  if (first instanceof Text) first.data = first.data.replace(/^\s+/, '')
  const last = el.lastChild
  if (last instanceof Text) last.data = last.data.replace(/\s+$/, '')
}

/** Whether a node holds blocks rather than inline content. */
function isContainer(name: string): boolean {
  return (
    name === 'Blockquote' ||
    name === 'BulletList' ||
    name === 'OrderedList' ||
    name === 'ListItem'
  )
}

/**
 * Renders `node`'s children into `parent`.
 *
 * @param fill whether the source between two children is text. It is inside a
 *        paragraph, where the gaps are the words; it is not between blocks,
 *        where the gaps are newlines and indentation.
 */
function children(
  node: SyntaxNode,
  md: string,
  parent: Node,
  fill: boolean,
): void {
  let at = node.from
  let child = node.firstChild
  // A leaf: its text is all it has. Headings and paragraphs with no markup in
  // them arrive here.
  if (child === null) {
    if (fill) parent.appendChild(text(md.slice(node.from, node.to)))
    return
  }
  for (; child !== null; child = child.nextSibling) {
    if (fill && child.from > at) {
      parent.appendChild(text(md.slice(at, child.from)))
    }
    render(child, md, parent)
    at = child.to
  }
  if (fill && at < node.to) parent.appendChild(text(md.slice(at, node.to)))
}

/** The content of a code node: its `CodeText`, or what is left after its marks. */
function codeTextOf(node: SyntaxNode, md: string): string {
  const parts: string[] = []
  for (let child = node.firstChild; child !== null; child = child.nextSibling) {
    if (child.name === 'CodeText') parts.push(md.slice(child.from, child.to))
  }
  if (parts.length > 0) return parts.join('')
  // An inline code span has no CodeText child; strip its backticks by hand.
  return md.slice(node.from, node.to).replace(/^`+|`+$/g, '')
}

/** A link or an image, from its `URL` child. */
function link(node: SyntaxNode, md: string): Node {
  let url = ''
  for (let child = node.firstChild; child !== null; child = child.nextSibling) {
    if (child.name === 'URL') url = md.slice(child.from, child.to)
  }
  const label = document.createDocumentFragment()
  children(node, md, label, true)
  if (node.name === 'Image') {
    const img = document.createElement('img')
    img.src = safeUrl(url)
    img.alt = label.textContent
    return img
  }
  const a = anchor(url, '')
  a.appendChild(label)
  return a
}

function anchor(url: string, label: string): HTMLAnchorElement {
  const a = document.createElement('a')
  a.href = safeUrl(url)
  // A notebook is someone's file; a link in it leads away from their work, so
  // it opens beside it rather than over it.
  a.target = '_blank'
  a.rel = 'noopener noreferrer'
  if (label.length > 0) a.textContent = label
  return a
}

/**
 * @returns `url` if it is one a link may point at, and '' otherwise.
 *
 * `javascript:` and `data:` are script in a link's clothing, and a prose cell
 * comes from a file that may not have been written by the person reading it.
 */
function safeUrl(url: string): string {
  const trimmed = url.trim()
  return /^(javascript|data|vbscript):/i.test(trimmed) ? '' : trimmed
}

function text(s: string): Text {
  return document.createTextNode(s)
}

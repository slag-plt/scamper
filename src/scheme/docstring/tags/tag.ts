import { ICE, Range } from '../../../lpm'
import { DocComment } from '../docstring'
import { mkScamperErrorWithRange } from '../../util'

export function matchesDocTagFormat(line: string): boolean {
  const splitLine = line.split('@', 2)
  // has an @
  // && the @ is at the beginning
  // && right after the @ is not a whitespace
  return (
    splitLine.length === 2 && splitLine[0] === '' && /^\w/.test(splitLine[1])
  )
}

export interface DocTag<T = unknown> {
  tag: string
  contents: T
  range: Range
}
export type DocTagParser<T> = (contents: string, range: Range) => DocTag<T>
export const DocTagParsers = new Map<string, DocTagParser<unknown>>()

export function parseAllTags(docComments: DocComment[], tags: DocTag[]) {
  while (docComments.length > 0) {
    const comment = docComments.shift()
    if (comment === undefined) {
      throw new ICE(
        'Docstring.parseAllTags',
        'Atomicity violation: doc lines changed while parsing?',
      )
    }
    const { line, range } = comment
    if (!matchesDocTagFormat(line)) {
      throw mkScamperErrorWithRange(
        'Parser',
        `Expected only tags at the end of docstring, but encountered a non-tag: ${line}`,
        range,
      )
    }
    const [tag, ...rest] = line.split(' ')
    tags.push(parseTag(tag, rest.join(' '), range))
  }
}

export function registerDocTagParser<T>(tag: string, parser: DocTagParser<T>) {
  DocTagParsers.set(tag, parser)
}

function parseTag(tag: string, contents: string, range: Range): DocTag {
  const parser = DocTagParsers.get(tag)
  if (parser === undefined) {
    throw mkScamperErrorWithRange('Parser', `Unknown doc tag: ${tag}`, range)
  }
  return parser(contents, range)
}

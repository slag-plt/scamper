import { ICE, Range } from "../../lpm"
import { DocComment } from "./docstring"
import { mkScamperErrorWithRange } from "../util"

export function matchesDocTagFormat(line: string): boolean {
  const splitLine = line.split("@", 2)
  // has an @ && the @ is at the beginning && right after the @ is not a whitespace
  return (
    splitLine.length === 2 && splitLine[0] === "" && /^\w/.test(splitLine[1])
  )
}

// TODO: contents should depend on the tag
// also, we are just going to make tags codified in scamper, no runtime-defined tags
export interface DocTag {
  tag: string
  contents: string
  range: Range
}
export function parseAllTags(docComments: DocComment[], tags: DocTag[]) {
  while (docComments.length > 0) {
    const comment = docComments.shift()
    if (comment === undefined) {
      throw new ICE(
        "Docstring.parseAllTags",
        "Atomicity violation: doc lines changed while parsing?",
      )
    }
    const { line, range } = comment
    if (!matchesDocTagFormat(line)) {
      throw mkScamperErrorWithRange(
        "Parser",
        `Expected only tags at the end of docstring, but encountered a non-tag: ${line}`,
        range,
      )
    }
    const [tag, ...rest] = line.split(" ")
    tags.push({ tag, contents: rest.join(" "), range })
  }
}

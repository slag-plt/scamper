import { ScamperError } from "../lpm"

export function matchesDocTagFormat(line: string): boolean {
  const splitLine = line.split("@", 2)
  // has an @ && the @ is at the beginning && right after the @ is not a whitespace
  return (
    splitLine.length === 2 && splitLine[0] === "" && /^\w/.test(splitLine[1])
  )
}

export interface DocTag {
  tag: string
  contents: string
}
export function parseAllTags(docLines: string[], tags: DocTag[]) {
  while (docLines.length > 0) {
    const line = docLines.shift()
    if (line === undefined) {
      throw new ScamperError(
        "Parser",
        "Atomicity violation: doc lines changed while parsing?",
      )
    }
    if (!matchesDocTagFormat(line)) {
      throw new ScamperError(
        "Parser",
        `Expected only tags at the end of docstring, but encountered a non-tag: ${line}`,
      )
    }
    const [tag, ...rest] = line.split(" ")
    tags.push({ tag, contents: rest.join(" ") })
  }
}

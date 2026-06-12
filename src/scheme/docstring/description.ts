import { ScamperError } from "../../lpm"
import { ParseStage } from "./docstring"

export function parseFunctionDescription(
  docLines: string[],
): string | { stage: ParseStage; description: string } {
  let description = ""
  while (docLines.length > 0) {
    const line = docLines.shift()
    if (line === undefined) {
      throw new ScamperError(
        "Parser",
        "Atomicity violation: doc lines changed while parsing?",
      )
    }
    // description lines cannot start with @
    // we won't check if it's actually a correct tag line, that's the next stage
    if (line.startsWith("@")) {
      // put the line back
      docLines.unshift(line)
      return { stage: ParseStage.Tags, description: description.trim() }
    }
    // just append the line onto the string
    description += line + " "
  }
  return description.trim()
}

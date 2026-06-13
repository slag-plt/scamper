import { ICE } from "../../lpm"
import { DocComment, ParseStage } from "./docstring"

export function parseFunctionDescription(
  docComments: DocComment[],
): string | { stage: ParseStage; description: string } {
  let description = ""
  while (docComments.length > 0) {
    const comment = docComments.shift()
    if (comment === undefined) {
      throw new ICE(
        "Docstring.parseFunctionDescription",
        "Atomicity violation: doc lines changed while parsing?",
      )
    }
    // description lines cannot start with @
    // we won't check if it's actually a correct tag comment, that's the next stage
    if (comment.line.startsWith("@")) {
      // put the comment back
      docComments.unshift(comment)
      return { stage: ParseStage.Tags, description: description.trim() }
    }
    // just append the comment onto the string
    description += comment.line + " "
  }
  return description.trim()
}

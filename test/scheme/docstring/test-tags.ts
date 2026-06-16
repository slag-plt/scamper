import { DocTag, registerDocTagParser } from "../../../src/scheme/docstring/tags/tag"

export const testTag1 = "@tag"
export const testTag1Contents = "tag1 tag2"
export const testTagLine1 = `${testTag1} ${testTag1Contents}`

export const testTag2 = "@another-tag"
export const testTag2Contents = "tag3"
export const testTagLine2 = `${testTag2} ${testTag2Contents}`

function registerPlainTestTag(tag: string): void {
  registerDocTagParser(tag, (contents, range): DocTag<string> => ({
    tag,
    contents,
    range,
  }))
}

registerPlainTestTag(testTag1)
registerPlainTestTag(testTag2)

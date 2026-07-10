import * as L from "../lpm"

/** A single line comment, tracked so docstrings can be reassembled from it. */
export interface Comment {
  line: string
  range: L.Range
}

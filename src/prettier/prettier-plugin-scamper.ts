import { Plugin } from "prettier"
import { Node } from "../lpm"
import { SchemeParser } from "./scheme-parser"

const ScamperPlugin: Plugin<Node> = {
  languages: [
    {
      name: "Scamper",
      parsers: ["scamper-scheme"],
    },
  ],
  parsers: {
    "scamper-scheme": SchemeParser,
  },
}

export default ScamperPlugin

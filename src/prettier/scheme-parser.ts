import { Parser } from "prettier"
import { compile } from "../scheme"
import { SimpleErrorChannel } from "../lpm/output/simple-error"
import { Node, progToNode } from "../lpm"
import { throwNull } from "../util"

type ScamperParser = Parser<Node>

const errChannel = new SimpleErrorChannel()

const parse: ScamperParser["parse"] = (text) => {
  return progToNode(
    compile(errChannel, text) ??
      throwNull(errChannel.getSubthreadErrors().message),
  )
}

const astFormat = "scamper-prog"

const locStart: ScamperParser["locStart"] = (node) => node.range.begin.idx

const locEnd: ScamperParser["locEnd"] = (node) => node.range.end.idx

export const SchemeParser: ScamperParser = {
  parse,
  astFormat,
  locStart,
  locEnd,
}

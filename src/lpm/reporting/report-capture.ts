import type { Value } from "../lang"
import type { PageGraph } from "./pruning"

/** The terminal result of evaluating a report expression. */
export type ReportCapture =
  | { tag: "value"; value: Value }
  | { tag: "page-graph"; pageGraph: PageGraph }

/**
 * Returns the reported value currently shown by both error text and query
 * modals. Page graphs are completed before they are captured, so their root
 * invocation has already settled.
 */
export function getReportCaptureValue(capture: ReportCapture): Value {
  if (capture.tag === "value") {
    return capture.value
  }
  return capture.pageGraph.rootPage.invocation.node.result
}

import { QueryEntry } from "../../../scamper"
import { SimpleErrorChannel } from "../../../lpm/output/simple-error"
import {
  getReportCaptureValue,
  type PageGraph,
  ReportError,
} from "../../../lpm"
import { computed, MaybeRefOrGetter, ref, toValue } from "vue"
import { SchedulerId } from "../../../lpm/scheduler"

export const ModalCols = 10
export const ModalRows = 2
export const ModalPadding = 0.5

export const ModalHeight = ModalPadding * 2 + ModalRows

export const ModalWidth = `${ModalCols.toString()}ch`
export const ModalVerticalPadding = `${ModalPadding.toString()}lh`
const paddingHorizontal = `${ModalPadding.toString()}ch`
export const ModalOverallPadding = `${ModalVerticalPadding} ${paddingHorizontal}`

export const ConnectorHeight = 1

export function getQueryAnchorName(id: SchedulerId) {
  return `--query-${id}`
}

export function useReportedValue(query: MaybeRefOrGetter<QueryEntry>) {
  const q = toValue(query)
  const isDone = ref(false)
  void q.done.finally(() => {
    isDone.value = true
  })

  return computed(() => {
    if (!isDone.value) {
      return "Querying…"
    }
    if (!(q.err instanceof SimpleErrorChannel)) {
      return "Fatal query error"
    }
    const firstErr = q.err.errors.at(0)
    if (!firstErr) {
      return "Queried code could not be reached!"
    }
    if (!(firstErr instanceof ReportError)) {
      return firstErr
    }
    return getReportCaptureValue(firstErr.capture)
  })
}

/** Returns a recursive page graph only after its query has settled. */
export function useReportedPageGraph(query: MaybeRefOrGetter<QueryEntry>) {
  const q = toValue(query)
  const isDone = ref(false)
  void q.done.finally(() => {
    isDone.value = true
  })

  return computed<PageGraph | null>(() => {
    if (!isDone.value || !(q.err instanceof SimpleErrorChannel)) {
      return null
    }
    const firstErr = q.err.errors.at(0)
    if (!(firstErr instanceof ReportError)) {
      return null
    }
    return firstErr.capture.tag === "page-graph"
      ? firstErr.capture.pageGraph
      : null
  })
}

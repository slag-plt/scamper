import { QueryEntry } from "../../../scamper"
import { SimpleErrorChannel } from "../../../lpm/output/simple-error"
import { ReportError } from "../../../lpm"
import { computed, MaybeRefOrGetter, toValue } from "vue"
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
  return computed(() => {
    const q = toValue(query)
    if (!(q.err instanceof SimpleErrorChannel)) {
      return "Fatal query error"
    }
    const firstErr = q.err.errors.at(0)
    // TODO: don't forget done wiring
    if (!firstErr) {
      return "No query found."
    }
    if (!(firstErr instanceof ReportError)) {
      return firstErr
    }
    return firstErr.value
  })
}

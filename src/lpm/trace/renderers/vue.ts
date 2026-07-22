import * as U from '../../util'
import VueRenderer from '../../renderers/vue'
import TraceStartRenderer from './TraceStartRenderer.vue'
import TraceOutputRenderer from './TraceOutputRenderer.vue'

VueRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  () => TraceStartRenderer,
)

VueRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  () => TraceOutputRenderer,
)

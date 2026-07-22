import * as L from '../../../lpm'
import VueRenderer from '../../../lpm/renderers/vue.js'
import ReactiveFileRenderer from './ReactiveFileRenderer.vue'
import ReactiveFileChooserRenderer from './ReactiveFileChooserRenderer.vue'

VueRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file'), () => ReactiveFileRenderer)
VueRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file-chooser'), () => ReactiveFileChooserRenderer)

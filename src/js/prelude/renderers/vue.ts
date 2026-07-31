import * as L from '../../../lpm'
import VueRenderer from '../../../lpm/renderers/vue.js'
import ReactiveFileChooserRenderer from './ReactiveFileChooserRenderer.vue'

VueRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'reactive-file-chooser'), () => ReactiveFileChooserRenderer)

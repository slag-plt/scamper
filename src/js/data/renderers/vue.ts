import VueRenderer from '../../../lpm/renderers/vue.js'
import { data_plotQ } from '../viz.js'
import PlotRenderer from './PlotRenderer.vue'

VueRenderer.registerCustomRenderer(data_plotQ, () => PlotRenderer)

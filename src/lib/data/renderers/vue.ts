import VueRenderer from '../../../lpm/renderers/vue.js'
import { plotQ } from '../viz.js'
import PlotRenderer from './PlotRenderer.vue'

VueRenderer.registerCustomRenderer(plotQ, () => PlotRenderer)

import VueRenderer from '../../../lpm/renderers/vue.js'
import { isRgb, isHsv } from '../color.js'
import { drawingQ } from '../drawing.js'
import { isReactiveImageFile } from '../image.js'
import RgbRenderer from './RgbRenderer.vue'
import HsvRenderer from './HsvRenderer.vue'
import DrawingRenderer from './DrawingRenderer.vue'
import ReactiveImageFileRenderer from './ReactiveImageFileRenderer.vue'

VueRenderer.registerCustomRenderer(isRgb, () => RgbRenderer)
VueRenderer.registerCustomRenderer(isHsv, () => HsvRenderer)
VueRenderer.registerCustomRenderer(drawingQ, () => DrawingRenderer)
VueRenderer.registerCustomRenderer(isReactiveImageFile, () => ReactiveImageFileRenderer)

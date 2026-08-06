import VueRenderer from '../../../lpm/renderers/vue.js'
import { color_isRgb, color_isHsv } from '../color.js'
import { drawing_drawingQ } from '../drawing.js'
import { image_isReactiveImageFile } from '../image.js'
import RgbRenderer from './RgbRenderer.vue'
import HsvRenderer from './HsvRenderer.vue'
import DrawingRenderer from './DrawingRenderer.vue'
import ReactiveImageFileRenderer from './ReactiveImageFileRenderer.vue'

VueRenderer.registerCustomRenderer(color_isRgb, () => RgbRenderer)
VueRenderer.registerCustomRenderer(color_isHsv, () => HsvRenderer)
VueRenderer.registerCustomRenderer(drawing_drawingQ, () => DrawingRenderer)
VueRenderer.registerCustomRenderer(image_isReactiveImageFile, () => ReactiveImageFileRenderer)

import VueRenderer from '../../../lpm/renderers/vue.js'
import { image_isRgb, image_isHsv } from '../color.js'
import { image_drawingQ } from '../drawing.js'
import { image_isReactiveImageFile } from '../image.js'
import RgbRenderer from './RgbRenderer.vue'
import HsvRenderer from './HsvRenderer.vue'
import DrawingRenderer from './DrawingRenderer.vue'
import ReactiveImageFileRenderer from './ReactiveImageFileRenderer.vue'

VueRenderer.registerCustomRenderer(image_isRgb, () => RgbRenderer)
VueRenderer.registerCustomRenderer(image_isHsv, () => HsvRenderer)
VueRenderer.registerCustomRenderer(image_drawingQ, () => DrawingRenderer)
VueRenderer.registerCustomRenderer(image_isReactiveImageFile, () => ReactiveImageFileRenderer)

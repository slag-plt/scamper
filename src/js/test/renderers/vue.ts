import VueRenderer from '../../../lpm/renderers/vue.js'
import { isResult } from '../index.js'
import TestResultRenderer from './TestResultRenderer.vue'

VueRenderer.registerCustomRenderer(isResult, () => TestResultRenderer)

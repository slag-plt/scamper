import VueRenderer from '../../../lpm/renderers/vue.js'
import { test_isResult } from '../index.js'
import TestResultRenderer from './TestResultRenderer.vue'

VueRenderer.registerCustomRenderer(test_isResult, () => TestResultRenderer)

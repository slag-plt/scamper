import VueRenderer from '../../../lpm/renderers/vue.js'
import { isSuiteOutput, isTestResult } from './json.js'
import GradescopeResultsRenderer from './GradescopeResultsRenderer.vue'

VueRenderer.registerCustomRenderer(isTestResult, () => GradescopeResultsRenderer)
VueRenderer.registerCustomRenderer(isSuiteOutput, () => GradescopeResultsRenderer)

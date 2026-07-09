import VueRenderer from '../../../lpm/renderers/vue.js'
import { compositionQ } from '../index.js'
import MusicRenderer from './MusicRenderer.vue'

VueRenderer.registerCustomRenderer(compositionQ, () => MusicRenderer)

import VueRenderer from '../../../lpm/renderers/vue.js'
import { music_compositionQ } from '../index.js'
import MusicRenderer from './MusicRenderer.vue'

VueRenderer.registerCustomRenderer(music_compositionQ, () => MusicRenderer)

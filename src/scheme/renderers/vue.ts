import VueRenderer from "../../lpm/renderers/vue"
import { isPat, isExp } from "../ast.js"
import PatRenderer from "../ast-components/PatRenderer.vue"
import ExpRenderer from "../ast-components/ExpRenderer.vue"

VueRenderer.registerCustomRenderer(isPat, () => PatRenderer)
VueRenderer.registerCustomRenderer(isExp, () => ExpRenderer)

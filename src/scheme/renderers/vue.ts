import VueRenderer from "../../lpm/renderers/vue"
import { isPat, isExp } from "../ast.js"
import PatRenderer from "../ast-components/PatRenderer.vue"
import ExpRenderer from "../ast-components/ExpRenderer.vue"

VueRenderer.registerCustomRenderer(isPat, () => PatRenderer)
VueRenderer.registerCustomRenderer(isExp, () => ExpRenderer)
// StmtRenderer is never rendered since Raiser only raises expressions
// TODO: change if this ever changes!
// VueRenderer.registerCustomRenderer(isStmt, () => StmtRenderer)

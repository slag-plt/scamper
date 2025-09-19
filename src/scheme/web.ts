import * as AST from './ast.js'
import HtmlRenderer from '../lpm/renderers/html-renderer.js'
import hljs from 'highlight.js'

function mkHljsCode (text: string): HTMLElement {
  const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
  const ret = document.createElement('code')
  ret.classList.add('hljs')
  ret.innerHTML = elt.value
  ret.tabIndex = 0;
  return ret
}

HtmlRenderer.registerCustomRenderer(AST.isPat, (v) => mkHljsCode(AST.patToString(v as AST.Pat)))
HtmlRenderer.registerCustomRenderer(AST.isExp, (v) => mkHljsCode(AST.expToString(v as AST.Exp)))
HtmlRenderer.registerCustomRenderer(AST.isStmt, (v) => mkHljsCode(AST.stmtToString(v as AST.Stmt)))
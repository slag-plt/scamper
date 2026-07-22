import { AstPath, doc, Doc, Printer } from "prettier"
import * as A from "../../scheme/ast"
import TextRenderer from "../../lpm/renderers/text"

const {
  builders: { group, indent, join, line, hardline },
} = doc

// ---- Type predicates -------------------------------------------------------

export function isSchemeNode(v: unknown): v is A.SchemeNode {
  if (typeof v !== "object" || v === null) return false
  if (!("tag" in v)) return false
  return typeof v.tag === "string"
}

function isLetBinding(v: unknown): v is { name: string; value: A.Exp } {
  return typeof v === "object" && v !== null && "name" in v && "value" in v
}

function isMatchBranch(v: unknown): v is { pat: A.Pat; body: A.Exp } {
  return typeof v === "object" && v !== null && "pat" in v && "body" in v
}

function isCondBranch(v: unknown): v is { test: A.Exp; body: A.Exp } {
  return typeof v === "object" && v !== null && "test" in v && "body" in v
}

// ---- Printer ----------------------------------------------------------------

export const SchemePrinter: Printer = {
  print: (path, _options, print) => {
    const node: unknown = path.node
    if (!isSchemeNode(node)) return ""

    switch (node.tag) {
      ///// Program ///////////////////////////////////////////////////////////////

      case "prog":
        return join(hardline, path.map(print, "body"))

      ///// Statements ////////////////////////////////////////////////////////////

      case "import":
        return `(import ${node.module})`

      case "define":
        return group([
          "(define ",
          node.name,
          indent([line, path.call(print, "value")]),
          ")",
        ])

      case "display":
        return group([
          "(display",
          indent([line, path.call(print, "value")]),
          ")",
        ])

      case "stmtexp":
        return path.call(print, "expr")

      case "struct":
        return `(struct ${node.name} (${node.fields.join(" ")}))`

      ///// Expressions ///////////////////////////////////////////////////////////

      case "lit":
        return TextRenderer.render(node.value)

      case "var":
        return node.name

      case "app":
        if (node.args.length === 0) {
          return group(["(", path.call(print, "head"), ")"])
        }
        return group([
          "(",
          path.call(print, "head"),
          indent([line, join(line, path.map(print, "args"))]),
          ")",
        ])

      case "lam":
        return group([
          "(lambda (",
          join(" ", node.params),
          node.restParam ? [" . ", node.restParam] : "",
          ")",
          indent([line, path.call(print, "body")]),
          ")",
        ])

      case "let": {
        const bindingDocs: Doc[] = path.map((bindingPath: AstPath) => {
          const raw: unknown = bindingPath.node
          if (!isLetBinding(raw)) return ""
          return group([
            "[",
            raw.name,
            " ",
            bindingPath.call(print, "value"),
            "]",
          ])
        }, "bindings")
        return group([
          "(let",
          indent([line, group(["(", join(line, bindingDocs), ")"])]),
          indent([line, path.call(print, "body")]),
          ")",
        ])
      }

      case "begin":
        return group([
          "(begin",
          indent([line, join(line, path.map(print, "exps"))]),
          ")",
        ])

      case "if":
        return group([
          "(if ",
          path.call(print, "guard"),
          indent([
            line,
            path.call(print, "ifB"),
            line,
            path.call(print, "elseB"),
          ]),
          ")",
        ])

      case "match": {
        const branchDocs: Doc[] = path.map((branchPath: AstPath) => {
          const raw: unknown = branchPath.node
          if (!isMatchBranch(raw)) return ""
          return group([
            "[",
            branchPath.call(print, "pat"),
            " ",
            branchPath.call(print, "body"),
            "]",
          ])
        }, "branches")
        return group([
          "(match ",
          path.call(print, "scrutinee"),
          indent([line, join(line, branchDocs)]),
          ")",
        ])
      }

      case "quote":
        return `'${TextRenderer.render(node.value)}`

      case "jsvar":
        return `(js-var ${JSON.stringify(node.name)})`

      case "error":
        return group(["(error", indent([line, path.call(print, "exp")]), ")"])

      case "apply":
        return group([
          "(apply ",
          path.call(print, "fn"),
          indent([line, path.call(print, "args")]),
          ")",
        ])

      case "let*": {
        const bindingDocs: Doc[] = path.map((bindingPath: AstPath) => {
          const raw: unknown = bindingPath.node
          if (!isLetBinding(raw)) return ""
          return group([
            "[",
            raw.name,
            " ",
            bindingPath.call(print, "value"),
            "]",
          ])
        }, "bindings")
        return group([
          "(let*",
          indent([line, group(["(", join(line, bindingDocs), ")"])]),
          indent([line, path.call(print, "body")]),
          ")",
        ])
      }

      case "and":
        return group([
          "(and",
          indent([line, join(line, path.map(print, "exps"))]),
          ")",
        ])

      case "or":
        return group([
          "(or",
          indent([line, join(line, path.map(print, "exps"))]),
          ")",
        ])

      case "cond": {
        const branchDocs: Doc[] = path.map((branchPath: AstPath) => {
          const raw: unknown = branchPath.node
          if (!isCondBranch(raw)) return ""
          return group([
            "[",
            branchPath.call(print, "test"),
            " ",
            branchPath.call(print, "body"),
            "]",
          ])
        }, "branches")
        return group(["(cond", indent([line, join(line, branchDocs)]), ")"])
      }

      case "section":
        return group([
          "(section",
          indent([line, join(line, path.map(print, "exps"))]),
          ")",
        ])

      case "report":
        return group(["(report", indent([line, path.call(print, "exp")]), ")"])

      ///// Patterns //////////////////////////////////////////////////////////////

      case "pwild":
        return "_"

      case "pvar":
        return node.name

      case "plit":
        return TextRenderer.render(node.value)

      case "pctor":
        if (node.args.length === 0) {
          return `(${node.name})`
        }
        return group([
          "(",
          node.name,
          indent([line, join(line, path.map(print, "args"))]),
          ")",
        ])
    }
  },
}

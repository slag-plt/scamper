import { format, type Plugin } from "prettier"
import type { PageLink, RecursivePage } from "../../../lpm/reporting/pruning"
import { raisePage } from "../../../scheme/raise"
import * as A from "../../../scheme/ast"
import ScamperPlugin from "../../../prettier/prettier-plugin-scamper"
import { SchemePrinter } from "../../../prettier/scheme/printer"

export type PageSourceSegment =
  | { tag: "text"; text: string }
  | { tag: "link"; text: string; link: PageLink }

interface MarkedApp extends A.App {
  pageLinkId?: number
}

const markerStart = "\u200b\u200b"
const markerEnd = "\u200b\u200c"
const markerId = "\u2060"
const markerClose = "\u200d"

function marker(start: boolean, id: number): string {
  return `${start ? markerStart : markerEnd}${markerId.repeat(id + 1)}${markerClose}`
}

/**
 * Raises and formats one page, retaining invisible boundaries around page-link
 * applications. The boundaries are emitted by the printer rather than put in
 * the source string, so they do not alter Scamper Prettier's layout choices.
 */
export async function renderRecursivePage(
  page: RecursivePage,
): Promise<PageSourceSegment[]> {
  const raised = raisePage(page)
  const links: PageLink[] = []
  const source = markLinks(raised.source, raised.links, links)
  const plugin = pagePlugin(source)
  const formatted = await format(A.expToString(raised.source), {
    parser: "scamper-page",
    plugins: [ScamperPlugin, plugin],
    // Pages are small code views rather than a full-width editor. A compact
    // width makes Prettier expose the raised expression's structure.
    printWidth: 28,
  })
  return splitMarkedSource(formatted, links)
}

function pagePlugin(source: A.Exp): Plugin {
  return {
    parsers: {
      "scamper-page": {
        astFormat: "scamper-page",
        parse: () => source,
        locStart: () => 0,
        locEnd: () => 0,
      },
    },
    printers: {
      "scamper-page": {
        print(path, options, print) {
          const node = path.node as unknown
          if (isMarkedApp(node)) {
            return [
              marker(true, node.pageLinkId),
              SchemePrinter.print(path, options, print),
              marker(false, node.pageLinkId),
            ]
          }
          return SchemePrinter.print(path, options, print)
        },
      },
    },
  }
}

function isMarkedApp(
  node: unknown,
): node is MarkedApp & { pageLinkId: number } {
  return (
    typeof node === "object" &&
    node !== null &&
    "tag" in node &&
    node.tag === "app" &&
    "pageLinkId" in node &&
    typeof node.pageLinkId === "number"
  )
}

/**
 * Copies the raised AST with a page-link id on each linked application.
 * A linked outer application deliberately stops this traversal: nested native
 * buttons are invalid HTML, so the current UI gives the outer link priority.
 */
function markLinks(
  exp: A.Exp,
  pageLinks: ReadonlyMap<A.App, PageLink>,
  links: PageLink[],
): A.Exp {
  if (exp.tag === "app") {
    const link = pageLinks.get(exp)
    if (link) {
      const pageLinkId = links.push(link) - 1
      return { ...exp, pageLinkId } as MarkedApp
    }
    return {
      ...exp,
      head: markLinks(exp.head, pageLinks, links),
      args: exp.args.map((arg) => markLinks(arg, pageLinks, links)),
    }
  }

  switch (exp.tag) {
    case "lam":
      return { ...exp, body: markLinks(exp.body, pageLinks, links) }
    case "report":
      return { ...exp, exp: markLinks(exp.exp, pageLinks, links) }
    case "let":
    case "let*":
      return {
        ...exp,
        bindings: exp.bindings.map((binding) => ({
          ...binding,
          value: markLinks(binding.value, pageLinks, links),
        })),
        body: markLinks(exp.body, pageLinks, links),
      }
    case "begin":
    case "and":
    case "or":
    case "section":
      return {
        ...exp,
        exps: exp.exps.map((subexp) => markLinks(subexp, pageLinks, links)),
      }
    case "if":
      return {
        ...exp,
        guard: markLinks(exp.guard, pageLinks, links),
        ifB: markLinks(exp.ifB, pageLinks, links),
        elseB: markLinks(exp.elseB, pageLinks, links),
      }
    case "match":
      return {
        ...exp,
        scrutinee: markLinks(exp.scrutinee, pageLinks, links),
        branches: exp.branches.map((branch) => ({
          ...branch,
          body: markLinks(branch.body, pageLinks, links),
        })),
      }
    case "cond":
      return {
        ...exp,
        branches: exp.branches.map((branch) => ({
          test: markLinks(branch.test, pageLinks, links),
          body: markLinks(branch.body, pageLinks, links),
        })),
      }
    case "lit":
    case "var":
    case "quote":
      return exp
  }
}

function splitMarkedSource(
  source: string,
  links: readonly PageLink[],
): PageSourceSegment[] {
  const segments: PageSourceSegment[] = []
  let offset = 0

  while (offset < source.length) {
    const start = source.indexOf(markerStart, offset)
    if (start === -1) {
      pushText(segments, source.slice(offset))
      break
    }
    pushText(segments, source.slice(offset, start))
    const startMarker = readMarker(source, start, true)
    const end = source.indexOf(markerEnd, startMarker.end)
    if (end === -1) {
      throw new Error("Recursive page formatter lost a page-link boundary")
    }
    const endMarker = readMarker(source, end, false)
    if (startMarker.id !== endMarker.id) {
      throw new Error("Recursive page formatter crossed page-link boundaries")
    }
    const link = links.at(startMarker.id)
    if (!link) {
      throw new Error("Recursive page formatter produced an unknown page link")
    }
    segments.push({
      tag: "link",
      text: source.slice(startMarker.end, end),
      link,
    })
    offset = endMarker.end
  }
  return segments
}

function readMarker(
  source: string,
  start: number,
  isStart: boolean,
): { id: number; end: number } {
  const prefix = isStart ? markerStart : markerEnd
  if (!source.startsWith(prefix, start)) {
    throw new Error("Recursive page formatter produced a malformed boundary")
  }
  let cursor = start + prefix.length
  let length = 0
  while (source[cursor] === markerId) {
    length++
    cursor++
  }
  if (length === 0 || source[cursor] !== markerClose) {
    throw new Error(
      "Recursive page formatter produced a malformed page-link id",
    )
  }
  return { id: length - 1, end: cursor + markerClose.length }
}

function pushText(segments: PageSourceSegment[], text: string) {
  if (text.length > 0) {
    segments.push({ tag: "text", text })
  }
}

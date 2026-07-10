import type { Closure, Value } from "../lang"
import { isClosure } from "../util"
import type { InvocationNode, ReportTrace } from "./invocation-node"

/** A completed application that can be raised as its settled literal value. */
export interface LeafInvocation {
  tag: "leaf"
  /** The raw dynamic invocation collapsed into this leaf. */
  node: InvocationNode
  value: Value
}

/** An invocation that remains on a route to a recursive page. */
export interface BranchInvocation {
  tag: "branch"
  /** The raw dynamic invocation represented by this retained branch. */
  node: InvocationNode
  children: PrunedNode[]
  /** True when this invocation is a tracked closure call that recurs. */
  isRecursive: boolean
}

export type PrunedNode = LeafInvocation | BranchInvocation

/** The first, non-destructive pruning pass over a completed report capture. */
export interface PrunedCapture {
  /** The exact runtime closure selected from the report root. */
  trackedClosure: Closure
  root: BranchInvocation
}

/** A navigable route from one materialized page to another. */
export interface PageLink {
  targetPage: RecursivePage
  /** Unary intermediary calls elided between the source and target pages. */
  skippedInvocations: BranchInvocation[]
}

/** One invocation that should receive its own recursive-page view. */
export interface RecursivePage {
  invocation: BranchInvocation
  links: PageLink[]
}

/** The second pruning pass, with unary bridges collapsed into route links. */
export interface PageGraph {
  rootPage: RecursivePage
  pages: RecursivePage[]
}

/**
 * Reduces subtrees that do not lead to another qualifying tracked invocation
 * to their settled result. The tracked closure is the direct child of the
 * report root, and is compared by runtime identity rather than by name.
 */
export function pruneNonRecursiveBranches(
  trace: ReportTrace,
): PrunedCapture | null {
  // This runs only after the reported expression completes: an active
  // invocation would make its result unavailable for literal replacement.
  if (trace.stack.length > 0) {
    return null
  }

  // The query contract gives the reported closure a direct report-root child.
  // Do not scan descendants or infer a function from its source name.
  const root = trace.root.children.at(0)
  if (!root) {
    return null
  }
  const tracked = root.fn
  if (!isClosure(tracked)) {
    return null
  }

  const prunedRoot = materialize(analyze(root, tracked))
  if (prunedRoot.tag === "leaf") {
    // A base-case root has no recursive page to show.
    return null
  }

  return { trackedClosure: tracked, root: prunedRoot }
}

/**
 * Materializes recursive calls, their direct callers, and forks. All other
 * retained nodes are unary bridges represented by PageLink.skippedInvocations.
 */
export function truncateUnaryPaths(prunedCapture: PrunedCapture): PageGraph {
  const pageNodes = new Set<BranchInvocation>()
  markMaterializedPages(prunedCapture.root, true, pageNodes)

  const pages: RecursivePage[] = []
  const pagesByInvocation = new Map<BranchInvocation, RecursivePage>()
  const builtPages = new Set<BranchInvocation>()

  const pageFor = (invocation: BranchInvocation): RecursivePage => {
    const existing = pagesByInvocation.get(invocation)
    if (existing) {
      return existing
    }
    const page: RecursivePage = { invocation, links: [] }
    pagesByInvocation.set(invocation, page)
    pages.push(page)
    return page
  }

  const linksToNextPages = (
    invocation: BranchInvocation,
    skipped: BranchInvocation[],
  ): PageLink[] => {
    if (pageNodes.has(invocation)) {
      return [
        {
          targetPage: pageFor(invocation),
          skippedInvocations: skipped,
        },
      ]
    }

    // A non-page retained node is normally a unary bridge. The flat-map keeps
    // this mock lossless if a future materialization rule changes that shape.
    return retainedChildren(invocation).flatMap((child) =>
      linksToNextPages(child, [...skipped, invocation]),
    )
  }

  const buildPage = (invocation: BranchInvocation): RecursivePage => {
    const page = pageFor(invocation)
    if (builtPages.has(invocation)) {
      return page
    }
    builtPages.add(invocation)

    page.links = retainedChildren(invocation).flatMap((child) =>
      linksToNextPages(child, []),
    )
    for (const link of page.links) {
      buildPage(link.targetPage.invocation)
    }
    return page
  }

  return { rootPage: buildPage(prunedCapture.root), pages }
}

/** Runs both pruning passes and retains only the user-facing page graph. */
export function buildPageGraph(trace: ReportTrace): PageGraph | null {
  const prunedCapture = pruneNonRecursiveBranches(trace)
  if (!prunedCapture) {
    return null
  }
  return truncateUnaryPaths(prunedCapture)
}

interface InvocationAnalysis {
  invocation: InvocationNode
  children: InvocationAnalysis[]
  hasTrackedDescendant: boolean
  isRecursive: boolean
  hasRecursiveDescendant: boolean
}

function analyze(
  invocation: InvocationNode,
  tracked: Closure,
): InvocationAnalysis {
  const children = invocation.children.map((child) => analyze(child, tracked))
  const hasTrackedDescendant = children.some(
    (child) => child.invocation.fn === tracked || child.hasTrackedDescendant,
  )
  const isRecursive = invocation.fn === tracked && hasTrackedDescendant
  const hasRecursiveDescendant = children.some(
    (child) => child.isRecursive || child.hasRecursiveDescendant,
  )

  return {
    invocation,
    children,
    hasTrackedDescendant,
    isRecursive,
    hasRecursiveDescendant,
  }
}

function materialize(analysis: InvocationAnalysis): PrunedNode {
  if (!analysis.isRecursive && !analysis.hasRecursiveDescendant) {
    return {
      tag: "leaf",
      node: analysis.invocation,
      value: analysis.invocation.result,
    }
  }

  return {
    tag: "branch",
    node: analysis.invocation,
    children: analysis.children.map(materialize),
    isRecursive: analysis.isRecursive,
  }
}

function markMaterializedPages(
  invocation: BranchInvocation,
  isRoot: boolean,
  pageNodes: Set<BranchInvocation>,
): void {
  const children = retainedChildren(invocation)
  if (
    isRoot ||
    invocation.isRecursive ||
    children.some((child) => child.isRecursive) ||
    children.length >= 2
  ) {
    pageNodes.add(invocation)
  }

  for (const child of children) {
    markMaterializedPages(child, false, pageNodes)
  }
}

function retainedChildren(invocation: BranchInvocation): BranchInvocation[] {
  return invocation.children.filter(
    (child): child is BranchInvocation => child.tag === "branch",
  )
}

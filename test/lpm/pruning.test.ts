import { describe, expect, test } from "vitest"
import { Closure, Env, Value } from "../../src/lpm"
import {
  InvocationNode,
  ReportTrace,
} from "../../src/lpm/reporting/invocation-node"
import {
  BranchInvocation,
  buildPageGraph,
  pruneNonRecursiveBranches,
  truncateUnaryPaths,
} from "../../src/lpm/reporting/pruning"
import * as U from "../../src/lpm/util"

function closure(name: string): Closure {
  return U.mkClosure([], [], new Map(), () => undefined, name)
}

function invocation(
  fn: Closure,
  children: InvocationNode[] = [],
  result: Value = 0,
): InvocationNode {
  return {
    fn,
    env: Env.empty,
    args: [],
    children,
    result,
    apIdx: 0,
  }
}

function trace(root: InvocationNode): ReportTrace {
  return { root: { children: [root] }, stack: [] }
}

function invocationChild(node: BranchInvocation, index = 0) {
  const child = node.children.at(index)
  expect(child?.tag).toBe("branch")
  return child as BranchInvocation
}

describe("recursive report pruning", () => {
  test("reduces base cases and unrelated work to settled literals", () => {
    const f = closure("f")
    const g = closure("g")
    const sideEffectFree = closure("side-effect-free")

    const baseCase = invocation(f, [], 1)
    const recursiveCall = invocation(f, [baseCase], 2)
    const bridge = invocation(g, [recursiveCall], 3)
    const unrelated = invocation(sideEffectFree, [], 4)
    const root = invocation(f, [bridge, unrelated], 5)

    const prunedCapture = pruneNonRecursiveBranches(trace(root))
    expect(prunedCapture).not.toBeNull()
    if (!prunedCapture) {
      return
    }

    expect(prunedCapture.trackedClosure).toBe(f)
    expect(prunedCapture.root.isRecursive).toBe(true)

    const retainedBridge = invocationChild(prunedCapture.root)
    expect(retainedBridge.node).toBe(bridge)
    const retainedRecursiveCall = invocationChild(retainedBridge)
    expect(retainedRecursiveCall.node).toBe(recursiveCall)

    expect(retainedRecursiveCall.children[0]).toMatchObject({
      tag: "leaf",
      node: baseCase,
      value: 1,
    })
    expect(prunedCapture.root.children[1]).toMatchObject({
      tag: "leaf",
      node: unrelated,
      value: 4,
    })
  })

  test("compares the direct report-root closure by identity, not name", () => {
    const tracked = closure("f")
    const sameName = closure("f")
    const foreignBase = invocation(sameName, [], 1)
    const root = invocation(tracked, [foreignBase], 2)

    expect(pruneNonRecursiveBranches(trace(root))).toBeNull()
  })

  test("does not materialize a base-case tracked root", () => {
    const f = closure("f")

    expect(pruneNonRecursiveBranches(trace(invocation(f, [], 1)))).toBeNull()
  })

  test("retains only the final page graph after both pruning passes", () => {
    const f = closure("f")
    const baseCase = invocation(f, [], 1)
    const root = invocation(f, [baseCase], 2)

    const pageGraph = buildPageGraph(trace(root))
    expect(pageGraph?.rootPage.invocation.node).toBe(root)
  })

  test("collapses unary bridges but preserves direct callers and forks", () => {
    const f = closure("f")
    const g = closure("g")
    const h = closure("h")
    const left = closure("left")
    const right = closure("right")

    const firstBase = invocation(f, [], 1)
    const firstRecursive = invocation(f, [firstBase], 2)
    const leftCaller = invocation(left, [firstRecursive], 3)

    const secondBase = invocation(f, [], 4)
    const secondRecursive = invocation(f, [secondBase], 5)
    const rightCaller = invocation(right, [secondRecursive], 6)

    const fork = invocation(h, [leftCaller, rightCaller], 7)
    const unaryBridge = invocation(g, [fork], 8)
    const root = invocation(f, [unaryBridge], 9)

    const prunedCapture = pruneNonRecursiveBranches(trace(root))
    expect(prunedCapture).not.toBeNull()
    if (!prunedCapture) {
      return
    }
    const pageGraph = truncateUnaryPaths(prunedCapture)

    const pageInvocations = pageGraph.pages.map((page) => page.invocation.node)
    expect(pageInvocations).toHaveLength(6)
    for (const expected of [
      root,
      fork,
      leftCaller,
      firstRecursive,
      rightCaller,
      secondRecursive,
    ]) {
      expect(pageInvocations).toContain(expected)
    }

    const [rootLink] = pageGraph.rootPage.links
    expect(rootLink.targetPage.invocation.node).toBe(fork)
    expect(rootLink.skippedInvocations.map((node) => node.node)).toEqual([
      unaryBridge,
    ])

    const forkPage = rootLink.targetPage
    expect(
      forkPage.links.map((link) => link.targetPage.invocation.node),
    ).toEqual([leftCaller, rightCaller])
    expect(
      forkPage.links.every((link) => link.skippedInvocations.length === 0),
    ).toBe(true)

    expect(
      forkPage.links[0].targetPage.links[0].targetPage.invocation.node,
    ).toBe(firstRecursive)
    expect(
      forkPage.links[1].targetPage.links[0].targetPage.invocation.node,
    ).toBe(secondRecursive)
  })
})

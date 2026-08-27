/**
 * DOM helpers for specs, kept apart from `test/util.ts` because that one
 * reaches for `node:process` and so cannot be imported by a browser test.
 */

/**
 * Narrows away a query's null.
 *
 * `expect(x).not.toBeNull()` reads well but does not narrow, so a spec that
 * asserts and then uses the value still needs a `!` or a cast. This throws
 * instead, and names what was missing when it does.
 */
export function required<T>(v: T | null | undefined, what: string): T {
  if (v === null || v === undefined) {
    throw new Error(`expected ${what}`)
  }
  return v
}

/** The element with `id`, or a failure naming it. */
export function byId(id: string): HTMLElement {
  return required(document.getElementById(id), `an element with id "${id}"`)
}

/** The first descendant of `root` matching `selector`, or a failure naming it. */
export function query(root: ParentNode, selector: string): HTMLElement {
  return required(
    root.querySelector<HTMLElement>(selector),
    `an element matching ${selector}`,
  )
}

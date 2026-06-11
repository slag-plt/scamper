export function catchIf<T>(
  fn: () => T,
  predicate: (err: unknown) => boolean,
): T | undefined {
  try {
    return fn()
  } catch (e) {
    if (predicate(e)) throw e
    return undefined
  }
}

// TODO: we have 3 different version of this, we should consolidate and use this one
const IS_TAGGED = Symbol("##SCAMPER_TAGGED##")

export interface Tagged<Sym extends symbol, T = unknown> {
  readonly [IS_TAGGED]: Sym
  readonly value: T
}

export function makeTagged<Sym extends symbol, T>(
  sym: Sym,
  value: T,
): Tagged<Sym, T> {
  return { [IS_TAGGED]: sym, value }
}

export function isTagged(val: unknown): val is Tagged<symbol> {
  return typeof val === "object" && val !== null && IS_TAGGED in val
}

export function hasTag<Sym extends symbol>(
  val: unknown,
  sym: Sym,
): val is Tagged<Sym> {
  return isTagged(val) && val[IS_TAGGED] === sym
}

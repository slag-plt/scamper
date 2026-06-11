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

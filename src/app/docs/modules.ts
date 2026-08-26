import { docRegistry } from '../../lib'
import type { FunctionDoc } from '../../scheme/docstring/docstring'
import { functionDocName } from '../../scheme/docstring/render'

/**
 * The modules the docs cover, in presentation order (prelude first as the
 * default and most common). "runtime" is LPM-internal plumbing, not
 * user-facing, so it is deliberately excluded.
 */
export const moduleOrder = [
  'prelude', 'image', 'lab', 'music', 'test', 'gradescope',
  'audio', 'canvas', 'html', 'reactive', 'data', 'rex', 'file',
]

/** A documented function, together with the module it came from. */
export interface LibEntry {
  module: string
  doc: FunctionDoc
}

/**
 * A stable id for an entry. Six names -- `square`, `html?`, `button?` and
 * friends -- are exported by more than one module, so the module has to be
 * part of the id for anchors and list keys to stay unique.
 */
export function entryId(entry: LibEntry): string {
  return `${entry.module}-${functionDocName(entry.doc)}`
}

/** Every documented function across every module, in module order. */
export function allEntries(): LibEntry[] {
  return moduleOrder.flatMap((module) =>
    [...(docRegistry.get(module)?.values() ?? [])].map((doc) => ({
      module,
      doc,
    })),
  )
}

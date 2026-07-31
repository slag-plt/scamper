import fs from 'fs'
import path from 'node:path'
import { parseArgs } from 'node:util'

import type { ScamperDiagnostic } from '../../scheme/diagnostic'
import { ConsoleOutput } from '../../lpm/output'
import { compile } from '../../scheme'
import { setFS } from '../../fs'
import NodeFileSystem from '../../fs/node'
import Scamper, { initialize } from '../../scamper'

////////////////////////////////////////////////////////////////////////////////

const { values, positionals } = parseArgs({
  options: {
    help: {
      type: 'boolean',
      short: '?',
      default: false,
    },
    trace: {
      type: 'boolean',
    },
    check: {
      type: 'boolean',
      default: false,
    },
    // Define other options as needed
  },
  allowPositionals: true, // Set to true if your CLI accepts positional arguments
})

if (values.help || positionals.length !== 1) {
  console.log('Usage: scamper [options] filename')
  console.log('Options:')
  console.log('  -?, --help       Show this help message')
  console.log('  --trace          Enable step-by-step tracing')
  console.log(
    '  --check          Scope-check the program and report diagnostics without running it',
  )
  process.exit(0)
}

const filename = positionals[0]

const src = fs.readFileSync(filename, 'utf-8')

// Wire a Node-backed file system rooted at the checked file's directory so
// scope checking and execution can resolve the program's sibling file imports
// (getFS() is otherwise uninitialized outside the browser -- see #287).
setFS(await NodeFileSystem.create(path.dirname(path.resolve(filename))))

await initialize()

// --check: run the optional scope-check pass, report diagnostics, and exit
// without running. Exit code is non-zero if any diagnostic was reported.
if (values.check) {
  let diagnostics: ScamperDiagnostic[]
  try {
    ;({ diagnostics } = await compile(src, { scopeCheck: true }))
  } catch (e) {
    console.error(
      `${filename}: check failed: ${e instanceof Error ? e.message : String(e)}`,
    )
    process.exit(1)
  }
  for (const d of diagnostics) {
    console.error(formatDiagnostic(filename, d))
  }
  process.exit(diagnostics.length > 0 ? 1 : 0)
}

const out = new ConsoleOutput()

const request = await Scamper.getInstance().execute({
  src, out, err: out, isTracing: values.trace ?? false
})

if (request === null) {
  process.exit(1)
}

await request.done
process.exit(out.seenError ? 1 : 0)

/** Formats a diagnostic as `file:line:col: severity [phase]: message`. */
function formatDiagnostic(file: string, d: ScamperDiagnostic): string {
  const loc = d.range
    ? `:${d.range.begin.line.toString()}:${d.range.begin.col.toString()}`
    : ''
  return `${file}${loc}: ${d.severity} [${d.phase}]: ${d.message}`
}

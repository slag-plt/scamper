import fs from 'fs'
import { parseArgs } from 'node:util'

import { builtinLibs, initializeLibs } from '../lib'
import * as LPM from '../lpm'
import { ConsoleOutput } from '../lpm/output'
import { Thread } from '../lpm/thread.js'
import * as Scheme from '../scheme'

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
    // Define other options as needed
  },
  allowPositionals: true, // Set to true if your CLI accepts positional arguments
});

if (values.help || positionals.length !== 1) {
  console.log('Usage: scamper [options] filename')
  console.log('Options:')
  console.log('  -?, --help       Show this help message')
  console.log('  --trace          Enabling step-by-step tracing')
  process.exit(0)
}

const filename = positionals[0]

const src = fs.readFileSync(filename, 'utf-8');
const out = new ConsoleOutput()
const program = Scheme.compile(out, src)
if (program === undefined) { process.exit(1) }

const options = LPM.defaultOptions
options.isTracing = values.trace ?? false

await initializeLibs()
new Thread(
  '##main##',
  Scheme.mkInitialEnv(), 
  program,
  options,
  builtinLibs,
  out,
  out,
  new Map([['scheme', Scheme.raiser]])).evaluate()
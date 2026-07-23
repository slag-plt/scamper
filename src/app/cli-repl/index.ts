import fs from 'fs'
import { parseArgs } from 'node:util'

import { ConsoleOutput } from '../../lpm/output'
import Scamper, { initialize, getDefaultEnv } from '../../scamper'

import readline from 'node:readline'
import { stdin, stdout } from 'node:process'

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
})

var env;

// Needs updating for REPL
if (values.help) {
  console.log('Usage: npm cli-repl [options]')
  console.log('Options:')
  console.log('  -?, --help       Show this help message')
  process.exit(0)
}

async function exec(scamper : Scamper, out: ConsoleOutput, code : string): Promise<void> {
  // console.log('[' + code + ']')
  const request = await scamper.execute(
    {src: code, out: out, err: out, isTracing: false}, env
  )
  if (request) {
    await request.done
  }
  // console.log(request)
  env = request.fiber.topLevelEnv
  stdout.write(">>> ")
} // exec

async function main(): Promise<void> {
  
  const rl = readline.createInterface({ input: stdin, output: stdout})

  await initialize()

  const out = new ConsoleOutput()
  const scamper = await Scamper.getInstance();
  env = getDefaultEnv()

  var done = false;

  rl.setPrompt(">>> ")
  stdout.write(">>> ")
  rl.on('line', (line) => {
    if (! line) {
      rl.close()
    } else {
      exec(scamper, out, line)
    }
  })

  // process.exit(out.seenError ? 1 : 0)
}

await main()

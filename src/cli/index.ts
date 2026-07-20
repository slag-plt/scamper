import fs from "fs"
import { parseArgs } from "node:util"

// import { builtinLibs, initializeLibs } from "../js"
import { ConsoleOutput } from "../lpm/output"
import Scamper from "../scamper"

////////////////////////////////////////////////////////////////////////////////

const { values, positionals } = parseArgs({
  options: {
    help: {
      type: "boolean",
      short: "?",
      default: false,
    },
    trace: {
      type: "boolean",
    },
    // Define other options as needed
  },
  allowPositionals: true, // Set to true if your CLI accepts positional arguments
})

if (values.help || positionals.length !== 1) {
  console.log("Usage: scamper [options] filename")
  console.log("Options:")
  console.log("  -?, --help       Show this help message")
  console.log("  --trace          Enabling step-by-step tracing")
  process.exit(0)
}

const filename = positionals[0]

const src = fs.readFileSync(filename, "utf-8")
const out = new ConsoleOutput()

const request = await Scamper.getInstance().execute({
  src, out, err: out, isTracing: values.trace ?? false
})

if (request === null) {
  process.exit(1)
}

await request.done
process.exit(out.seenError ? 1 : 0)
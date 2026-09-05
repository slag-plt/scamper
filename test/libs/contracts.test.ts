// Every contracted library binding, applied once from Scheme.
//
// A binding's contract wrapper is only ever built when the binding is applied
// *from Scheme*: a test that calls the native directly bypasses it, and a call
// made from library code deliberately skips the checks (#476/#488). So a
// binding can be thoroughly tested and still have its contract never run --
// which is how #487, #467 and #491 all reached main. This is the sweep that
// closes that gap: one Scheme call per contracted binding, with an argument
// tuple built from the binding's own docstring (see contract-samples.ts).
//
// Two tiers, and the second is the one that earns its keep:
//
//  1. the contract accepted its own documented types -- the run reports no
//     contract or arity error. No exceptions are permitted: a binding either
//     passes or is on SKIP.
//  2. the call completed -- the run reports no error at all. This is what
//     catches #487: a docstring naming the wrong type hands the native a value
//     its own contract is happy with, so tier 1 passes and the native
//     explodes. Tier 2 needs ENVIRONMENTAL for the calls that cannot finish in
//     jsdom.
//
// What it does *not* catch is #467's shape -- a signature that drops a
// parameter the native takes. Such a call is well-typed in every argument it
// does pass, so both tiers are happy; that is #496's job, and the two are
// complementary.
import { beforeAll, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { expToString } from '../../src/scheme/ast'
import { MockFileSystem } from '../stubs/mock-file-system'
import {
  ARGS,
  HELPERS,
  MODULE_SAMPLES,
  SAMPLES,
  SAMPLE_FILE,
  SAMPLE_FILE_CONTENTS,
} from './contract-samples'
import { DocumentedBinding, contractedBindings } from './library-bindings'
import { runProgram } from './harness.js'

/**
 * Bindings whose contract cannot be reached from a test at all, so they are
 * not called. This list is the honest successor to the "18% never run" figure
 * the sweep was written to retire: every contract that has still never
 * executed is named here, with the reason it cannot.
 */
const SKIP = new Map<string, string>([
  // audio_contextQ is `typeof AudioContext !== "undefined" && v instanceof
  // AudioContext` (src/js/audio/index.ts), and jsdom has no Web Audio -- so
  // under this environment *no* value satisfies `context?` and the contract
  // can only ever be reached by failing it.
  ['audio:audio-pipeline', 'ctx : context? is unsatisfiable without Web Audio'],
  ['audio:oscillator-node', 'ctx : context? is unsatisfiable without Web Audio'],
  ['audio:audio-file-node', 'ctx : context? is unsatisfiable without Web Audio'],
  ['audio:delay-node', 'ctx : context? is unsatisfiable without Web Audio'],
  // Each suspends the fiber on something outside the program: a file chooser
  // the student has to answer, or a network fetch.
  ['prelude:with-file', 'suspends on a file chooser the test cannot answer'],
  ['prelude:with-file-chooser', 'suspends on a file chooser the test cannot answer'],
  ['image:image-load', 'suspends on a real image decode'],
  ['image:with-image-from-url', 'suspends on a network fetch'],
  // Writes an actual file through getFS().saveBytes (src/js/image/image.ts).
  ['image:image-save!', 'writes a real file'],
  // Starts a requestAnimationFrame loop that would outlive the test.
  ['canvas:animate-with', 'starts an animation loop that outlives the test'],
])

/**
 * Bindings whose contract runs and passes, but whose native cannot finish in
 * this environment. Tier 1 still applies to every one of them; only tier 2 is
 * waived.
 */
const ENVIRONMENTAL = new Map<string, string>([
  ['audio:audio-context', 'constructs an AudioContext, which jsdom has not'],
  ['audio:play-sample', 'constructs an AudioContext, which jsdom has not'],
  ['music:load-instrument', 'requireWaf() needs a browser with Web Audio'],
  ['music:load-percussion', 'requireWaf() needs a browser with Web Audio'],
  ['music:use-high-quality-instruments', 'requireWaf() needs a browser with Web Audio'],
  ['music:play-composition', 'requireWaf() needs a browser with Web Audio'],
])

/** Bindings that fail on a real bug, each with the issue tracking it. */
const KNOWN_BROKEN = new Map<string, string>([
  // `cons` documents `v2 : any`, but mkCons (src/lpm/util.ts) requires a list.
  // The docstring is what is wrong, but correcting it changes the message a
  // student sees for `(cons 1 2)` -- and displaces the fixture
  // test/regressions/contract-error-call-site.test.ts uses as its example of
  // an error raised *outside* a contract wrapper -- so it belongs on its own
  // diff.
  ['prelude:cons', 'v2 : any, but cons requires a list (#541)'],
])

/**
 * Bindings whose contract admits a zero-rest-argument call but whose native
 * does not survive one -- #492's shape. Separate from the maps above so it can
 * be tuned without weakening them.
 */
const ZERO_REST_BROKEN = new Map<string, string>([
  ['prelude:-', 'leaks a raw JS TypeError on no arguments (#517)'],
  ['prelude:/', 'leaks a raw JS TypeError on no arguments (#517)'],
  ['prelude:range', '& args admits no arguments; the native needs 1-3 (#542)'],
  [
    'prelude:vector-range',
    '& args admits no arguments; the native needs 1-3 (#542)',
  ],
  // Being fixed in #496, which corrects these four signatures' arity.
  ['prelude:any-of', 'signature says & f1, the value requires one (#496)'],
  ['prelude:all-of', 'signature says & f1, the value requires one (#496)'],
  ['prelude:compose', 'signature says & f1, the value requires one (#496)'],
  ['prelude:o', 'signature says & f, the value requires one (#496)'],
])

/***** Reading a run's output *************************************************/

// runProgram returns displayed values and reported errors in one list, so an
// error is recognised by the shape ScamperError.toString() gives it: the phase,
// then " error", then an optional bracketed location (dropped here, since the
// libs harness strips ranges), then the message.
const ERROR_LINE = /^(?:Parser|Runtime|Docstring) error/

// A contract violation goes through ##error## (runtime_error), which pins its
// source to "error"; a native's own error carries the *function's* name as its
// source instead. So the "(error)" prefix is what tells the two apart, and
// these two patterns are the exact messages contract.ts generates.
const CONTRACT_VIOLATION = /^Runtime error: \(error\) expected .+, received .+/
const REST_VIOLATION =
  /^Runtime error: \(error\) expected every value of .+ to be .+, but at least one was not/
// From runtime_checkArity ("at most N") and the closure arity check in
// op-handlers.ts (exactly N).
const ARITY_VIOLATION =
  /Arity mismatch in function call: expected (?:at most )?\d+ arguments?, got \d+/

const isContractFailure = (line: string): boolean =>
  CONTRACT_VIOLATION.test(line) ||
  REST_VIOLATION.test(line) ||
  ARITY_VIOLATION.test(line)

/***** Building one binding's program *****************************************/

/** A binding's key in every table here: `<module>:<name>`. */
const key = (b: DocumentedBinding): string => `${b.module}:${b.name}`

/**
 * `table[k]`, when the table actually has that key. An index signature types
 * every lookup as present, which it is not -- and a bare `table[k]` would
 * reach Object.prototype's own members for a predicate spelled `constructor`.
 */
function lookup<T>(table: Record<string, T>, k: string): T | undefined {
  return Object.hasOwn(table, k) ? table[k] : undefined
}

/** The sample value for `predicate` in `module`, or undefined if there is none. */
function sampleFor(module: string, predicate: string): string | undefined {
  const overrides = lookup(MODULE_SAMPLES, module)
  const perModule =
    overrides === undefined ? undefined : lookup(overrides, predicate)
  return perModule ?? lookup(SAMPLES, predicate)
}

/** A binding's parameters in call order: fixed, then optional, then rest. */
function allParams(b: DocumentedBinding) {
  return [
    ...b.doc.params,
    ...b.doc.optParams,
    ...(b.doc.restParam ? [b.doc.restParam] : []),
  ]
}

/**
 * The arguments to apply `b` to: its own override if it has one, else one
 * sample per documented parameter. `missing` names the predicate that has no
 * sample, which is a hole in contract-samples.ts rather than a library fault.
 */
function argsFor(b: DocumentedBinding): { args?: string[]; missing?: string } {
  const override = ARGS.get(key(b))
  if (override !== undefined) {
    return { args: override }
  }
  const args: string[] = []
  for (const p of allParams(b)) {
    const pred = expToString(p.predicate)
    const sample = sampleFor(b.module, pred)
    if (sample === undefined) {
      return { missing: pred }
    }
    args.push(sample)
  }
  return { args }
}

/**
 * The program that applies `b` to `args`. Helper modules are imported first so
 * that the module under test wins the name race on anything both export; the
 * prelude needs no import, being in scope from the start.
 */
function mkProgram(b: DocumentedBinding, args: string[]): string {
  const modules = [...(lookup(HELPERS, b.module) ?? []), b.module].filter(
    (m) => m !== 'prelude',
  )
  return [
    ...modules.map((m) => `(import ${m})`),
    `(${[b.name, ...args].join(' ')})`,
  ].join('\n')
}

/** One binding's call: the program that made it and the errors it reported. */
interface Attempt {
  binding: DocumentedBinding
  program: string
  errors: string[]
}

async function attempt(b: DocumentedBinding, args: string[]): Promise<Attempt> {
  const program = mkProgram(b, args)
  const log = await runProgram(program)
  return { binding: b, program, errors: log.filter((l) => ERROR_LINE.test(l)) }
}

/**
 * The failure as the report shows it: the exact program that ran, so a wrong
 * sample in contract-samples.ts can be told from a real library fault at a
 * glance.
 */
function describeFailure(a: Attempt): string {
  return `${key(a.binding)}\n${a.program}\n--> ${a.errors.join('\n--> ')}`
}

/***** The sweep **************************************************************/

const BINDINGS = contractedBindings()
const MODULES = [...new Set(BINDINGS.map((b) => b.module))]

// A generated program that runs 400-odd library calls end to end takes rather
// longer than a unit test's default budget.
const TIMEOUT = 120_000

beforeAll(async () => {
  // The `file` library reads and writes through the configured backend; an
  // in-memory one keeps this sweep from touching the real filesystem, and
  // gives its readers something that is a file rather than merely a string.
  const fs = await MockFileSystem.create()
  await fs.saveFile(SAMPLE_FILE, SAMPLE_FILE_CONTENTS)
  setBackend(localBackend(fs))
})

test('every documented parameter predicate has a sample value', () => {
  const missing = BINDINGS.filter((b) => !SKIP.has(key(b))).flatMap((b) => {
    const { missing } = argsFor(b)
    return missing === undefined ? [] : [`${key(b)}: ${missing}`]
  })
  expect(
    missing,
    'add the predicate to SAMPLES (or the binding to ARGS) in contract-samples.ts',
  ).toEqual([])
})

/** Every attempt made, keyed by `<module>:<name>`, for the staleness checks. */
const attempts = new Map<string, Attempt>()
const zeroRestAttempts = new Map<string, Attempt>()

describe.each(MODULES)('%s contracts', (module) => {
  const bindings = BINDINGS.filter(
    (b) => b.module === module && !SKIP.has(key(b)),
  )
  let runs: Attempt[] = []

  beforeAll(async () => {
    runs = []
    for (const b of bindings) {
      const { args } = argsFor(b)
      if (args === undefined) {
        continue // reported by the sample-coverage test above
      }
      const a = await attempt(b, args)
      attempts.set(key(b), a)
      runs.push(a)
    }
  }, TIMEOUT)

  test('every contract accepts its own documented types', () => {
    expect(
      runs
        .filter((a) => a.errors.some(isContractFailure))
        .map(describeFailure),
      'the contract rejected a value its own docstring says it takes',
    ).toEqual([])
  })

  test('every call completes on its own documented types', () => {
    expect(
      runs
        .filter(
          (a) =>
            a.errors.length > 0 &&
            !ENVIRONMENTAL.has(key(a.binding)) &&
            !KNOWN_BROKEN.has(key(a.binding)),
        )
        .map(describeFailure),
      'the contract passed but the call did not finish -- #487\'s shape',
    ).toEqual([])
  })
})

/***** Zero rest arguments ****************************************************/

// A rest parameter's contract is satisfied vacuously by *no* arguments, so
// `(string)` and `(append)` are calls the library says it accepts. #492 is what
// happens when it does not: the first leaked a raw TypeError, the second
// returned void. Kept apart from the sweep above, with its own exception list,
// because "the documented types work" and "the empty case works" are two
// different claims about a binding.
describe('a rest parameter admits zero arguments', () => {
  const bindings = BINDINGS.filter(
    (b) => b.doc.restParam !== undefined && !SKIP.has(key(b)),
  )
  let runs: Attempt[] = []

  beforeAll(async () => {
    runs = []
    for (const b of bindings) {
      const { args } = argsFor(b)
      if (args === undefined) {
        continue
      }
      // The tuple is positional and the rest argument is its last entry, so
      // dropping that entry is the same call with nothing for the rest
      // parameter to collect.
      const a = await attempt(b, args.slice(0, -1))
      zeroRestAttempts.set(key(b), a)
      runs.push(a)
    }
  }, TIMEOUT)

  test('and the call survives it', () => {
    expect(
      runs
        .filter(
          (a) =>
            a.errors.length > 0 &&
            !ENVIRONMENTAL.has(key(a.binding)) &&
            !ZERO_REST_BROKEN.has(key(a.binding)),
        )
        .map(describeFailure),
      'the contract admits the empty call but the native does not -- #492\'s shape',
    ).toEqual([])
  })
})

/***** The exception lists retire themselves **********************************/

describe('the exception lists are still needed', () => {
  const contracted = new Set(BINDINGS.map(key))

  test.each([
    ['SKIP', SKIP],
    ['ENVIRONMENTAL', ENVIRONMENTAL],
    ['KNOWN_BROKEN', KNOWN_BROKEN],
    ['ZERO_REST_BROKEN', ZERO_REST_BROKEN],
    ['ARGS', ARGS],
  ] as [string, Map<string, unknown>][])(
    'every %s entry names a contracted binding',
    (_name, m) => {
      expect([...m.keys()].filter((k) => !contracted.has(k))).toEqual([])
    },
  )

  // An entry that no longer produces an error has been fixed; one that produced
  // no attempt at all is doing no work either, so both retire.
  const stillFailing = (runs: Map<string, Attempt>, k: string): boolean =>
    (runs.get(k)?.errors ?? []).length > 0

  test('every ENVIRONMENTAL and KNOWN_BROKEN entry still fails', () => {
    const stale = [...ENVIRONMENTAL.keys(), ...KNOWN_BROKEN.keys()].filter(
      (k) => !stillFailing(attempts, k),
    )
    expect(stale, 'these now run cleanly -- drop them from the list').toEqual([])
  })

  test('every ZERO_REST_BROKEN entry still fails', () => {
    const stale = [...ZERO_REST_BROKEN.keys()].filter(
      (k) => !stillFailing(zeroRestAttempts, k),
    )
    expect(stale, 'these now run cleanly -- drop them from the list').toEqual([])
  })
})

import { describe, expect, test } from 'vitest'
import { docRegistry } from '../../src/lib'
import { expToString } from '../../src/scheme/ast'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/589
//
// Fourteen parameters in src/lib/data.scm declared `: any` while the
// refinement line directly under each named a real type ("list of numbers",
// "list of datasets"). Contracts are generated from the *declared* predicate
// alone -- the refinement line is free prose (Param.description) and is never
// checked -- so the looser half won and the value reached the native, where
// `listToVector` walked it as a list:
//
//   Runtime error: Unexpected error in Javascript function call:
//   TypeError: Cannot read properties of undefined (reading 'head')
//
// naming `head`, an internal field of the list representation that a student
// never wrote. Same shape as #531 (sample-node), one library over.
//
// Ranges are stripped -- they point at the student's own call, which says
// nothing these tests are about.

/**
 * A wrong argument, and the contract error it should now produce. One per
 * family: the two `with-*-options` takers of an association list, a fixed
 * `labels` list, the variadic `plot-*` rest parameter, and the `dataset-*`
 * data list.
 */
const REJECTED: [label: string, program: string, message: string][] = [
  // The issue's own program.
  [
    'dataset-bar given a number',
    '(dataset-bar "t" 5)',
    'expected a list as the second argument, received number',
  ],
  [
    'dataset-line given a string',
    '(dataset-line "t" "nope")',
    'expected a list as the second argument, received string',
  ],
  [
    'dataset-bubble given a vector',
    '(dataset-bubble "t" (vector 1 2 3))',
    'expected a list as the second argument, received vector',
  ],
  [
    'dataset-pie given a boolean',
    '(dataset-pie "t" #t)',
    'expected a list as the second argument, received boolean',
  ],
  // The rest parameter is checked per *argument*, not against the collected
  // list, so its predicate is the element type (`dataset?`) -- as rex-concat's
  // `xs : rex?` and beside's `d1 : drawing?` already are.
  //
  // These two are the quiet half of the bug: rather than raising anything,
  // `datasets.map(ds => ds.opts)` read `.opts` off a number and produced a
  // *plot* whose datasets were `void`, which then rendered as an empty chart.
  // A silently wrong picture is worse than the Javascript error the others got.
  [
    'plot-linear given a non-dataset',
    '(plot-linear 5)',
    'expected every value of datasets to be a dataset, but at least one was not',
  ],
  [
    'plot-radial given a non-dataset',
    '(plot-radial (list "a") "not-a-dataset")',
    'expected every value of datasets to be a dataset, but at least one was not',
  ],
  // A fixed parameter ahead of a rest parameter is checked first.
  [
    'plot-category given non-list labels',
    '(plot-category 5 (dataset-bar "c" (list 1 2)))',
    'expected a list as the first argument, received number',
  ],
  [
    'with-plot-options given non-list options',
    '(with-plot-options 5 (plot-linear (dataset-line "xs" (list (pair 1 1)))))',
    'expected a list as the first argument, received number',
  ],
  [
    'with-dataset-options given non-list options',
    '(with-dataset-options "border-color" (dataset-bar "c" (list 1 2)))',
    'expected a list as the first argument, received string',
  ],
]

describe('#589: data.scm turns away a wrong argument through its contract', () => {
  test.each(REJECTED)('%s', async (_label, program, message) => {
    expect(
      await runProgram(`(import data)\n${program}`, { stripRanges: true }),
    ).toEqual([`Runtime error: (error) ${message}`])
  })

  test('no raw Javascript error reaches the student', async () => {
    for (const [, program] of REJECTED) {
      const out = (
        await runProgram(`(import data)\n${program}`, { stripRanges: true })
      ).join('\n')
      expect(out).not.toContain('TypeError')
      expect(out).not.toContain('Unexpected error in Javascript function call')
    }
  })
})

// Behaviour alone cannot see a docstring that has stopped parsing: the binding
// would simply lose its contract and every assertion above would start passing
// for the wrong reason (see #531's note in
// sample-node-vector-contract.test.ts). So assert the declared types too.
describe('#589: every data.scm parameter whose prose names a type declares it', () => {
  /** binding -> [fixed param predicates, rest param predicate or undefined]. */
  const DECLARED: [string, string[], string | undefined][] = [
    ['with-plot-options', ['list?', 'plot?'], undefined],
    ['with-dataset-options', ['list?', 'dataset?'], undefined],
    ['plot-linear', [], 'dataset?'],
    ['plot-category', ['list?'], 'dataset?'],
    ['plot-radial', ['list?'], 'dataset?'],
    ['dataset-line', ['string?', 'list?'], undefined],
    ['dataset-bar', ['string?', 'list?'], undefined],
    ['dataset-scatter', ['string?', 'list?'], undefined],
    ['dataset-bubble', ['string?', 'list?'], undefined],
    ['dataset-pie', ['string?', 'list?'], undefined],
    ['dataset-polar', ['string?', 'list?'], undefined],
    ['dataset-radar', ['string?', 'list?'], undefined],
  ]

  test.each(DECLARED)('%s', (name, params, rest) => {
    const doc = docRegistry.get('data')?.get(name)
    expect(doc).toBeDefined()
    expect(doc?.params.map((p) => expToString(p.predicate))).toEqual(params)
    expect(
      doc?.restParam && expToString(doc.restParam.predicate),
    ).toEqual(rest)
  })

  // The converse, so the sweep cannot be "completed" by tightening the two
  // parameters that are genuinely unconstrained: a predicate's argument is any
  // value at all, which is why neither carries a refinement line to disagree
  // with.
  test.each(['dataset?', 'plot?'])('%s takes any value, by design', (name) => {
    const doc = docRegistry.get('data')?.get(name)
    expect(doc?.params.map((p) => expToString(p.predicate))).toEqual(['any'])
    expect(doc?.params[0].description).toBeUndefined()
  })
})

describe('#589: what data.scm already accepted, it still accepts', () => {
  test('a categorical plot still builds from labels and datasets', async () => {
    expect(
      await runProgram(
        `(import data)
         (plot? (plot-category (list "a" "b")
                               (dataset-bar "counts" (list 1 2))
                               (dataset-line "trend" (list 1 2))))`,
        { stripRanges: true },
      ),
    ).toEqual(['#t'])
  })

  test('options still apply to a plot and to a dataset', async () => {
    expect(
      await runProgram(
        `(import data)
         (plot? (with-plot-options (list (pair "x-min" 0))
                                   (plot-linear (dataset-line "xs" (list (pair 1 1))))))
         (dataset? (with-dataset-options (list (pair "border-color" "red"))
                                         (dataset-bar "counts" (list 1 2))))`,
        { stripRanges: true },
      ),
    ).toEqual(['#t', '#t'])
  })

  test('the empty list is still a list, and the native still rejects it', async () => {
    // `null` is a list, so the contract passes it through to the native's own
    // "at least one data point" check -- which is the error the student wants
    // here, not a type error.
    expect(
      await runProgram('(import data)\n(dataset-bar "t" (list))', {
        stripRanges: true,
      }),
    ).toEqual([
      'Runtime error: (dataset-bar) dataset-bar requires at least one data point',
    ])
  })
})

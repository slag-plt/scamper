import { afterEach, describe, expect, test } from 'vitest'
import { initialize } from '../../../src/scamper'
import { useExampleChecks } from '../../../src/app/web/composables/use-example-checks'
import { setCheckExamples } from '../../../src/app/web/run-prefs'
import type { ExampleOutcome } from '../../../src/scheme/examples'

await initialize()
// Importing scamper.ts kicks off its renderer registration as a fire-and-forget
// module-load side effect; settle it here so it cannot land after teardown.
await import('../../../src/app/web/renderers.js')

/** A documented `fact`, with whatever example lines the test wants. */
function documented(body: string, ...examples: string[]): string {
  return [
    ';;; (fact n) -> number?',
    ';;;   n : number?',
    ';;; Returns n factorial.',
    ...examples.map((e) => `;;; @example ${e}`),
    `(define fact (lambda (n) ${body}))`,
  ].join('\n')
}

const realFactorial = '(if (zero? n) 1 (* n (fact (- n 1))))'

async function check(src: string, limitMs?: number): Promise<readonly ExampleOutcome[]> {
  const checks = useExampleChecks(limitMs === undefined ? {} : { limitMs })
  await checks.runChecks(src)
  return checks.outcomes.value
}

describe('example checks', () => {
  afterEach(() => {
    setCheckExamples(true)
  })

  test('passes an example the code agrees with', async () => {
    const outcomes = await check(
      documented(realFactorial, '(fact 5) -> 120'),
    )
    expect(outcomes).toHaveLength(1)
    expect(outcomes[0].status).toBe('pass')
    expect(outcomes[0].range.begin.line).toBe(4)
  })

  test('passes an example whose expected side is not a literal', async () => {
    const src = [
      ';;; (upto n) -> list?',
      ';;;   n : number?',
      ';;; Counts up to n.',
      ';;; @example (upto 3) -> (list 1 2 3)',
      '(define upto (lambda (n) (range 1 (+ n 1) 1)))',
    ].join('\n')
    const outcomes = await check(src)
    expect(outcomes[0].status).toBe('pass')
  })

  test('fails an example the code disagrees with, keeping both values', async () => {
    const outcomes = await check(
      documented('(* n 2)', '(fact 5) -> 120'),
    )
    expect(outcomes[0].status).toBe('fail')
    expect(outcomes[0].actual).toBe(10)
    expect(outcomes[0].expected).toBe(120)
  })

  test('reports an example that raises as an error', async () => {
    const outcomes = await check(
      documented(realFactorial, '(fact "five") -> 120'),
    )
    expect(outcomes[0].status).toBe('error')
    expect(outcomes[0].message).toBeTruthy()
  })

  test('stops an example that never finishes', async () => {
    const src = [
      ';;; (loop) -> any?',
      ';;; Never finishes.',
      ';;; @example (loop) -> 1',
      '(define loop (lambda () (loop)))',
    ].join('\n')
    const outcomes = await check(src, 50)
    expect(outcomes[0].status).toBe('timeout')
  })

  test('checks every example, even after one has failed', async () => {
    const outcomes = await check(
      documented(realFactorial, '(fact 5) -> 121', '(fact 0) -> 1'),
    )
    expect(outcomes.map((o) => o.status)).toStrictEqual(['fail', 'pass'])
  })

  test('finds nothing in a file with no examples', async () => {
    expect(await check('(define x 1)')).toStrictEqual([])
  })

  test('checks nothing while the preference is off', async () => {
    setCheckExamples(false)
    expect(await check(documented(realFactorial, '(fact 5) -> 120'))).toStrictEqual([])
  })

  test('cancel drops the marks', async () => {
    const checks = useExampleChecks()
    await checks.runChecks(documented(realFactorial, '(fact 5) -> 120'))
    expect(checks.outcomes.value).toHaveLength(1)
    checks.cancel()
    expect(checks.outcomes.value).toStrictEqual([])
  })

  test('a sweep superseded by a newer one publishes nothing', async () => {
    const checks = useExampleChecks()
    const sweep = checks.runChecks(
      documented(realFactorial, '(fact 5) -> 120'),
    )
    checks.cancel()
    await sweep
    expect(checks.outcomes.value).toStrictEqual([])
  })
})

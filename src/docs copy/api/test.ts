import { ArgDoc, Doc } from './docs.js'

export const testOk: Doc = new Doc(
  'test-ok',
  'test-result?',
  [ new ArgDoc('desc', 'string?') ],
  'Returns a test result indicating that the test passed.'
)

export const testError: Doc = new Doc(
  'test-error',
  'test-result?',
  [ new ArgDoc('desc', 'string?'), new ArgDoc('reason', 'string?') ],
  'Returns a test result indicating that the test failed.'
)

export const testCase: Doc = new Doc(
  'test-case',
  'test-result?',
  [
    new ArgDoc('desc', 'string?'),
    new ArgDoc('eq?', 'function?, a function that tests for equality between two values'),
    new ArgDoc('expected', 'any?'),
    new ArgDoc('test-fn', 'function?, a function that produces the actual value to be tested')
  ],
  'Returns a test result indicating whether the given equality test passed: `(eq? expected (test-fn))`.'
)

export const testExn: Doc = new Doc(
  'test-exn',
  'test-result?',
  [
    new ArgDoc('desc', 'string?'),
    new ArgDoc('test-fn', 'function?, a function that should throw an exception')
  ],
  'Returns a test result indicating whether the given function threw an exception.'
)
import Doc from './docs.js'

export const testOk: Doc = new Doc(
  '(test-ok desc): test-result?', [
    'desc: string?'
  ],
  'Returns a test result indicating that the test passed.'
)

export const testError: Doc = new Doc(
  '(test-error desc reason): test-result?', [
    'desc: string?',
    'reason: string?'
  ],
  'Returns a test result indicating that the test failed.'
)

export const testCase: Doc = new Doc(
  '(test-case desc eq? expected test-fn): test-result?', [
    'desc: string?',
    'eq?: function?, a function that tests for equality between two values',
    'expected: any?',
    'test-fn: function?, a function that produces the actual value to be tested'
  ],
  'Returns a test result indicating whether the given equality test passed: `(eq? expected (test-fn))`.'
)

export const testExn: Doc = new Doc(
  '(test-exn desc test-fn): test-result?', [
    'desc: string?',
    'test-fn: function?, a function that should throw an exception'
  ],
  'Returns a test result indicating whether the given function threw an exception.'
)
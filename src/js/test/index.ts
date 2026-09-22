import * as L from '../../lpm'
import './renderers/text.js'

export type Result = Ok | ErrExp | ErrExn | ErrGen
export interface Ok extends L.Struct { [L.structKind]: 'test-result-ok', desc: string }
export interface ErrExp extends L.Struct { [L.structKind]: 'test-result-error-expected', desc: string, expected: L.Value, actual: L.Value }
export interface ErrExn extends L.Struct { [L.structKind]: 'test-result-error-exn', desc: string, exn: L.Value }
export interface ErrGen extends L.Struct { [L.structKind]: 'test-result-error-gen', desc: string, reason: string }

// N.B., each constructor's contract narrows `desc` (and `reason`) to a string,
// and each re-narrows it here because `test-case` and `test-exn` in test.scm
// name them at top level, which reaches the native without the contract
// (#553). A non-string description otherwise reached the renderers, which
// print it as the test's name.

export function test_testResultOk(desc: L.Value): Ok {
  if (!L.isString(desc)) {
    throw new L.ScamperError('Runtime', 'test-result-ok: expected a string')
  }
  return { [L.scamperTag]: 'struct', [L.structKind]: 'test-result-ok', desc }
}

export function test_testResultErrorExpected(desc: L.Value, expected: L.Value, actual: L.Value): ErrExp {
  if (!L.isString(desc)) {
    throw new L.ScamperError('Runtime', 'test-result-error-expected: expected a string')
  }
  return { [L.scamperTag]: 'struct', [L.structKind]: 'test-result-error-expected', desc, expected, actual }
}

export function test_testResultErrorExn(desc: L.Value, exn: L.Value): ErrExn {
  if (!L.isString(desc)) {
    throw new L.ScamperError('Runtime', 'test-result-error-exn: expected a string')
  }
  return { [L.scamperTag]: 'struct', [L.structKind]: 'test-result-error-exn', desc, exn }
}

export function test_testResultErrorGeneric(desc: L.Value, reason: L.Value): ErrGen {
  if (!L.isString(desc) || !L.isString(reason)) {
    throw new L.ScamperError('Runtime', 'test-result-error-gen: expected strings')
  }
  return { [L.scamperTag]: 'struct', [L.structKind]: 'test-result-error-gen', desc, reason }
}

// N.B., test-case and test-exn moved to test.scm (built on the `with-handler`
// special form). A js-var procedure can no longer call the caller-supplied
// test/equality functions, since callScamperFn is disabled.

export function test_isResult (v: L.Value): boolean {
  return L.isStructKind(v, 'test-result-ok') || L.isStructKind(v, 'test-result-error-expected')
      || L.isStructKind(v, 'test-result-error-exn') || L.isStructKind(v, 'test-result-error-gen')
}

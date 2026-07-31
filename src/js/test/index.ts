import * as L from '../../lpm'
import './renderers/text.js'

export type Result = Ok | ErrExp | ErrExn | ErrGen
export interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
export interface ErrExp extends L.Struct { [L.structKind]: 'exp', desc: string, expected: L.Value, actual: L.Value }
export interface ErrExn extends L.Struct { [L.structKind]: 'exn', desc: string, exn: L.Value }
export interface ErrGen extends L.Struct { [L.structKind]: 'gen', desc: string, reason: string }

export function test_testResultOk(desc: string): Ok {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}

export function test_testResultErrorExpected(desc: string, expected: L.Value, actual: L.Value): ErrExp {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exp', desc, expected, actual }
}

export function test_testResultErrorExn(desc: string, exn: L.Value): ErrExn {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exn', desc, exn }
}

export function test_testResultErrorGeneric(desc: string, reason: string): ErrGen {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'gen', desc, reason }
}

// N.B., test-case and test-exn moved to test.scm (built on the `with-handler`
// special form). A js-var procedure can no longer call the caller-supplied
// test/equality functions, since callScamperFn is disabled.

export function test_isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'exp')
      || L.isStructKind(v, 'exn') || L.isStructKind(v, 'gen')
}

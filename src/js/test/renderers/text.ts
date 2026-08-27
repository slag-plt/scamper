import * as L from '../../../lpm'
import TextRenderer from '../../../lpm/renderers/text.js'
import { Result, test_isResult } from '../index.js'

TextRenderer.registerCustomRenderer(test_isResult, (v: L.Value) => {
  const result = v as Result
  switch (result[L.structKind]) {
    case 'test-result-ok':
      return `Test "${result.desc}"\n✅ Passed!`
    case 'test-result-error-expected':
      return `Test "${result.desc}"\n❌ Failed! Expected ${TextRenderer.render(result.expected)}, received ${TextRenderer.render(result.actual)}`
    case 'test-result-error-exn':
      return `Test "${result.desc}"\n❌ Failed! Exception thrown: ${TextRenderer.render(result.exn)}`
    case 'test-result-error-gen':
      return `Test "${result.desc}"\n❌ Failed! ${result.reason}`
  }
})

import * as L from '../../../lpm'
import TextRenderer from '../../../lpm/renderers/text.js'
import { Result, test_isResult } from '../index.js'

TextRenderer.registerCustomRenderer(test_isResult, (v: any) => {
  const result = v as Result
  switch (result[L.structKind]) {
    case 'ok':
      return `Test "${result.desc}"\n✅ Passed!`
    case 'exp':
      return `Test "${result.desc}"\n❌ Failed! Expected ${TextRenderer.render(result.expected)}, received ${TextRenderer.render(result.actual)}`
    case 'exn':
      return `Test "${result.desc}"\n❌ Failed! Exception thrown: ${TextRenderer.render(result.exn)}`
    case 'gen':
      return `Test "${result.desc}"\n❌ Failed! ${result.reason}`
  }
})

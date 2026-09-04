import { expect, test } from 'vitest'
import HTMLDisplay from '../../src/lpm/output/html'
import { runProgramWithHTML } from '../harness'
import { getByLabelText } from '@testing-library/dom'
import { drawing_canvasAriaLabel } from '../../src/js/image/drawing'
// Registers the image library's HTML custom renderer (drawings -> <canvas>)
// on the shared HtmlRenderer singleton. Load-bearing: scamper.ts starts that
// registration and initialize() awaits it (#511), but runProgramWithHTML()
// never calls initialize(), so nothing else here would have run it.
import '../../src/app/web/renderers'

const testSrc = `(import image)
(circle 10 "solid" "red")`

test('tracing-images', async () => {
  // make mock root div
  const mockRootName = 'test-root'
  const mockRoot = document.createElement(mockRootName)
  document.body.appendChild(mockRoot)
  // mock HTMLDisplay
  const mockOut = new HTMLDisplay(mockRoot)
  await runProgramWithHTML(testSrc, mockOut)

  expect(getByLabelText(mockRoot, drawing_canvasAriaLabel)).toBeVisible()
})

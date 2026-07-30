import { expect, test } from 'vitest'
import HTMLDisplay from '../../src/lpm/output/html'
import { runProgramWithHTML } from '../harness'
import { getByLabelText } from '@testing-library/dom'
import { image_canvasAriaLabel } from '../../src/js/image/drawing'
// Registers the image library's HTML custom renderer (drawings -> <canvas>)
// on the shared HtmlRenderer singleton. Needed here because production code
// only wires this up via scamper.ts's fire-and-forget dynamic import (see
// its header comment), which isn't awaited anywhere and so isn't guaranteed
// to have run before this test's assertions.
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

  expect(getByLabelText(mockRoot, image_canvasAriaLabel)).toBeVisible()
})

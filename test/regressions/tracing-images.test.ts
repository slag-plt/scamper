import {expect, test} from "vitest";
import HTMLDisplay from "../../src/lpm/output/html";
import {runProgramWithHTML} from "../harness";
import {getByLabelText} from "@testing-library/dom";
import {canvasAriaLabel} from "../../src/lib/image/drawing";

const testSrc =
`(import image)
(circle 10 "solid" "red")`;


//
test('tracing-images', () => {
    // make mock root div
    const mockRootName = 'test-root';
    const mockRoot = document.createElement(mockRootName)
    document.body.appendChild(mockRoot)
    // mock HTMLDisplay
    const mockOut = new HTMLDisplay(mockRoot)
    runProgramWithHTML(testSrc, mockOut)

    expect(getByLabelText(mockRoot, canvasAriaLabel)).toBeVisible()
})
import TextRenderer from '../../../lpm/renderers/text.js'
import { isSuiteOutput, isTestResult, toJsonText } from './json.js'

// The CLI pipes its stdout to results.json, so this rendering is the file
// itself -- see gradescope/run_autograder.
TextRenderer.registerCustomRenderer(isTestResult, toJsonText)
TextRenderer.registerCustomRenderer(isSuiteOutput, toJsonText)

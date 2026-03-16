import { afterEach, beforeEach, expect, test, vi } from "vitest"
import { runProgram } from "../harness.js"
import { cloneOptions, defaultOptions } from "../../src/lpm/index.js"

const opts = cloneOptions(defaultOptions)
opts.isTracing = true

beforeEach(() => {
  vi.stubGlobal("window", {
    AudioContext: vi.fn(),
  })
})
afterEach(() => {
  vi.unstubAllGlobals()
})

test("basic tracing", () => {
  expect(
    runProgram(
      `
    (define x 5)

    (+ 1 (+ x (+ 3 (+ x 5))))

    (define mult-3
      (lambda (x)
        (+ x (+ x x))))

    (+ (mult-3 x) (mult-3 x))
  `,
      opts,
    ),
  ).toEqual([
    "Defining x",
    "--> 5",
    "Displaying (+ 1 (+ 5 (+ 3 (+ 5 5))))",
    "--> (+ 1 (+ 5 (+ 3 10)))",
    "--> (+ 1 (+ 5 13))",
    "--> (+ 1 18)",
    "--> 19",
    "19",
    "Defining mult-3",
    "--> [Function: ##anonymous##]",
    "Displaying (+ (mult-3 5) (mult-3 5))",
    "--> (+ (+ 5 (+ 5 5)) (mult-3 5))",
    "--> (+ (+ 5 10) (mult-3 5))",
    "--> (+ 15 (mult-3 5))",
    "--> (+ 15 (+ 5 (+ 5 5)))",
    "--> (+ 15 (+ 5 10))",
    "--> (+ 15 15)",
    "--> 30",
    "30",
  ])
})

// TODO: odd output: do we want to show structs differently?
test("tracing music structs", () => {
  expect(
    runProgram(
      `
      (import music)
      (list (dur 1 2) (dur 2 (+ 1 1)))
      `,
      opts,
    ),
  ).toEqual([
    'Imported library: music',
    "Displaying (list (dur 1 2) (dur 2 (+ 1 1)))",
    "--> (list (dur 1 2) (dur 2 (+ 1 1)))",
    "--> (list (dur 1 2) (dur 2 2))",
    "--> (list (dur 1 2) (dur 2 2))",
    "--> (list (dur 1 2) (dur 2 2))",
    "(list (dur 1 2) (dur 2 2))",
  ])
})

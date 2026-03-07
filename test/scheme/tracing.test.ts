import { expect, test } from "vitest"
import { runProgram } from "../harness.js"
import { cloneOptions, defaultOptions } from "../../src/lpm/index.js"

const opts = cloneOptions(defaultOptions)
opts.isTracing = true

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
    "Defining x as 5",
    "--> 5",
    "Displaying (+ 1 (+ 5 (+ 3 (+ 5 5))))",
    "--> (+ 1 (+ 5 (+ 3 10)))",
    "--> (+ 1 (+ 5 13))",
    "--> (+ 1 18)",
    "--> 19",
    "19",
    "Defining mult-3 as (lambda (x) (+ x (+ x x)))",
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

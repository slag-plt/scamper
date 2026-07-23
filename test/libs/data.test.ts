import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

test('tally-all', async () => {
  expect(await runProgram(`
  (import data)
  (tally-all (list "a" "b" "a" "c" "c" "d" "b" "a" "q" "r" "r" "a" "d"))
  `)).toEqual([
    '(list (pair "a" 4) (pair "b" 2) (pair "c" 2) (pair "d" 2) (pair "q" 1) (pair "r" 2))'
  ])
})

describe('parse-csv', () => {
  test('parses rows of fields as strings', async () => {
    expect(await runProgram(`
    (import data)
    (parse-csv "name,age\nAlice,30\nBob,25")
    `)).toEqual([
      '(list (list "name" "age") (list "Alice" "30") (list "Bob" "25"))'
    ])
  })

  test('the empty string has no delimiter to detect', async () => {
    expect(await runProgram(`
    (import data)
    (parse-csv "")
    `)).toEqual([
      `Runtime error [2:5-2:18]: (parse-csv) Error(s) parsing CSV files:
Delimiter (row undefined): Unable to auto-detect delimiting character; defaulted to ','`
    ])
  })

  test('an unterminated quoted field is a parse error', async () => {
    expect(await runProgram(`
    (import data)
    (parse-csv "a,\\"b\nc,d")
    `)).toEqual([
      `Runtime error [2:5-3:5]: (parse-csv) Error(s) parsing CSV files:
Quotes (row 0): Quoted field unterminated`
    ])
  })
})

describe('string->chars', () => {
  test('converts a string into a list of chars', async () => {
    expect(await runProgram(`
    (import data)
    (string->chars "abc")
    `)).toEqual([
      '(list #\\a #\\b #\\c)'
    ])
  })

  test('the empty string has no chars', async () => {
    expect(await runProgram(`
    (import data)
    (string->chars "")
    `)).toEqual([
      'null'
    ])
  })

  test('a non-string argument violates the contract', async () => {
    expect(await runProgram(`
    (import data)
    (string->chars 5)
    `)).toEqual([
      'Runtime error [11:1-11:52]: (error) expected a string, received number'
    ])
  })
})

describe('string->lines', () => {
  // The docstring declares this parameter as `list?`, but data_stringToLines
  // (src/js/data/utils.ts) implements it as a plain string split -- so no
  // call currently succeeds. These tests document that mismatch.
  test('a string argument violates the documented list? contract', async () => {
    expect(await runProgram(`
    (import data)
    (string->lines "a\nb\nc")
    `)).toEqual([
      'Runtime error [17:1-17:52]: (error) expected a list, received string'
    ])
  })

  test('a list argument satisfies the contract but not the implementation', async () => {
    expect(await runProgram(`
    (import data)
    (string->lines (list "a" "b"))
    `)).toEqual([
      'Runtime error [17:1-17:52]: Unexpected error in Javascript function call: TypeError: s.split is not a function'
    ])
  })
})

describe('dataset? and plot?', () => {
  test('discriminate datasets, plots, and arbitrary values', async () => {
    expect(await runProgram(`
    (import data)
    (dataset? (dataset-bar "Sales" (list 1 2 3)))
    (dataset? (plot-linear (dataset-bar "Sales" (list 1 2 3))))
    (dataset? 5)
    (plot? (plot-linear (dataset-bar "Sales" (list 1 2 3))))
    (plot? (dataset-bar "Sales" (list 1 2 3)))
    (plot? "plot")
    `)).toEqual([
      '#t', '#f', '#f', '#t', '#f', '#f'
    ])
  })
})

describe('with-plot-options', () => {
  test('applies axis bounds, labels, and title to a plot', async () => {
    expect(await runProgram(`
    (import data)
    (with-plot-options
      (list (pair "x-min" 0) (pair "x-max" 10) (pair "y-label" "Count") (pair "title" "My Plot"))
      (plot-linear (dataset-bar "Sales" (list 1 2 3))))
    `)).toEqual([
      '(plot [Blob: {"data":{"datasets":[{"type":"bar","label":"Sales","data":[1,2,3]}]},"options":{"scales":{"x":{"type":"linear","min":0,"max":10},"y":{"title":{"display":true,"text":"Count"}}},"plugins":{"title":{"display":true,"text":"My Plot"}}}}])'
    ])
  })
})

describe('with-dataset-options', () => {
  test('applies background and border color to a dataset', async () => {
    expect(await runProgram(`
    (import data)
    (with-dataset-options
      (list (pair "background-color" "red") (pair "border-color" "blue"))
      (dataset-bar "Sales" (list 1 2 3)))
    `)).toEqual([
      '(dataset [Blob: {"type":"bar","label":"Sales","data":[1,2,3],"backgroundColor":"red","borderColor":"blue"}])'
    ])
  })
})

describe('plot builders', () => {
  test('plot-linear, plot-category, and plot-radial pick a scale for their datasets', async () => {
    expect(await runProgram(`
    (import data)
    (plot-linear (dataset-bar "Sales" (list 1 2 3)))
    (plot-category (list "Q1" "Q2") (dataset-bar "Sales" (list 1 2)))
    (plot-radial (list "Q1" "Q2") (dataset-radar "Sales" (list 1 2)))
    `)).toEqual([
      '(plot [Blob: {"data":{"datasets":[{"type":"bar","label":"Sales","data":[1,2,3]}]},"options":{"scales":{"x":{"type":"linear"}}}}])',
      '(plot [Blob: {"data":{"labels":["Q1","Q2"],"datasets":[{"type":"bar","label":"Sales","data":[1,2]}]},"options":{"scales":{"x":{"type":"category"}}}}])',
      '(plot [Blob: {"data":{"labels":["Q1","Q2"],"datasets":[{"type":"radar","label":"Sales","data":[1,2]}]}}])'
    ])
  })
})

describe('dataset-line', () => {
  test('accepts either bare numbers or number pairs as data points', async () => {
    expect(await runProgram(`
    (import data)
    (dataset-line "Growth" (list 1 2 3))
    (dataset-line "Growth" (list (pair 0 1) (pair 1 4)))
    `)).toEqual([
      '(dataset [Blob: {"type":"line","label":"Growth","data":[1,2,3]}])',
      '(dataset [Blob: {"type":"line","label":"Growth","data":[{"x":0,"y":1},{"x":1,"y":4}]}])'
    ])
  })
})

describe('dataset-bar, dataset-pie, dataset-polar, dataset-radar', () => {
  test('wrap a list of numbers with their chart type', async () => {
    expect(await runProgram(`
    (import data)
    (dataset-bar "Sales" (list 1 2 3))
    (dataset-pie "Sales" (list 1 2 3))
    (dataset-polar "Sales" (list 1 2 3))
    (dataset-radar "Sales" (list 1 2 3))
    `)).toEqual([
      '(dataset [Blob: {"type":"bar","label":"Sales","data":[1,2,3]}])',
      '(dataset [Blob: {"type":"pie","label":"Sales","data":[1,2,3]}])',
      '(dataset [Blob: {"type":"polarArea","label":"Sales","data":[1,2,3]}])',
      '(dataset [Blob: {"type":"radar","label":"Sales","data":[1,2,3]}])'
    ])
  })
})

describe('dataset-scatter, dataset-bubble', () => {
  test('convert pairs and triples into coordinate objects', async () => {
    expect(await runProgram(`
    (import data)
    (dataset-scatter "Points" (list (pair 1 2) (pair 3 4)))
    (dataset-bubble "Points" (list (list 1 2 3) (list 4 5 6)))
    `)).toEqual([
      '(dataset [Blob: {"type":"scatter","label":"Points","data":[{"x":1,"y":2},{"x":3,"y":4}]}])',
      '(dataset [Blob: {"type":"bubble","label":"Points","data":[{"x":1,"y":2,"r":3},{"x":4,"y":5,"r":6}]}])'
    ])
  })
})

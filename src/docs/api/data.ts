import { ArgDoc, Doc } from './docs.js'

export const parseCv: Doc = new Doc(
  'parse-csv',
  'any',
  [ new ArgDoc('data', 'string?') ],
  'Parses `data` as a CSV-formatted string and returns a list of rows where each row is a list of fields as strings.'
)

export const stringToChars: Doc = new Doc(
  'string->chars',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Converts the string `s` into a list of char values.'
)

export const stringToWords: Doc = new Doc(
  'string->words',
  'list?',
  [ new ArgDoc('s', 'string?') ],
  'Splits the string `s` into a list of words, using whitespace as the delimiter.'
)

export const stringToLines: Doc = new Doc(
  'string->lines',
  'list?',
  [ new ArgDoc('chars', 'list?') ],
  'Splits the string `s` into a list of strings, where each string is a line of text..'
)

export const tallyAll: Doc = new Doc (
  'tally-all',
  'list?',
  [ new ArgDoc('lst', 'list?') ],
  'Takes a list `lst` and returns a list of pairs where each pair consists of an element from `lst` and the number of times that element appears in `lst`.'
)

export const datasetQ: Doc = new Doc (
  'dataset?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if `v` is a dataset, `#f` otherwise.'
)

export const plotQ: Doc = new Doc (
  'plot?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if `v` is a plot, `#f` otherwise.'
)

export const withPlotOptions: Doc = new Doc (
  'with-plot-options',
  'plot?',
  [
    new ArgDoc('options', 'list of key-value pairs'),
    new ArgDoc('plot', 'plot?'),
  ],
  `Takes an association list of options and a plot, and returns a new plot with the specified options applied. Valid options include:

  - 'x-min': sets the minimum x-axis value
  - 'x-max': sets the maximum x-axis value
  - 'y-min': sets the minimum y-axis value
  - 'y-max': sets the maximum y-axis value
  - 'x-label': sets the label for the x-axis
  - 'y-label': sets the label for the y-axis
  `
)

export const withDatasetOptions: Doc = new Doc (
  'with-dataset-options',
  'dataset?',
  [
    new ArgDoc('options', 'list of key-value pairs'),
    new ArgDoc('dataset', 'dataset?'),
  ],
  `Takes an association list of options and a dataset, and returns a new dataset with the specified options applied. Valid options include:
  
  - 'background-color': sets the background color of the dataset
  - 'border-color': sets the border color of the dataset
  `
)

export const plotLinear: Doc = new Doc (
  'plot-linear',
  'plot?',
  [
    new ArgDoc('datasets', 'list of datasets'),
  ],
  'Creates a linear plot from the provided list of datasets. Valid datasets for this plot include line, bar, scatter, and bubble datasets.'
)

export const plotCategory: Doc = new Doc (
  'plot-category',
  'plot?',
  [
    new ArgDoc('labels', 'list of strings'),
    new ArgDoc('datasets', 'list of datasets'),
  ],
  'Creates a categorical plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.'
)

export const plotRadial: Doc = new Doc (
  'plot-radial',
  'plot?',
  [
    new ArgDoc('labels', 'list of strings'),
    new ArgDoc('datasets', 'list of datasets'),
  ],
  'Creates a radial plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.'
)

export const datasetLine: Doc = new Doc (
  'dataset-line',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers or list of pairs of numbers'),
  ],
  'Creates a line dataset with the given `title` and `data` points. If the dataset is used in a numeric (e.g., linear) plot, the data points must be a list of pairs of numbers. If the dataset is used in a categorical plot, the data points must be a list of numbers.'
)

export const datasetBar: Doc = new Doc (
  'dataset-bar',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers'),
  ],
  'Creates a bar dataset with the given `title` and `data` points.'
)

export const datasetScatter: Doc = new Doc (
  'dataset-scatter',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers'),
  ],
  'Creates a scatter dataset with the given `title` and `data` points.'
)

export const datasetBubble: Doc = new Doc (
  'dataset-bubble',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of lists of three numbers'),
  ],
  'Creates a bubble dataset with the given `title` and `data` points. The three numbers of each data point represent x-coordinate, y-coordinate, and radius of each point.'
)

export const datasetPie: Doc = new Doc (
  'dataset-pie',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers'),
  ],
  'Creates a pie dataset with the given `title` and `data` points.'
)

export const datasetPolar: Doc = new Doc (
  'dataset-polar',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers'),
  ],
  'Creates a polar dataset with the given `title` and `data` points.'
)

export const datasetRadar: Doc = new Doc (
  'dataset-radar',
  'dataset?',
  [
    new ArgDoc('title', 'string?'),
    new ArgDoc('data', 'list of numbers'),
  ],
  'Creates a radar dataset with the given `title` and `data` points.'
)
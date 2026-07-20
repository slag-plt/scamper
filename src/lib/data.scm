;;; (parse-csv data) -> any
;;;  data : string?
;;; Parses `data` as a CSV-formatted string and returns a list of rows where each row is a list of fields as strings.
(define parse-csv (js-var "data_parseCsv"))
;;; (string->chars s) -> list?
;;;  s : string?
;;; Converts the string `s` into a list of char values.
(define string->chars (js-var "data_stringToChars"))
;;; (string->words s) -> list?
;;;  s : string?
;;; Splits the string `s` into a list of words, using whitespace as the delimiter.
(define string->words (js-var "data_stringToWords"))
;;; (string->lines chars) -> list?
;;;  chars : list?
;;; Splits the string `s` into a list of strings, where each string is a line of text..
(define string->lines (js-var "data_stringToLines"))
;;; (tally-all lst) -> list?
;;;  lst : list?
;;; Takes a list `lst` and returns a list of pairs where each pair consists of an element from `lst` and the number of times that element appears in `lst`.
(define tally-all (js-var "data_tallyAll"))
;;; (dataset? v) -> boolean?
;;;  v : any
;;; Returns `#t` if `v` is a dataset, `#f` otherwise.
(define dataset? (js-var "data_datasetQ"))
;;; (plot? v) -> boolean?
;;;  v : any
;;; Returns `#t` if `v` is a plot, `#f` otherwise.
(define plot? (js-var "data_plotQ"))
;;; (with-plot-options options plot) -> plot?
;;;  options : any
;;;   list of key-value pairs
;;;  plot : plot?
;;; Takes an association list of options and a plot, and returns a new plot with the specified options applied. Valid options include: - 'x-min': sets the minimum x-axis value - 'x-max': sets the maximum x-axis value - 'y-min': sets the minimum y-axis value - 'y-max': sets the maximum y-axis value - 'x-label': sets the label for the x-axis - 'y-label': sets the label for the y-axis
(define with-plot-options (js-var "data_withPlotOptions"))
;;; (with-dataset-options options dataset) -> dataset?
;;;  options : any
;;;   list of key-value pairs
;;;  dataset : dataset?
;;; Takes an association list of options and a dataset, and returns a new dataset with the specified options applied. Valid options include: - 'background-color': sets the background color of the dataset - 'border-color': sets the border color of the dataset
(define with-dataset-options (js-var "data_withDatasetOptions"))
;;; (plot-linear datasets) -> plot?
;;;  datasets : any
;;;   list of datasets
;;; Creates a linear plot from the provided list of datasets. Valid datasets for this plot include line, bar, scatter, and bubble datasets.
(define plot-linear (js-var "data_plotLinear"))
;;; (plot-category labels datasets) -> plot?
;;;  labels : any
;;;   list of strings
;;;  datasets : any
;;;   list of datasets
;;; Creates a categorical plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.
(define plot-category (js-var "data_plotCategory"))
;;; (plot-radial labels datasets) -> plot?
;;;  labels : any
;;;   list of strings
;;;  datasets : any
;;;   list of datasets
;;; Creates a radial plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.
(define plot-radial (js-var "data_plotRadial"))
;;; (dataset-line title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers or list of pairs of numbers
;;; Creates a line dataset with the given `title` and `data` points. If the dataset is used in a numeric (e.g., linear) plot, the data points must be a list of pairs of numbers. If the dataset is used in a categorical plot, the data points must be a list of numbers.
(define dataset-line (js-var "data_datasetLine"))
;;; (dataset-bar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a bar dataset with the given `title` and `data` points.
(define dataset-bar (js-var "data_datasetBar"))
;;; (dataset-scatter title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a scatter dataset with the given `title` and `data` points.
(define dataset-scatter (js-var "data_datasetScatter"))
;;; (dataset-bubble title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of lists of three numbers
;;; Creates a bubble dataset with the given `title` and `data` points. The three numbers of each data point represent x-coordinate, y-coordinate, and radius of each point.
(define dataset-bubble (js-var "data_datasetBubble"))
;;; (dataset-pie title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a pie dataset with the given `title` and `data` points.
(define dataset-pie (js-var "data_datasetPie"))
;;; (dataset-polar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a polar dataset with the given `title` and `data` points.
(define dataset-polar (js-var "data_datasetPolar"))
;;; (dataset-radar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a radar dataset with the given `title` and `data` points.
(define dataset-radar (js-var "data_datasetRadar"))

// The argument values test/libs/contracts.test.ts feeds every contracted
// library binding. Four deliberately separate tables, smallest first: a value
// per *predicate*, then the per-module and per-binding corrections that a
// predicate alone cannot express.
//
// Every value is a Scheme *expression*, spliced straight into the generated
// program. Nothing is bound with a preamble `define`, so no name the library
// exports can be shadowed by this file, and a failing program can be printed
// exactly as it ran.

/**
 * A file the `file` library's readers are pointed at. Seeded into the
 * in-memory filesystem by contracts.test.ts before the sweep runs: a name that
 * is merely a `string?` is well-typed and still not a file.
 */
export const SAMPLE_FILE = 'contract-sample.txt'

/** What {@link SAMPLE_FILE} holds. */
export const SAMPLE_FILE_CONTENTS = 'one\ntwo\n'

/**
 * A value satisfying each parameter predicate, keyed by the predicate's exact
 * source text -- so `(or/p pair? nonempty-list?)` is one key rather than
 * something reconstructed from its parts.
 *
 * The `procedure?` sample takes a rest-only argument list, which the grammar
 * admits (see syntax.grammar's ArgList), so one sample satisfies a callback of
 * any arity.
 *
 * N.B., `function` and `list` are deliberately absent: they are not predicates
 * and nothing in `src/lib` binds them. The one docstring that named them
 * (`apply☀︎`) was uncallable because of it.
 */
export const SAMPLES: Record<string, string> = {
  // prelude
  any: '0',
  'number?': '1',
  'integer?': '1',
  'string?': '"abc"',
  'char?': '#\\a',
  'boolean?': '#t',
  'list?': '(list 1 2 3)',
  'vector?': '(vector 1 2 3)',
  'procedure?': '(lambda (& args) 0)',
  '(or/p pair? nonempty-list?)': '(list 1 2 3)',
  'hash?': '(list->hash (list (pair "a" 1)))',
  'ref?': '(ref 0)',

  // image (and canvas, which imports it -- see HELPERS)
  'color?': '"red"',
  'rgb?': '(rgb 0 0 0)',
  'rgb-component?': '0',
  'hsv?': '(hsv 0 0 0)',
  'drawing?': '(solid-square 10 "red")',
  'fill-mode?': '"solid"',
  'font?': '(font "sans-serif")',

  // music
  'dur?': 'qn',
  'note-value?': '60',
  'mod?': 'percussion',
  'composition?': '(note 60 qn)',

  // rex
  'rex?': '(regex "a")',

  // data
  'dataset?': '(dataset-bar "counts" (list 1 2))',
  'plot?': '(plot-linear (dataset-line "xs" (list (pair 1 1))))',

  // gradescope
  'gradescope-test-result?': '(gradescope-test-result "case" "passed" 1 1 "ok")',
  'gradescope-test-suite-output?':
    '(gradescope-test-suite (list (test-result-ok "ok")))',

  // html, reactive
  'text-area?': '(text-area "notes")',
  'button?': '(button "Click" (lambda () #t))',
  // N.B., never on-timer: a timer subscription outlives the test (#375, and
  // the note in test/libs/reactive.test.ts).
  'subscription?': '(on-mouse-click)',

  // audio
  'sample?': '(sample-node (vector 0 0.5))',
  '(or/p audio-node? sample?)': '(sample-node (vector 0 0.5))',
}

/**
 * Per-module overrides of {@link SAMPLES}: a predicate's *value* has to be
 * built from what the module under test exports, and two modules can spell the
 * same type differently. `canvas?` is the whole of it -- `canvas` makes one
 * directly, `image` only converts a drawing into one.
 */
export const MODULE_SAMPLES: Record<string, Record<string, string>> = {
  canvas: {
    'canvas?': '(make-canvas 10 10)',
  },
  image: {
    'canvas?': '(drawing->canvas (solid-square 10 "red"))',
    'pixels?': '(canvas->pixels (drawing->canvas (solid-square 2 "red")))',
  },
}

/**
 * Modules imported *before* the module under test, so the module's own
 * bindings win the name race on anything both export.
 *
 * `canvas` re-exports `drawing?`/`color?`/`font?` as predicates only; the
 * constructors that build those values live in `image`. `reactive`'s `button?`
 * likewise comes from `html`, and its components draw on a canvas. `gradescope`
 * turns `test`'s results into its cases.
 */
export const HELPERS: Record<string, string[]> = {
  canvas: ['image'],
  gradescope: ['test'],
  reactive: ['html', 'canvas'],
}

/**
 * A list on which the accessor `c<letters>r` succeeds, matched to the
 * accessor's own nesting: reading the letters left to right, each `a` wraps
 * what is left in a list (so `car` reaches it) and each `d` prepends an
 * element (so `cdr` reaches it). The accessor applies its letters
 * right-to-left, which is why this builds them the other way.
 */
function accessorArg(letters: string): string {
  let arg = '(list 1 2)'
  for (const letter of letters) {
    arg = letter === 'a' ? `(list ${arg} 9)` : `(cons 9 ${arg})`
  }
  return arg
}

/** Every `c[ad]+r` accessor name of up to `depth` letters, shortest first. */
function accessorNames(depth: number): string[] {
  let letters = ['a', 'd']
  const names: string[] = []
  for (let i = 1; i <= depth; i++) {
    names.push(...letters.map((l) => `c${l}r`))
    letters = letters.flatMap((l) => [`a${l}`, `d${l}`])
  }
  return names
}

/**
 * The 30 `c[ad]+r` accessors' arguments, generated rather than written out:
 * they differ only in the nesting their name asks for, and all 30 share one
 * `(or/p pair? nonempty-list?)` predicate that says nothing about it.
 */
const ACCESSOR_ARGS: [string, string[]][] = accessorNames(4).map((name) => [
  `prelude:${name}`,
  [accessorArg(name.slice(1, -1))],
])

/**
 * Whole argument tuples for the bindings whose type-correct default is
 * semantically wrong -- an index out of range, a comparator of the wrong
 * arity, a list of the wrong element type. An *override*, not an exception:
 * the binding is still asserted on, just with an argument it can actually use.
 *
 * Positional, and as long as the signature: every fixed parameter, then every
 * optional one, then one rest argument if the signature declares a rest
 * parameter (which the zero-rest test drops -- see contracts.test.ts).
 */
export const ARGS = new Map<string, string[]>([
  ...ACCESSOR_ARGS,

  // prelude: well-typed arguments that mean something the function can use.
  ['prelude:with-handler', ['(lambda (msg) msg)', '(lambda () 0)']],
  ['prelude:quotient', ['7', '2']],
  ['prelude:remainder', ['7', '2']],
  ['prelude:modulo', ['7', '2']],
  ['prelude:string->number', ['"1"']],
  ['prelude:digit-value', ['#\\5']],
  ['prelude:list->string', ['(list #\\a #\\b)']],
  ['prelude:vector->string', ['(vector #\\a #\\b)']],
  ['prelude:list->hash', ['(list (pair "a" 1))']],
  ['prelude:assoc-key?', ['"a"', '(list (pair "a" 1))']],
  ['prelude:assoc-ref', ['"a"', '(list (pair "a" 1))']],
  ['prelude:assoc-set', ['"a"', '2', '(list (pair "a" 1))']],
  ['prelude:hash-ref', ['(list->hash (list (pair "a" 1)))', '"a"']],
  ['prelude:sort', ['(list 3 1 2)', '(lambda (a b) (< a b))']],
  ['prelude:filter', ['(lambda (x) #t)', '(list 1 2 3)']],
  ['prelude:vector-filter', ['(lambda (x) #t)', '(vector 1 2 3)']],
  ['prelude:string-map', ['(lambda (c) c)', '"abc"']],
  ['prelude:fold', ['(lambda (acc x) acc)', '0', '(list 1 2 3)']],
  ['prelude:fold-left', ['(lambda (acc x) acc)', '0', '(list 1 2 3)']],
  ['prelude:fold-right', ['(lambda (x acc) acc)', '0', '(list 1 2 3)']],
  ['prelude:reduce', ['(lambda (a b) a)', '(list 1 2 3)']],
  ['prelude:reduce-right', ['(lambda (a b) b)', '(list 1 2 3)']],
  // A generic `integer?` sample would cap the stack at one frame, for every
  // statement after this one in the generated program.
  ['prelude:set-maximum-recursion-depth!', ['10000']],

  // image: a "list" that has to hold points, an "align" that has to be one of
  // the three words the function knows, dimensions that have to match a buffer.
  ['image:path', ['10', '10', '(list (pair 0 0) (pair 10 10))', '"solid"', '"red"', '1']],
  ['image:with-dash', ['(list 5 5)', '(solid-square 10 "red")']],
  ['image:beside/align', ['"top"', '(solid-square 10 "red")']],
  ['image:above/align', ['"left"', '(solid-square 10 "red")']],
  ['image:overlay/align', ['"left"', '"top"', '(solid-square 10 "red")']],
  ['image:color-name->rgb', ['"red"']],
  ['image:find-colors', ['"red"']],
  ['image:pixel-map', ['(lambda (c) c)', '(drawing->canvas (solid-square 4 "red"))']],
  [
    'image:pixels->canvas',
    ['(canvas->pixels (drawing->canvas (solid-square 2 "red")))', '2', '2'],
  ],
  [
    'image:canvas-set-pixels!',
    [
      '(drawing->canvas (solid-square 2 "red"))',
      '(canvas->pixels (drawing->canvas (solid-square 2 "red")))',
    ],
  ],

  // canvas: a path is a list of points, like image's.
  [
    'canvas:canvas-path!',
    ['(make-canvas 10 10)', '(list (pair 0 0) (pair 10 10))', '"solid"', '"red"'],
  ],

  // data: a string that is actually delimited, and `any` parameters whose
  // shape every one of these reads.
  ['data:parse-csv', ['"name,age\\nAlice,30"']],
  ['data:with-plot-options', ['(list (pair "x-min" 0))', '(plot-linear (dataset-line "xs" (list (pair 1 1))))']],
  ['data:with-dataset-options', ['(list (pair "border-color" "red"))', '(dataset-bar "counts" (list 1 2))']],
  ['data:plot-linear', ['(dataset-line "xs" (list (pair 1 1)))']],
  ['data:plot-category', ['(list "a" "b")', '(dataset-bar "counts" (list 1 2))']],
  ['data:plot-radial', ['(list "a" "b")', '(dataset-bar "counts" (list 1 2))']],
  ['data:dataset-line', ['"xs"', '(list (pair 1 1))']],
  ['data:dataset-bar', ['"counts"', '(list 1 2)']],
  ['data:dataset-scatter', ['"points"', '(list (pair 1 1))']],
  ['data:dataset-bubble', ['"bubbles"', '(list (list 1 2 3))']],
  ['data:dataset-pie', ['"shares"', '(list 1 2)']],
  ['data:dataset-polar', ['"shares"', '(list 1 2)']],
  ['data:dataset-radar', ['"scores"', '(list 1 2)']],

  // file: a name that is merely a string is not a file, and "lines" are strings.
  ['file:file->string', [`"${SAMPLE_FILE}"`]],
  ['file:file->lines', [`"${SAMPLE_FILE}"`]],
  ['file:lines->file', ['(list "one" "two")', '"written.txt"']],

  // gradescope: a suite is built out of test results.
  ['gradescope:gradescope-test-suite', ['(list (test-result-ok "ok"))']],
  ['gradescope:gradescope-test-result', ['"case"', '"passed"', '1', '1', '"ok"']],

  // html: children have to be elements, not the generic `any` sample.
  ['html:tag-set-children!', ['(tag "div")', '(tag "span")']],

  // music: a sample of numbers, not the generic vector.
  ['music:note-handlers', ['(vector (lambda (msg) 0))']],
  ['reactive:on-note', ['(vector (lambda (msg) 0))']],

  // audio: `any`, documented as "vector? of numbers between -1.0 and 1.0".
  ['audio:sample-node', ['(vector 0 0.5)']],

  // reactive: a component's view draws and its update returns a state.
  [
    'reactive:reactive-canvas',
    [
      '10', '10', '0',
      '(lambda (st canv) (canvas-rectangle! canv 0 0 10 10 "solid" "red"))',
      '(lambda (msg st) st)',
      '(on-mouse-click)',
    ],
  ],
  [
    'reactive:reactive-container',
    ['0', '(lambda (st) (tag "div"))', '(lambda (msg st) st)', '(on-mouse-click)'],
  ],

  // test: a real comparator and a real thunk, so a case actually runs.
  ['test:test-case', ['"adds"', 'equal?', '4', '(lambda () (+ 2 2))']],
  ['test:test-exn', ['"raises"', '(lambda () (error "boom"))']],
])



export const builtinLibs = new Map<string, string[]>([
/// HEAD
    ["list",["any-off", "appendFile", "asin", "assoc-key?", "assoc-ref", "boolean?", "car", ]],
    ["images", ["above", "above/align", "all-color-names", "beside", "beside/align", 
      "canvas-set-pixels!", "circle", "color", "color?", "color-name?", "color-name->hsv", 
      "color-name->rgb", "drawing->image", "drawing->pixels", "ellipse", "find-colors", "font", 
      "hsv", "hsv?", "hsv->rgb", "hsv->string", "hsv-alpha", "hsv-complement", "hsv-hue", 
      "hsv-saturation", "hsv-value", "image?", "image->pixels", "image-color", "image-get-pixel", 
      "image-height", "image-recolor", "image-width", "isosceles-triangle", "outlined-circle", 
      "outlined-ellipse", "outlined-isosceles-triangle", "outlined-rectangle", "outlined-square", 
      "outlined-triangle", "overlay", "overlay/align", "overlay/offset", "path", "pixel-map", 
      "pixels->image", "rectangle", "rgb", "rgb?", "rgb->hsv", "rgb->string", "rgb-add", 
      "rgb-alpha", "rgb-average", "rgb-blue", "rgb-bluer", "rgb-component?", "rgb-darker", 
      "rgb-distance", "rgb-green", "rgb-greener", "rgb-greyscale", "rgb-hue", "rgb-lighter", 
      "rgb-phaseshift", "rgb-pseudo-complement", "rgb-red", "rgb-redder", "rgb-rotate-components", 
      "rgb-saturation","rgb-subtract", "rgb-thicken", "rgb-thin", "rgb-value", "rotate", "shape?", 
      "solid-circle", "solid-ellipse", "solid-isosceles-triangle", "solid-rectangle", "solid-square", 
      "solid-triangle", "square", "text", "triangle", "with-dash", "with-image-file", "with-image-from-url"]],
    ["color", ["all-color-names", "color", "color?", "color-name?", "color-name->hsv", 
      "color-name->rgb", "hsv", "hsv?", "hsv->rgb", "hsv->string", "hsv-alpha", "hsv-complement", 
      "hsv-hue", "hsv-saturation", "hsv-value", "image-color", "image-get-pixel", "rgb", "rgb?", 
      "rgb->hsv", "rgb->string", "rgb-add", "rgb-alpha", "rgb-average", "rgb-blue", "rgb-bluer", 
      "rgb-component?", "rgb-darker", "rgb-distance", "rgb-green", "rgb-greener", "rgb-greyscale", 
      "rgb-hue", "rgb-lighter", "rgb-phaseshift", "rgb-pseudo-complement", "rgb-red", "rgb-redder", 
      "rgb-rotate-components", "rgb-saturation", "rgb-subtract", "rgb-thicken", "rgb-thin", 
      "rgb-value"]],
    ["rbg", ["color", "color?", "color-name->rgb", "hsv->rgb", "image-get-pixel", "rgb", "rgb?", 
      "rgb->hsv", "rgb->string", "rgb-add", "rgb-alpha", "rgb-average", "rgb-blue", "rgb-bluer", 
      "rgb-component?", "rgb-darker", "rgb-distance", "rgb-green", "rgb-greener", "rgb-greyscale", 
      "rgb-hue", "rgb-lighter", "rgb-phaseshift", "rgb-pseudo-complement", "rgb-redder", 
      "rgb-rotate-components", "rgb-saturation", "rgb-subtract", "rgb-thicken", "rgb-thin", 
      "rgb-value"]],
    ["hsv", ["color?", "color-name->hsv", "hsv", "hsv?", "hsv->rgb", "hsv->string", "hsv-alpha", 
      "hsv-complement", "hsv-hue", "hsv-saturation", "hsv-value", "rgb-hue", "rgb-saturation", 
      "rgb-value"]],
    ["shapes", ["canvas-circle!", "canvas-ellipse!", "canvas-rectangle!", "circle", "ellipse", 
      "isosceles-triangle", "outlined-circle", "outlined-ellipse", "outlined-isosceles-triangle", 
      "outlined-rectangle", "outlined-square", "outlined-triangle", "rectangle", "shape?", 
      "solid-circle", "solid-ellipse", "solid-isosceles-triangle", "solid-rectangle", "solid-square", 
      "solid-triangle", "square", "triangle", "with-dash"]],
    ["composition", ["above", "above/align", "beside", "beside/align", "overlay", "overlay/align", 
      "overlay/offset"]],
    ["placement", ["above", "above/align", "beside", "beside/align", "overlay", "overlay/align", 
      "overlay/offset"]],
    ["path", ["canvas-path!", "path"]],
    ["canvas", ["animate-with", "canvas-circle!", "canvas-drawing!", "canvas-ellipse", 
      "canvas-onclick!", "canvas-path!", "cnvas-rectangle!", "canvas-set-pixels!", 
      "canvas-text!", "make-canvas", "reactive-canvas", "with-dash"]],
    ["pixel", ["canvas-set-pixels!", "drawing->image", "drawing->pixels", "image-get-pixel", 
      "pixel-map", "pixels->image"]],
    ["music", ["band", "composition?", "denominator", "dur", "dur?", "dynamics", "empty", "en", 
      "hn", "instrument", "load-instrument", "load-percussion", "make-note-handlers", "mod", "mod?", 
      "note", "note-event", "note-freq", "note-handlers", "note-value?", "numerator", "octave?", 
      "par", "percussion", "pickup", "pitch?", "play-composition", "qn", "repeat", "rest", "seq", "sn", 
      "tempo", "tn", "trigger", "use-high-quality-instruments", "wn"]],
    ["duration", ["denominator", "dur", "dur?", "en", "hn", "numerator", "sn", "tn", "wn"]],
    ["instruments", ["band", "instrument", "load-instrument", "load-percussion", "high-quality-instruments"]],
    ["note", ["empty", "make-note-handlers", "note", "note-event", "note-freq", "note-handlers", 
      "note-value?", "qn", "rest"]],
    ["modifications", ["dynamics", "mod", "mod?", "octave?", "percussion", "pitch?", "tempo"]],
    ["audio", ["audio-context", "audio-file-node", "audio-pipeline", "delay-node", "oscillator-node", 
      "play-sample", "sample-node"]],
    ["sound", ["audio-context", "audio-file-node", "audio-pipeline", "band", "composition?", "delay-node", 
      "denominator", "dur", "dur?", "dynamics", "empty", "en", "hn", "instrument", "load-instrument", 
      "load-percussion", "make-note-handlers", "mod", "mod?", "note", "note-event", "note-freq", 
      "note-handlers", "note-value?", "numerator", "oscillator-node", "octave?", "par", "percussion", "pickup",
       "pitch?", "play-composition", "play-sample", " qn", "repeat", "rest", "sample-node", "seq", "sn", "tempo", 
       "tn", "trigger", "use-high-quality-instruments", "wn"]],
    ["data", ["dataset?", "dataset-bar", "dataset-bubble", "dataset-line", "dataset-pie", 
      "dataset-polar", "dataset-radar", "dataset-scatter", "parse-csv", "plot?", "plot-category", 
      "plot-linear", "plot-radical", "string->chars", "string->lines", "string->words", "tally-all", 
      "with-dataset-options", "with-plot-options"]],
    ["create", ["dataset-bar", "dataset-bubble", "dataset-line", "dataset-pie", "dataset-polar", 
      "dataset?", "dataset-radar", "dataset-scatter", "plot-category", "plot-linear", "plot?", 
      "plot-radical", "with-dataset-options", "with-plot-options"]],
    ["plot", ["plot?", "plot-category", "plot-linear", "plot-radical", "with-plot-options"]],
    ["parse", ["parse-csv"]],
    
///=======
    ["list", ["append", "apply", "assoc-key ?", "assoc-ref", "assoc-set", "car", "cdr", "cons", "c...r", "filter", "fold", "fold-left", 
            "fold-right", "index-of", "length", "list", "list-drop", "list-of", "list?", "list-ref", "list->string", "list-tail", 
            "list-take", "list->vector", "make-list", "map", "null?", "null", "pair", "range", "reduce", "reduce-right", "reverse",
            "sort", "string -> list", "tally-all", "vector -> list"]] ,
    ["list manipulation", ["append", "apply", "assoc-key?", "assoc-ref", "assoc-set", "car", "cdr", "c...r", "filter", "fold", "fold-left",
            "fold-right", "index-of", "length", "list-drop", "list->string", "list-tail", "list-take", "list->vector", "map", 
            "reduce", "reduce-right", "reverse", "sort"]],
    ["list creation", ["cons", "list", "make-list", "null", "pair", "range", "string->list", "vector->list"]],
    ["math", ["abs", "acos", "asin", "atan", "ceiling", "cos", "/", "even?", "exp", "expt", "floor", ">=", ">", "integer?", "<=", "log",
            "<", "max", "min", "-", "modulo", "nan?", "negative?", "number?", "=", "odd?", "pi", "π", "+", "positive?", "quotient", "real?", 
            "remainder", "round", "sin", "sqrt", "square", "tan", "*", "truncate", "zero?"]],
    ["algebra", ["abs", "ceiling", "/", "exp", "expt", "floor", "log", "-", "modulo", "pi", "π", "+", "quotient",
            "remainder", "round", "sqrt", "square", "*", "truncate"]],
    ["comparator", ["even?", ">=", ">", "integer?", "<=", "<", "max", "min", "nan?", "negative?", "number?", "=", "odd?", "positive?",
            "real?", "zero?"]],
    ["trigonometry", ["acos", "asin", "atan", "cos", "sin", "tan"]],
    ["function composition", ["all-of", "any-of", "compose", "=-eps", "list-of", "o", "|>]"]],
    ["association list", ["assoc-key?", "assoc-ref", "assoc-set", "car", "cdr", "cons", "c...r", "filter", "fold",
            "fold-left", "fold-right", "index-of", "list", "list-drop", "list-of", "list?", "list-ref", "list->string",
            "list-tail", "list-take", "list->vector", "make-list", "map", "null?", "null", "sort"]],
    ["type check", ["boolean?", "char-alphabetic?", "char-numeric?", "char?", "char-upper-case?", "char-whitespace?", "digit-value",
            "integer?", "ref?", "list?", "nan?", "negative?", "null?", "number?", "odd?", "pair?", "positive?", "procedure?",
            "real?", "string?", "vector?", "void?", "zero?", "color?", "image?", "color-name?", "hsv?", "rgb?", "rgb-component?", "shape?",
            "composition?", "dur?", "mod?", "note-value?", "octave?", "pitch?", "dataset?", "plot?", "rex-matches?", "rex?"]],
    ["string", ["error", "file->lines", "file->string", "make-string", "number->string", "string", "string-append", "string-contains",
            "string-downcase", "string=?", "string-ci=?", "string-foldcase", "string>=?", "string-ci>=?", "string>?", "string-ci>?", 
            "string-length", "string<=?", "string-ci<=?", "string->list", "string<?", "string-ci<?", "string-map", "string->number", "string?",
            "string-ref", "string-split", "string-split-vector", "string->words", "string-upcase", "string->vector", "substring",
            "string->chars", "string->lines", "string->words"]],
    ["boolean/logic", ["all-of", "any-of", "boolean?", "else", "implies", "nand", "nor", "not", "xor"]],
    ["char", ["char-alphabetic?", "char-downcase", "char=?", "char-ci=?", "char-foldcase", "char>=?", "char-ci>=?", "char>?",
            "char-ci>?", "char<=?", "char-ci<=?", "char-lower-case?", "char<?", "char-ci<?", "char-numeric?", "char?", "char->integer",
            "char-upcase", "char-upper-case?", "char-whitespace?", "digit-value", "integer->char", "string->chars"]],
    ["regexes", ["rex-any-char", "rex-any-of", "rex-char-antiset", "rex-char-range", "rex-char-set", "rex-concat", "rex-empty",
            "rex-find-matches", "rex-matches?", "rex-optional", "rex?", "regex", "rex-repeat", "rex-repeat-0", "rex-split-string",
            "rex-string", "rex->string"]],
    ["vectors", ["list->vector", "make-vector", "string-split-vector", "string->vector", "vector", "vector-append",
            "vector-fill!", "vector-filter", "vector-for-each", "vector-length", "vector->list", "vector-map", "vector-map!",
            "vector?", "vector-range", "vector-ref", "vector-set!", "vector->string"]],
["mutation", ["set-maximum-recursion-depth!", "set-ref!", "vector-fill!", "vector-map!", "vector-set!", "canvas-set-pixels!",
            "canvas-circle!", "canvas-drawing!", "canvas-ellipse!", "canvas-onclick!", "canvas-path!", "canvas-rectangle!",
            "canvas-text!", "on-keydown!", "tag-set-children!"]],
    ["predicates", ["assoc-key?", "boolean?", "char-alphabetic?", "char=?", "char-ci=?", "char>=?", "char-ci>=?", "char>?",
            "char-ci>?", "char<=?", "char-ci<=?", "char-lower-case?", "char<?", "char-ci<?", "char-numeric?", "char?", "char-upper-case?", 
            "char-whitespace?", "equal?", "even?", "implies", "integer?", "ref?", "list?", "nan?", "negative?", "null?", "number?", 
            "odd?", "pair?", "positive?", "procedure?", "real?", "set-maximum-recursion-depth!", "set-ref!", "string=?", "string-ci=?",
            "string>=?", "string-ci>=?", "string>?", "string-ci>?", "string<=?", "string-ci<=?", "string<?", "string-ci<?", "string?",
            "vector-fill!", "vector-map!", "vector?", "vector-set!", "void?", "zero?", "canvas-set-pixels!", "color?", "image?",
            "color-name?", "hsv?", "rgb?", "rgb-component?", "shape?", "composition?", "dur?", "mod?", "note-value?", "octave?",
            "pitch?", "canvas-circle!", "canvas-drawing!", "canvas-ellipse!", "canvas-onclick!", "canvas-path!", "canvas-rectangle!",
            "canvas-text!", "on-keydown!", "tag-set-children!", "dataset?", "plot?", "rex-matches?", "rex?"]],
    ["testing", ["error", "test-case", "test-error", "test-exn", "test-ok"]],
    ["formatting", ["description", "part", "problem", "title"]],
    ["constants", ["else", "null", "pi", "π", "??", "void", "all-color-names", "empty", "en", "hn", "percussion",
            "qn", "sn", "trigger", "wn"]]
  ])


const dict = builtinLibs

// new Map<string, string[]>([
//         ["ap", ["b"]],
//         ["c", ["d"]]
//     ])

export class Search {  // GET INFO FROM DICTIONARY FOR THIS TO BE CORRECT

    tags : string[] //each element is one tag that was selected
    andOr : string //boolean searches passed only as "and" or "or" where "or" is default
    functions : string[] | undefined

    constructor(selectedTags : string[], andOr : string) {
        this.tags = selectedTags
        // For each tag in tags, get the functions from the hash map and, if they are not already
        // in functions (the array) add them there
        this.andOr = andOr
        // For each element of tags, we need to grab all elements of that key from map into temp array
        // Then put each element in temp array into functions according to boolean from andOr
        this.functions = dict.get(this.tags[0])   
    }
}



function searchResults (selectedTags : string[], andOr : string) {

    let userTags = Search.constructor(selectedTags, andOr)

    for (let i = 1; i < userTags.tags.length; i++) {
        const nextFuncs = new Array(dict.get(userTags.tags[i]))

        if(userTags.andOr.equals("and")) {
            var otherFuncs = new Array
            for (let j = 0; j < nextFuncs.length; j++) {
                if (userTags.functions.include(nextFuncs[j])) {
                    otherFuncs.push(nextFuncs[j])
                    userTags.functions = otherFuncs
                }
            }
        } else {
            for (let j = 0; j < nextFuncs.length; j++) {
                if (!userTags.functions.include(nextFuncs[j])) {
                    userTags.functions.push(nextFuncs[j])
                }
            }
        }
    }
}
  
    //Still need to:
    // process the list of function names stored as strings in this.functions to retrieve 
    //      the appropriate documentation
    // display the gathered documentation
    



 ///let sampleTest = Search.constructor(["boolean", "trigonometry"], "and")

   console.log(searchResults(["boolean", "trigonometry"], "and"))
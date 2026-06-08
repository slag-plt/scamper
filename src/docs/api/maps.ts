


export const builtinLibs = new Map<string, string[]>([
    ["list",["any-off", "appendFile", "asin", "assoc-key?", "assoc-ref", "boolean?", "car", ]],
    ["images", []],
    ["color", ["all-color-names", "color", "color-name->rgb", "color?", "hsv", "hsv-alpha", "hsv-complement", "hsv-hue", "hsv-saturation", "hsv->rgb", "hsv->string", "hsv-value", "image-color", "image-get-pixel", "color-name?", "hsv?", "rgb?", "rgb-component?", "rgb-alpha", "rgb-blue", "rgb-green", "rgb-red", "rgb->string", "rgb", "rgb-add", "rgb-average", "rgb-bluer", "rgb-darker", "rgb-distance", "rgb-greener", "rgb-greyscale", "rgb-hue", "rgb-lighter", "rgb-phaseshift", "rgb-pseudo-complement", "rgb-redder", "rgb-rotate-components", "rgb-saturation", "rgb-subtract", "rgb-thicken", "rgb-thin", "rgb->hsv", "rgb-value"]],
    ["rbg", ["color", "color-name->rgb", "color?", "hsv->rgb", "image-get-pixel", "rgb?", "rgb-component?", "rgb-alpha", "rgb-blue", "rgb-green", "rgb-red", "rgb->string", "rgb", "rgb-add", "rgb-average", "rgb-bluer", "rgb-darker", "rgb-distance", "rgb-greener", "rgb-greyscale", "rgb-hue", "rgb-lighter", "rgb-phaseshift", "rgb-pseudo-complement", "rgb-redder", "rgb-rotate-components", "rgb-saturation", "rgb-subtract", "rgb-thicken", "rgb-thin", "rgb->hsv", "rgb-value"]],
    ["hsv", ["color-name->hsv", "color?", "hsv", "hsv-alpha", "hsv-complement", "hsv-hue", "hsv-saturation", "hsv->rgb", "hsv->string", "hsv-value", "hsv?", "rgb-hue", "rgb-saturation", "rgb->hsv", "rgb-value"]],
    ["shapes", ["circle", "square", "ellipse", "isosceles-triangle", "outlined-rectangle", "outlined-square", "outlined-circle", "outlined-ellipse", "outlined-isosceles-triangle", "outlined-triangle", "rectangle", "shape?", "solid-circle", "solid-ellipse", "solid-isosceles-triangle", "solid-rectangle", "solid-square", "solid-triangle", "triangle", "with-dash", "canvas-circle!", "canvas-ellipse!", "canvas-rectangle!"]],
    ["composition", ["above", "above/align", "beside", "beside/align", "overlay", "overlay/align", "overlay/offset"]],
    ["placement", ["above", "above/align", "beside", "beside/align", "overlay", "overlay/align", "overlay/offset"]],
    ["path", ["path", "canvas-path!"]],
    ["canvas", ["canvas-set-pixels!", "with-dash", "animate-with", "canvas-circle!", "canvas-drawing!", "canvas-ellipse!", "canvas-onclick!", "canvas-path!", "canvas-rectangle!", "canvas-text!", "make-canvas", "reactive-canvas"]],
    ["pixel", ["canvas-set-pixels!", "drawing->image", "drawing->pixels", "image-get-pixel", "pixel-map", "pixels->image"]],
    ["music", ["band", "composition?", "denominator", "dur", "dur?", "dynamics", "empty", "en", "hn", "instrument", "load-instrument", "load-percussion", "make-note-handlers", "mod", "mod?", "note", "note-event", "note-freq", "note-handlers", "note-value?", "numerator", "octave?", "par", "percussion", "pickup", "pitch?", "play-composition", "qn", "repeat", "rest", "seq", "sn", "tempo", "tn", "trigger", "use-high-quality-instruments", "wn"]],
    ["duration", ["denominator", "dur", "dur?", "en", "hn", "numerator", "sn", "tn", "wn"]],
    ["instruments", ["band", "instrument", "load-instrument", "load-percussion", "high-quality-instruments"]],
    ["note", ["empty", "make-note-handlers", "note", "note-event", "note-freq", "note-handlers", "note-value?", "qn", "rest"]],
    ["modifications", ["dynamics", "mod", "mod?", "octave?", "percussion", "pitch?", "tempo"]],
    ["audio", ["audio-context", "audio-file-node", "audio-pipeline", "delay-node", "oscillator-node", "play-sample", "sample-node"]],
    ["sound", ["band", "composition?", "denominator", "dur", "dur?", "dynamics", "empty", "en", "hn", "instrument", "load-instrument", "load-percussion", "make-note-handlers", "mod", "mod?", "note", "note-event", "note-freq", "note-handlers", "note-value?", "numerator", "octave?", "par", "percussion", "pickup", "pitch?", "play-composition", " qn", "repeat", "rest", "seq", "sn", "tempo", "tn", "trigger", "use-high-quality-instruments", "wn", "audio-context", "audio-file-node", "audio-pipeline", "delay-node", "oscillator-node", "play-sample", "sample-node"]],
    ["data", ["dataset?", "dataset-bar", "dataset-bubble", "dataset-line", "dataset-pie", "dataset-polar", "dataset-radar", "dataset-scatter", "parse-csv", "plot?", "plot-category", "plot-linear", "plot-radical", "string->chars", "string->lines", "string->words", "tally-all", "with-dataset-options", "with-plot-options"]],
    ["create", ["dataset-bar", "dataset-bubble", "dataset-line", "dataset-pie", "dataset-polar", "dataset?", "dataset-radar", "dataset-scatter", "plot-category", "plot-linear", "plot?", "plot-radical", "with-dataset-options", "with-plot-options"]],
    ["plot", ["plot?", "plot-category", "plot-linear", "plot-radical", "with-plot-options"]],
    ["parse", ["parse-csv"]]
    
  ])
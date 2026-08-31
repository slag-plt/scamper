; libs.scm -- a tour of the Scamper libraries, one section each.
;
; Every statement produces a value rather than starting playback or waiting for
; a click, so the file can be run start to finish and read as a transcript. Open
; it in the IDE to see the drawings, charts, and widgets rendered.
;
; Unlike showcase.scm, this file needs a browser: `canvas`, `html`, and
; `reactive` all reach for `document`, so it does not run under `npm run cli`.
;
; The `file` library is not shown here. Its functions read and write a user's
; own files, so they need a file system to talk to and there is nothing sensible
; for a sample to open.

; ---------------------------------------------------------------------------
; prelude -- always available, no import needed
; ---------------------------------------------------------------------------

(range 5)
(map #(* % %) (range 5))
(string-append "scam" "per")

; ---------------------------------------------------------------------------
; image -- shapes, combined into pictures
; ---------------------------------------------------------------------------

(import image)

; A shape is a value like any other, described by its size, whether it is
; filled, and its color.
(circle 50 "solid" "cornflowerblue")
(rectangle 80 30 "outline" "black")
(triangle 40 "solid" "seagreen")

; Pictures are built by putting shapes next to, above, or on top of each other.
(beside (circle 40 "solid" "red")
        (circle 40 "solid" "gold")
        (circle 40 "solid" "green"))

(above (rectangle 60 20 "solid" "tomato")
       (rectangle 60 20 "solid" "orange"))

(overlay (circle 20 "solid" "white")
         (circle 50 "solid" "midnightblue"))

(rotate 30 (square 50 "solid" "purple"))

(text "Scamper" 24 "black")

; A color is a value too, and can be taken apart and recombined.
(rgb 255 128 0)
(rgb-red (color-name->rgb "teal"))
(rgb-lighter (rgb 100 100 100))
(rgb->hsv (rgb 255 0 0))

; A drawing knows its own dimensions.
(drawing-width (circle 50 "solid" "red"))

; ---------------------------------------------------------------------------
; canvas -- drawing at precise coordinates
; ---------------------------------------------------------------------------

(import canvas)

; Where `image` composes shapes, `canvas` places them. Its functions are
; effectful -- each draws onto the canvas and returns nothing -- so they are
; sequenced with `begin` and the canvas itself is what is displayed.
(define c (make-canvas 200 100))

(begin
  (canvas-rectangle! c 0 0 200 100 "outline" "black")
  (canvas-circle! c 50 50 30 "solid" "orange")
  (canvas-rectangle! c 110 20 60 60 "solid" "steelblue")
  (canvas-text! c 10 95 "drawn on a canvas" 12 "solid" "black")
  c)

; ---------------------------------------------------------------------------
; data -- charts and tabular data
; ---------------------------------------------------------------------------

(import data)

(plot-category
  (list "red" "green" "blue")
  (dataset-bar "votes" (list 8 5 12)))

(plot-linear
  (dataset-line "squares" (map #(pair % (* % %)) (range 10))))

(parse-csv "name,score\nAda,95\nAlan,91")
(tally-all (list "a" "b" "a" "c" "a"))

; ---------------------------------------------------------------------------
; music -- compositions
; ---------------------------------------------------------------------------

(import music)

; A note is a MIDI number and a duration; `seq` plays compositions one after
; another and `par` plays them at the same time.
(note 60 qn)
(seq (note 60 qn) (note 62 qn) (note 64 hn))
(par (note 60 wn) (note 64 wn) (note 67 wn))
(repeat 2 (seq (note 60 en) (rest en)))

; A mod wraps a composition to change how it is played.
(mod (tempo qn 120)
     (seq (note 60 qn) (note 64 qn) (note 67 hn)))

; A composition is only a value until it is handed to `play-composition`, which
; is left to the reader so that opening this file stays quiet.

; ---------------------------------------------------------------------------
; html -- buttons, text areas, and raw elements
; ---------------------------------------------------------------------------

(import html)

; A button's callback is run for its effect, so the interesting ones change
; something the reader can see -- here, a canvas defined above it.
(define scratch (make-canvas 120 60))
scratch
(button "draw a circle"
        (lambda () (canvas-circle! scratch 30 30 25 "solid" "orchid")))

(text-area "notes")
(tag "p" "an ordinary paragraph element")

; ---------------------------------------------------------------------------
; reactive -- components that respond to events
; ---------------------------------------------------------------------------

(import reactive)

; A reactive component is a model, a view that draws it, an update that
; responds to events, and as many subscriptions as it needs. This one counts
; the clicks and key presses it receives, so it takes two.
(reactive-canvas 200 60
  0
  (lambda (st canv)
    (begin
      (canvas-rectangle! canv 0 0 200 60 "outline" "black")
      (canvas-text! canv 10 35
        (string-append "events: " (number->string st)) 16 "solid" "black")))
  (lambda (msg st)
    (match msg
      [(event-mouse-click btn x y) (+ st 1)]
      [(event-key-up key) (+ st 1)]))
  (on-mouse-click)
  (on-key-up))

; ---------------------------------------------------------------------------
; lab -- structuring a worksheet
; ---------------------------------------------------------------------------

(import lab)

(title "A Lab About Colors")
(part "Part 1: Mixing")
(problem "Write a function that averages two colors.")
(description "Use rgb-average and check it against a color you know.")

; ---------------------------------------------------------------------------
; rex -- regular expressions
; ---------------------------------------------------------------------------

(import rex)

(define digits (rex-repeat (rex-char-range #\0 #\9)))
(rex-matches? digits "12345")
(rex-find-matches (regex "[a-z]+" "g") "one two three")
(rex-split-string (regex "," "") "a,b,c")
(rex->string digits)

; ---------------------------------------------------------------------------
; test -- checking that code does what it should
; ---------------------------------------------------------------------------

(import test)

(test-case "adding two numbers"
  equal? 4
  (lambda () (+ 2 2)))

(test-case "a failing case reports what it got instead"
  equal? 5
  (lambda () (+ 2 2)))

(test-exn "dividing by a non-number raises an error"
  (lambda () (/ 1 "two")))

; ---------------------------------------------------------------------------
; audio -- synthesized sound
; ---------------------------------------------------------------------------

; The `audio` library builds sound from oscillators and filters, and needs a
; real browser audio context to do it -- so, like playback above, it is left to
; the reader rather than run here:
;
;   (import audio)
;   (define ctx (audio-context 44100))
;   (play-sample (audio-pipeline ctx (oscillator-node ctx "sine" 440)))

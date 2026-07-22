;;; (dur? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a duration.
(define dur? (js-var "music_durQ"))

;;; (dur num den) -> dur?
;;;  num : integer?
;;;  den : integer?
;;; Creates a new duration object representing the ratio `num/den`.
(define dur (js-var "music_dur"))

;;; (numerator dur) -> integer?
;;;  dur : dur?
;;; Returns the numerator of `dur`.
(define numerator (js-var "music_numerator"))

;;; (denominator dur) -> integer?
;;;  dur : dur?
;;; Returns the denominator of `dur`.
(define denominator (js-var "music_denominator"))

;;; (pitch? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid pitch, a string denoting a pitch class, e.g., `"Ab"`.
(define pitch? (js-var "music_isPitchClass"))

;;; (octave? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid octave, an integer in the range (0, 10).
(define octave? (js-var "music_isOctave"))

;;; (note-value? n) -> boolean?
;;;  n : number?
;;; Returns `#t` if and only `n` is a valid MIDI note value (0--127).
(define note-value? (js-var "music_isValidMidiNote"))

;;; (note midi-note dur) -> composition?
;;;  midi-note : note-value?
;;;  dur : dur?
;;; Creates a new composition consisting of a single note from the given MIDI note value and duration.
(define note (js-var "music_note"))

;;; (note-freq freq dur) -> composition?
;;;  freq : integer?
;;;   0 <= frequency <= 4000
;;;  dur : dur?
;;; Creates a new composition consisting of a single note of the given frequency and duration.
(define note-freq (js-var "music_noteFreq"))

;;; (repeat n comp) -> composition?
;;;  n : integer?
;;;   n >= 0
;;;  comp : composition?
;;; Creates a new composition formed by repeating `comp` `n` times sequentially.
(define repeat (js-var "music_repeat"))

;;; (empty) -> composition?
;;; The empty composition.
(define empty (js-var "music_empty"))

;;; (rest dur) -> composition?
;;;  dur : dur?
;;; Creates a new composition consisting of a single rest from the given duration.
(define rest (js-var "music_rest"))

;;; (trigger proc) -> composition?
;;;  proc : procedure?
;;;   a procedure that takes no arguments
;;; Creates a new composition that calls the function `proc` when played.
(define trigger (js-var "music_trigger"))

;;; (par . comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays `comp1`, `comp2`, ..., in parallel.
(define par (js-var "music_par"))

;;; (seq . comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays `comp1`, `comp2`, ..., in sequence.
(define seq (js-var "music_seq"))

;;; (pickup c1 c2) -> composition?
;;;  c1 : composition?
;;;  c2 : composition?
;;; Creates a new composition that plays `c2` preceded by `c1`. `c1`'s duration is not factored into the duration of the overall composition.
(define pickup (js-var "music_pickup"))

;;; (mod? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid modification.
(define mod? (js-var "music_modQ"))

;;; (percussion) -> mod?
;;; A modification that switches playback to percussion mode (MIDI channel 9). In percussion mode, each note corresponds to one percussion instrument.
(define percussion (js-var "music_percussion"))

;;; (tempo beat bpm) -> mod?
;;;  beat : dur?
;;;   the pulse of the tempo
;;;  bpm : number?
;;;   beats per minute
;;; A modification that plays the modified composition at the given `beat` and `bpm`.
(define tempo (js-var "music_tempo"))

;;; (dynamics velocity) -> mod?
;;;  velocity : integer?
;;;   0 <= level <= 127
;;; A modification that plays the modified composition at the given MIDI `velocity` level. Note than a `velocity` of `127` corresponds to full volume for that note.
(define dynamics (js-var "music_dynamics"))

;;; (instrument prog) -> composition?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Creates a new composition that plays composition `comp` played with MIDI sound or program `prog`. See the "General MIDI" Wikipedia article for a complete list of MIDI program numbers to sound mappings. Additionally, you should call `load-instrument` at the top-level of your program to download the desired instrument's soundfont before using this function.
(define instrument (js-var "music_instrument"))

;;; (note-handlers handlers) -> mod?
;;;  handlers : vector?
;;; Create a new modification that plays the composition with the given note handlers. This modification should be used at the top-level of a composition as nested handlers will take precedence over outer handlers.
(define note-handlers (js-var "music_noteHandlers"))

;;; (mod kind comp) -> composition?
;;;  kind : mod?
;;;  comp : composition?
;;; Creates a new composition that plays `comp` with the given modification `mod`.
(define mod (js-var "music_mod"))

;;; (note-event id) -> composition?
;;;  id : any
;;; Creates a new zero-length composition that triggers an event with the given `id`.
(define note-event (js-var "music_noteEvent"))

;;; (composition? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid composition.
(define composition? (js-var "music_compositionQ"))

;;; (load-instrument prog) -> void
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Downloads and loads the requested MIDI instrument soundfont.
(define load-instrument (js-var "music_loadInstrument"))

;;; (load-percussion prog) -> void
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Loads the requested percussion MIDI instrument soundfont.
(define load-percussion (js-var "music_loadPercussion"))

;;; (use-high-quality-instruments enable) -> void
;;;  enable : boolean?
;;;   whether to use high-quality MIDI instruments
;;; Enables (or disables) the use of high-quality MIDI instruments. Note that high-quality instruments are much bigger and take longer to load.
(define use-high-quality-instruments (js-var "music_useHighQualityInstruments"))

;;; (make-note-handlers) -> vector?
;;; Makes an empty vector of note handlers appropriate for use with `note-handler` and `on-note`
(define make-note-handlers (js-var "music_makeNoteHandlers"))

;;; (play-composition comp) -> void
;;;  comp : composition?
;;; Plays the given composition. Note that this function must be triggered from some user action on the screen, _e.g._, a button click. Otherwise, the browser will silently block audio playback.
(define play-composition (js-var "music_playComposition"))

;;; (wn) -> dur?
;;; A whole note duration (4/4).
(define wn (js-var "music_wn"))

;;; (hn) -> dur?
;;; A half note duration (2/4).
(define hn (js-var "music_hn"))

;;; (qn) -> dur?
;;; A quarter note duration (1/4).
(define qn (js-var "music_qn"))

;;; (en) -> dur?
;;; An eighth note duration (1/8).
(define en (js-var "music_en"))

;;; (sn) -> dur?
;;; A sixteenth note duration (1/16).
(define sn (js-var "music_sn"))

;;; (tn) -> dur?
;;; A thirty-secondth note duration (1/32).
(define tn (js-var "music_tn"))

;;; The Scamper music library, inspired from Hudak's Euterpea library for the Haskell programming language

;;; (dur? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a duration.
;;; @category duration, music, predicates, sound, typecheck, dur
(define-export dur? (js-var "music_durQ"))

;;; (dur num den) -> dur?
;;;  num : integer?
;;;  den : integer?
;;; Creates a new duration object representing the ratio `num/den`.
;;; @category duration, music, sound, dur?
(define-export dur (js-var "music_dur"))

;;; (numerator dur) -> integer?
;;;  dur : dur?
;;; Returns the numerator of `dur`.
;;; @category duration, music, sound, denominator
(define-export numerator (js-var "music_numerator"))

;;; (denominator dur) -> integer?
;;;  dur : dur?
;;; Returns the denominator of `dur`.
;;; @category duration, music, sound, numerator
(define-export denominator (js-var "music_denominator"))

;;; (pitch? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid pitch, a string denoting a pitch class, e.g., `"Ab"`.
;;; @category modifications, music, predicates, sound, typecheck, octave?
(define-export pitch? (js-var "music_isPitchClass"))

;;; (octave? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid octave, an integer in the range (0, 10).
;;; @category modifications, music, predicates, sound, typecheck, pitch?
(define-export octave? (js-var "music_isOctave"))

;;; (note-value? n) -> boolean?
;;;  n : number?
;;; Returns `#t` if and only `n` is a valid MIDI note value (0--127).
;;; @category music, note, predicates, sound, typecheck, note
(define-export note-value? (js-var "music_isValidMidiNote"))

;;; (note midi-note dur) -> composition?
;;;  midi-note : note-value?
;;;  dur : dur?
;;; Creates a new composition consisting of a single note from the given MIDI note value and duration.
;;; @category music, note, sound, note-value?, composition?, empty, instrument, mod, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export note (js-var "music_note"))

;;; (note-freq freq dur) -> composition?
;;;  freq : integer?
;;;   0 <= frequency <= 4000
;;;  dur : dur?
;;; Creates a new composition consisting of a single note of the given frequency and duration.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-event, play-composition, repeat, rest, trigger
(define-export note-freq (js-var "music_noteFreq"))

;;; (repeat n comp) -> composition?
;;;  n : integer?
;;;   n >= 0
;;;  comp : composition?
;;; Creates a new composition formed by repeating `comp` `n` times sequentially.
;;; @category music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, rest, trigger
(define-export repeat (js-var "music_repeat"))

;;; (empty) -> composition?
;;; The empty composition.
;;; @category constants, music, note, sound, composition?, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export empty (js-var "music_empty"))

;;; (rest dur) -> composition?
;;;  dur : dur?
;;; Creates a new composition consisting of a single rest from the given duration.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, trigger
(define-export rest (js-var "music_rest"))

;;; (trigger proc) -> composition?
;;;  proc : procedure?
;;;   a procedure that takes no arguments
;;; Creates a new composition that calls the function `proc` when played.
;;; @category constants, interactive, music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest
(define-export trigger (js-var "music_trigger"))

;;; (par & comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays `comp1`, `comp2`, ..., in parallel.
;;; @category music, sound, pickup, seq
(define-export par (js-var "music_par"))

;;; (seq & comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays `comp1`, `comp2`, ..., in sequence.
;;; @category music, sound, par, pickup
(define-export seq (js-var "music_seq"))

;;; (pickup c1 c2) -> composition?
;;;  c1 : composition?
;;;  c2 : composition?
;;; Creates a new composition that plays `c2` preceded by `c1`. `c1`'s duration is not factored into the duration of the overall composition.
;;; @category pickup, par, seq
(define-export pickup (js-var "music_pickup"))

;;; (mod? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid modification.
;;; @category modifications, music, predicates, sound, typecheck, dynamics, mod, note-handlers, percussion, tempo
(define-export mod? (js-var "music_modQ"))

;;; percussion: mod?
;;; A modification that switches playback to percussion mode (MIDI channel 9). In percussion mode, each note corresponds to one percussion instrument.
;;; @category constants, modifications, music, sound, load-percussion, dynamics, mod, mod?, note-handlers, tempo
(define-export percussion (js-var "music_percussion"))

;;; (tempo beat bpm) -> mod?
;;;  beat : dur?
;;;   the pulse of the tempo
;;;  bpm : number?
;;;   beats per minute
;;; A modification that plays the modified composition at the given `beat` and `bpm`.
;;; @category modifications, music, duration, sound, dynamics, mod, mod?, note-handlers, percussion
(define-export tempo (js-var "music_tempo"))

;;; (dynamics velocity) -> mod?
;;;  velocity : integer?
;;;   0 <= level <= 127
;;; A modification that plays the modified composition at the given MIDI `velocity` level. Note than a `velocity` of `127` corresponds to full volume for that note.
;;; @category modifications, music, sound, mod, mod?, note-handlers, percussion, tempo
(define-export dynamics (js-var "music_dynamics"))

;;; (instrument prog) -> composition?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Creates a new composition that plays composition `comp` played with MIDI sound or program `prog`. See the "General MIDI" Wikipedia article for a complete list of MIDI program numbers to sound mappings. Additionally, you should call `load-instrument` at the top-level of your program to download the desired instrument's soundfont before using this function.
;;; @category instruments, music, sound, load-instrument, use-high-quality-instruments, composition?, empty, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export instrument (js-var "music_instrument"))

;;; (note-handlers handlers) -> mod?
;;;  handlers : vector?
;;; Create a new modification that plays the composition with the given note handlers. This modification should be used at the top-level of a composition as nested handlers will take precedence over outer handlers.
;;; @category music, note, sound, make-note-handlers, dynamics, mod, mod?, percussion, tempo
(define-export note-handlers (js-var "music_noteHandlers"))

;;; (mod kind comp) -> composition?
;;;  kind : mod?
;;;  comp : composition?
;;; Creates a new composition that plays `comp` with the given modification `mod`.
;;; @category modifications, music, sound, dynamics, mod?, note-handlers, percussion, tempo, play-composition
(define-export mod (js-var "music_mod"))

;;; (note-event id) -> composition?
;;;  id : any
;;; Creates a new zero-length composition that triggers an event with the given `id`.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-freq, play-composition, repeat, rest, trigger
(define-export note-event (js-var "music_noteEvent"))

;;; (composition? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a valid composition.
;;; @category music, predicates, sound, typecheck, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export composition? (js-var "music_compositionQ"))

;;; (load-instrument prog) -> void?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Downloads and loads the requested MIDI instrument soundfont.
;;; @category instruments, music, sound, instrument, use-high-quality-instruments
(define-export load-instrument (js-var "music_loadInstrument"))

;;; (load-percussion prog) -> void?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Loads the requested percussion MIDI instrument soundfont.
;;; @category instruments, music, sound, percussion
(define-export load-percussion (js-var "music_loadPercussion"))

;;; (use-high-quality-instruments enable) -> void?
;;;  enable : boolean?
;;;   whether to use high-quality MIDI instruments
;;; Enables (or disables) the use of high-quality MIDI instruments. Note that high-quality instruments are much bigger and take longer to load.
;;; @category instruments, music, sound, instrument, load-instrument
(define-export use-high-quality-instruments (js-var "music_useHighQualityInstruments"))

;;; (make-note-handlers) -> vector?
;;; Makes an empty vector of note handlers appropriate for use with `note-handler` and `on-note`
;;; @category music, note, sound, note-handlers
(define-export make-note-handlers (js-var "music_makeNoteHandlers"))

;;; (play-composition comp) -> void?
;;;  comp : composition?
;;; Plays the given composition. Note that this function must be triggered from some user action on the screen, _e.g._, a button click. Otherwise, the browser will silently block audio playback.
;;; @category music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, repeat, rest, trigger
(define-export play-composition (js-var "music_playComposition"))

;;; wn: dur?
;;; A whole note duration (4/4).
;;; @category constants, duration, music, sound, hn, qn, en, sn, tn
(define-export wn (js-var "music_wn"))

;;; hn: dur?
;;; A half note duration (2/4).
;;; @category constants, duration, music, sound, wn, qn, en, sn, tn
(define-export hn (js-var "music_hn"))

;;; qn: dur?
;;; A quarter note duration (1/4).
;;; @category constants, music, note, sound, wn, hn, en, sn, tn
(define-export qn (js-var "music_qn"))

;;; en: dur?
;;; An eighth note duration (1/8).
;;; @category constants, duration, music, sound, wn, hn, qn, sn, tn
(define-export en (js-var "music_en"))

;;; sn: dur?
;;; A sixteenth note duration (1/16).
;;; @category constants, duration, music, sound, wn, hn, qn, en, tn
(define-export sn (js-var "music_sn"))

;;; tn: dur?
;;; A thirty-secondth note duration (1/32).
;;; @category duration, music, sound, wn, hn, qn, en, sn
(define-export tn (js-var "music_tn"))

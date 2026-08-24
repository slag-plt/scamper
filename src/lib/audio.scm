;;; (sample? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an audio sample.
(define-export sample? (js-var "audio_sampleQ"))

;;; (sample-node v) -> sample?
;;;  v : any
;;;   vector? of numbers between -1.0 and 1.0
;;; Returns an audio sample generated from the provided example.
;;; @category audio, sound
(define-export sample-node (js-var "audio_sampleNode"))

;;; (context? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an audio context.
(define-export context? (js-var "audio_contextQ"))

;;; (audio-context sampleRate) -> context?
;;;  sampleRate : integer?
;;;   positive
;;; Creates an audio context with the given sample rate.
;;; @category audio, sound
(define-export audio-context (js-var "audio_audioContext"))

;;; (audio-node? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an audio node.
(define-export audio-node? (js-var "audio_audioNodeQ"))

;;; (pipeline? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an audio pipeline.
(define-export pipeline? (js-var "audio_pipelineQ"))

;;; (audio-pipeline ctx pipeline & n1) -> pipeline?
;;;  ctx : context?
;;;  pipeline : (or/p audio-node? sample?)
;;;  n1 : (or/p audio-node? sample?)
;;; Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.
;;; The first argument is what the pipeline plays and may be a sample; the rest are what
;;; it plays through, and must be audio nodes. A sample is played at `ctx`'s sample rate,
;;; one frame per element, so the context decides its pitch and duration.
;;; @category audio, sound
(define-export audio-pipeline (js-var "audio_audioPipeline"))

;;; (oscillator-node ctx type freq) -> audio-node?
;;;  ctx : context?
;;;  type : string?
;;;  freq : number?
;;;   positive
;;; Creates an oscillator node with the given type and frequency.
;;; @category audio, sound
(define-export oscillator-node (js-var "audio_oscillatorNode"))

;;; (audio-file-node ctx path) -> audio-node?
;;;  ctx : context?
;;;  path : string?
;;; Creates an audio source node connected to the audio file at the given path.
;;; @category audio, sound
(define-export audio-file-node (js-var "audio_audioFileNode"))

;;; (delay-node ctx delay) -> audio-node?
;;;  ctx : context?
;;;  delay : number?
;;;   positive
;;; Creates a delay node with the given delay time.
;;; @category audio, sound
(define-export delay-node (js-var "audio_delayNode"))

;;; (play-sample sample) -> void?
;;;  sample : sample?
;;; Plays the given audio sample. Note that due to browser limitations, the call to this function must be guarded by user input, _e.g._, by invoking it with a button press.
;;; @category audio, sound
(define-export play-sample (js-var "audio_playSample"))

;;; (sample-node v) -> sample?
;;;  v : any
;;;   vector? of numbers between -1.0 and 1.0
;;; Returns an audio sample generated from the provided example.
(define sample-node (js-var "audio_sampleNode"))
;;; (audio-context sampleRate) -> context?
;;;  sampleRate : integer?
;;;   positive
;;; Creates an audio context with the given sample rate.
(define audio-context (js-var "audio_audioContext"))
;;; (audio-pipeline ctx n1) -> pipeline?
;;;  ctx : context?
;;;  n1 : audio-node?
;;; Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.
(define audio-pipeline (js-var "audio_audioPipeline"))
;;; (oscillator-node ctx type freq) -> node?
;;;  ctx : context?
;;;  type : string?
;;;  freq : number?
;;;   positive
;;; Creates an oscillator node with the given type and frequency.
(define oscillator-node (js-var "audio_oscillatorNode"))
;;; (audio-file-node ctx path) -> node?
;;;  ctx : context?
;;;  path : string?
;;; Creates an audio source node connected to the audio file at the given path.
(define audio-file-node (js-var "audio_audioFileNode"))
;;; (delay-node ctx delay) -> node?
;;;  ctx : context?
;;;  delay : number?
;;;   positive
;;; Creates a delay node with the given delay time.
(define delay-node (js-var "audio_delayNode"))
;;; (play-sample sample) -> void?
;;;  sample : audio?
;;; Plays the given audio sample. Note that due to browser limitations, the call to this function must be guarded by user input, _e.g._, by invoking it with a button press.
(define play-sample (js-var "audio_playSample"))

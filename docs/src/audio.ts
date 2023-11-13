import Doc from './docs.js'

export const image: Doc = new Doc(
  '(sample-node v): sample?', [
    'v: vector? of numbers between -1.0 and 1.0'
  ],
  'Returns an audio sample generated from the provided example.'
)

export const audioContextDoc: Doc = new Doc(
  '(audio-context sampleRate) -> context?', [
    'sampleRate: integer?, positive'
  ],
  'Creates an audio context with the given sample rate.'
)

export const audioPipelineDoc: Doc = new Doc(
  '(audio-pipeline ctx n1 ... nk) -> pipeline?', [
    'ctx: context?',
    'n1 ... nk: audio-node?'
  ],
  'Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.'
)

export const oscillatorNodeDoc: Doc = new Doc(
  '(oscillator-node ctx type freq) -> node?', [
    'ctx: context?',
    'type: string?',
    'freq: number?, positive'
  ],
  'Creates an oscillator node with the given type and frequency.'
)

// const microphoneNodeDoc: Doc = new Doc(
//   '(microphone-node ctx) -> node?', [
//     'ctx: context?'
//   ],
//   'Creates an audio source node connected to the user\'s microphone.'
// )

export const audioFileNodeDoc: Doc = new Doc(
  '(audio-file-node ctx path) -> node?', [
    'ctx: context?',
    'path: string?'
  ],
  'Creates an audio source node connected to the audio file at the given path.'
)

export const delayNodeDoc: Doc = new Doc(
  '(delay-node ctx delay) -> node?', [
    'ctx: context?',
    'delay: number?, positive'
  ],
  'Creates a delay node with the given delay time.'
)

export const playSampleDoc: Doc = new Doc(
  '(play-sample sample) -> void?', [
    'sample: audio?'
  ],
  'Plays the given audio sample. Note that due to browser limitations, the call to this function must be guarded by user input, _e.g._, by invoking it with a button press.'
)
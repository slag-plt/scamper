import {ArgDoc, Doc} from './docs.js'

export const sampleNodeDoc: Doc = new Doc(
  'sample-node',
  'sample?',
  [ new ArgDoc('v', 'vector? of numbers between -1.0 and 1.0') ],
  'Returns an audio sample generated from the provided example.'
)

export const audioContextDoc: Doc = new Doc(
  'audio-context',
  'context?',
  [ new ArgDoc('sampleRate', 'integer?, positive') ],
  'Creates an audio context with the given sample rate.'
)

export const audioPipelineDoc: Doc = new Doc(
  'audio-pipeline',
  'pipeline?',
  [ new ArgDoc('ctx', 'context?'), new ArgDoc('n1 ... nk', 'audio-node?') ],
  'Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.'
)

export const oscillatorNodeDoc: Doc = new Doc(
  'oscillator-node',
  'node?',
  [ 
    new ArgDoc('ctx', 'context?'),
    new ArgDoc('type', 'string?'),
    new ArgDoc('freq', 'number?, positive')
  ],
  'Creates an oscillator node with the given type and frequency.'
)

export const audioFileNodeDoc: Doc = new Doc(
  'audio-file-node',
  'node?',
  [ new ArgDoc('ctx', 'context?'), new ArgDoc('path', 'string?') ],
  'Creates an audio source node connected to the audio file at the given path.'
)

export const delayNodeDoc: Doc = new Doc(
  'delay-node',
  'node?',
  [ new ArgDoc('ctx', 'context?'), new ArgDoc('delay', 'number?, positive') ],
  'Creates a delay node with the given delay time.'
)

export const playSampleDoc: Doc = new Doc(
  'play-sample',
  'void?',
  [ new ArgDoc('sample', 'audio?') ],
  'Plays the given audio sample. Note that due to browser limitations, the call to this function must be guarded by user input, _e.g._, by invoking it with a button press.'
)
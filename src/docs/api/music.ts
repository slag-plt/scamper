import { ArgDoc, Doc } from './docs.js'

export const pitch: Doc = new Doc(
  'pitch?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a valid pitch, a string denoting a pitch class, e.g., `"Ab"`.'
)

export const octave: Doc = new Doc(
  'octave?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a valid octave, an integer in the range (0, 10).'
)

export const durQ: Doc = new Doc(
  'dur?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a valid duration object.'
)

export const dur: Doc = new Doc(
  'dur',
  'duration?',
  [ new ArgDoc('num', 'integer?'), new ArgDoc('den', 'integer?') ],
  'Creates a new duration object representing the ratio `num/den`.'
)

export const numerator: Doc = new Doc(
  'numerator',
  'integer?',
  [ new ArgDoc('dur', 'duration?') ],
  'Returns the numerator of `dur`.'
)

export const denominator: Doc = new Doc(
  'denominator',
  'integer?',
  [ new ArgDoc('dur', 'duration?') ],
  'Returns the denominator of `dur`.'
)

export const empty: Doc = new Doc(
  'empty',
  'composition?',
  [],
  'The empty composition.'
)

export const note: Doc = new Doc(
  'note',
  'composition?',
  [ new ArgDoc('midi-note', 'note-value?'), new ArgDoc('dur', 'dur?') ],
  'Creates a new composition consisting of a single note from the given MIDI note value and duration.'
)

export const noteValueQ: Doc = new Doc(
  'note-value?',
  'boolean?',
  [ new ArgDoc('n', 'number') ],
  'Returns `#t` if and only `n` is a valid MIDI note value (0--127).'
)

export const noteFreq: Doc = new Doc(
  'note-freq',
  'composition?',
  [ new ArgDoc('freq', 'integer?, 0 <= frequency <= 4000'), new ArgDoc('dur', 'dur?') ],
  'Creates a new composition consisting of a single note of the given frequency and duration.'
)

export const rest: Doc = new Doc(
  'rest',
  'composition?',
  [ new ArgDoc('dur', 'dur?') ],
  'Creates a new composition consisting of a single rest from the given duration.'
)

export const par: Doc = new Doc(
  'par',
  'composition?',
  [ new ArgDoc('comp1, comp2, ...', 'composition?') ],
  'Creates a new composition that plays `comp1`, `comp2`, ..., in parallel.'
)

export const seq: Doc = new Doc(
  'seq',
  'composition?',
  [ new ArgDoc('comp1, comp2, ...', 'composition?') ],
  'Creates a new composition that plays `comp1`, `comp2`, ..., in sequence.'
)

export const pickup: Doc = new Doc(
  'pickup',
  'composition?',
  [ new ArgDoc('c1', 'composition?'), new ArgDoc('c2', 'composition?') ],
  'Creates a new composition that plays `c2` preceded by `c1`. `c1`\'s duration is not factored into the duration of the overall composition.'
)

export const mod: Doc = new Doc(
  'mod',
  'composition?',
  [ new ArgDoc('kind', 'mod?'), new ArgDoc('comp', 'composition?') ],
  'Creates a new composition that plays `comp` with the given modification `mod`.'
)

export const modQ: Doc = new Doc(
  'mod?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a valid modification.'
)

export const compositionQ: Doc = new Doc(
  'composition?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a valid composition.'
)

export const band: Doc = new Doc(
  'band',
  'composition?',
  [ new ArgDoc('inst', 'number?, a valid MIDI instrument program number (1--128)') ],
  'Creates a new composition that plays `comp` with the given instruments, where the _i_th instrument is assigned to the _i_th MIDI channel. Individual channels can be selected for playback using the `instrument` mod.'
)

export const instrument: Doc = new Doc(
  'instrument',
  'composition?',
  [ new ArgDoc('prog', 'integer?, a valid MIDI program number (0--127)') ],
  'Creates a new composition that plays composition `comp` played with MIDI sound or program `prog`. See the "General MIDI" Wikipedia article for a complete list of MIDI program numbers to sound mappings. Additionally, you should call `load-instrument` at the top-level of your program to download the desired instrument\'s soundfont before using this function.'
)

export const trigger: Doc = new Doc(
  'trigger',
  'composition?',
  [ new ArgDoc('proc', 'procedure?, a procedure that takes no arguments') ],
  'Creates a new composition that calls the function `proc` when played.'
)

// export const onnote: Doc = new Doc(
//   'onnote',
//   'composition?',
//   [ new ArgDoc('fn', 'function?, a function of zero arguments that returns void.') ],
//   '`fn` is called when the modded composition begins to play. _(Currently not implemented.)_'
// )

export const percussion: Doc = new Doc(
  'percussion',
  'mod?',
  [],
  'A modification that switches playback to percussion mode (MIDI channel 9). In percussion mode, each note corresponds to one percussion instrument.'
)

// export const bend: Doc = new Doc(
//   'bend',
//   'mod?',
//   [ new ArgDoc('semitones', 'number?, -1 <= semitones <= 1') ],
//   'A modification that bends the pitch of the modified composition by the given number of semitones.'
// )

export const tempo: Doc = new Doc(
  'tempo',
  'mod?',
  [
    new ArgDoc('beat', 'dur?, the pulse of the tempo'),
    new ArgDoc('bpm', 'number?, beats per minute')
  ],
  'A modification that plays the modified composition at the given `beat` and `bpm`.'
)

export const dynamics: Doc = new Doc(
  'dynamics',
  'mod?',
  [
    new ArgDoc('velocity', 'integer?, 0 <= level <= 127'),
  ],
  'A modification that plays the modified composition at the given MIDI `velocity` level. Note than a `velocity` of `127` corresponds to full volume for that note.'
)

export const repeat: Doc = new Doc(
  'repeat',
  'composition?',
  [ new ArgDoc('n', 'integer?, n >= 0'), new ArgDoc('comp', 'composition?') ],
  'Creates a new composition formed by repeating `comp` `n` times sequentially.'
)

export const wn: Doc = new Doc(
  'wn',
  'dur?',
  [],
  'A whole note duration (4/4).'
)

export const hn: Doc = new Doc(
  'hn',
  'dur?',
  [],
  'A half note duration (2/4).'
)

export const qn: Doc = new Doc(
  'qn',
  'dur?',
  [],
  'A quarter note duration (1/4).'
)

export const en: Doc = new Doc(
  'en',
  'dur?',
  [],
  'An eighth note duration (1/8).'
)

export const sn: Doc = new Doc(
  'sn',
  'dur?',
  [],
  'A sixteenth note duration (1/16).'
)

export const tn: Doc = new Doc(
  'tn',
  'dur?',
  [],
  'A thirty-secondth note duration (1/32).'
)

export const playComposition: Doc = new Doc(
  'play-composition',
  'void',
  [ new ArgDoc('comp', 'composition?') ],
  'Plays the given composition. Note that this function must be triggered from some user action on the screen, _e.g._, a button click. Otherwise, the browser will silently block audio playback.'
)

export const loadInstrument: Doc = new Doc(
  'load-instrument',
  'void',
  [ new ArgDoc('prog', 'integer?, a valid MIDI program number (0--127)') ],
  'Downloads and loads the requested MIDI instrument soundfont.'
)

export const loadPercussion: Doc = new Doc(
  'load-percussion',
  'void',
  [ new ArgDoc('prog', 'integer?, a valid MIDI program number (0--127)') ],
  'Loads the requested percussion MIDI instrument soundfont.'
)

export const useHighQualityInstruments: Doc = new Doc(
  'use-high-quality-instruments',
  'void',
  [ new ArgDoc('enable', 'boolean?, whether to use high-quality MIDI instruments') ],
  'Enables (or disables) the use of high-quality MIDI instruments. Note that high-quality instruments are much bigger and take longer to load.'
)
import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'
import HtmlRenderer from '../lpm/renderers/html-renderer.js'
import { waf } from './webaudiofont/webaudiofont.js'

const Music: L.Library = new L.Library()

export type PitchClass = string
export type Octave = number

export interface Duration extends L.Struct {
  [L.structKind] : 'dur',
  numerator : number,
  denominator : number
}

function dur (numerator: number, denominator: number): Duration {
  checkContract(arguments, contract('dur', [C.number, C.number]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'dur', numerator, denominator }
}
Music.registerValue('dur', dur)

function numerator (dur: Duration): number {
  checkContract(arguments, contract('numerator', [durC]))
  return dur.numerator
}
Music.registerValue('numerator', numerator)

function denominator (dur: Duration): number {
  checkContract(arguments, contract('denominator', [durC]))
  return dur.denominator
}
Music.registerValue('denominator', denominator)

Music.registerValue('wn', dur(1, 1))
Music.registerValue('hn', dur(1, 2))
Music.registerValue('qn', dur(1, 4))
Music.registerValue('en', dur(1, 8))
Music.registerValue('sn', dur(1, 16))
Music.registerValue('tn', dur(1, 32))

function isPitchClass (s: string): boolean {
  checkContract(arguments, contract('pitch?', [C.string]))
  return /^[A-Ga-g][#b]{0,2}$/.test(s)
}
Music.registerValue('pitch?', isPitchClass)

function isOctave (n: number): boolean {
  checkContract(arguments, contract('octave?', [C.number]))
  return n >= 0 && n <= 10
}
Music.registerValue('octave?', isOctave)

function isValidMidiNote (n: number): boolean {
  checkContract(arguments, contract('note?', [C.number]))
  return n >= 0 && n <= 127
}
Music.registerValue('note-value?', isValidMidiNote)

const durC: C.Spec = {
  predicate: (v: any) => L.isStructKind(v, 'dur'),
  errorMsg: (actual: any) => `expected a duration, received ${L.typeOf(actual)}`,
}

const noteC: C.Spec = {
  predicate: (v: any) => isValidMidiNote(v),
  errorMsg: (actual: any) => `expected a midi note (0--127), received ${L.typeOf(actual)}`,
}

export interface Note extends L.Struct { [L.structKind]: 'note', note: number, duration: Duration }
function note (note: number, duration: Duration): Note {
  checkContract(arguments, contract('note', [noteC, durC]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note', note, duration }
}
Music.registerValue('note', note)

export interface NoteFreq extends L.Struct {
  [L.structKind]: 'note-freq',
  freq: number,
  duration: Duration
}
function noteFreq (freq: number, duration: Duration): NoteFreq {
  checkContract(arguments, contract('note-freq', [C.number, durC]))  
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note-freq', freq, duration }
}
Music.registerValue('note-freq', noteFreq)

function repeat (n: number, composition: Composition): Composition {
  checkContract(arguments, contract('repeat', [C.nat, compositionC]))
  if (n === 0) {
    return empty()
  } else {
    return seq(composition, repeat(n - 1, composition))
  }
}
Music.registerValue('repeat', repeat)

interface Empty extends L.Struct { [L.structKind]: 'empty' }
const empty = (): Empty => ({ [L.scamperTag]: 'struct', [L.structKind]: 'empty' })
Music.registerValue('empty', empty)

interface Rest extends L.Struct { [L.structKind]: 'rest', duration: Duration }
function rest (duration: Duration): Rest {
  checkContract(arguments, contract('rest', [durC]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'rest', duration }
}
Music.registerValue('rest', rest)

interface Trigger extends L.Struct { [L.structKind]: 'trigger', fn: L.ScamperFn }
function trigger (fn: L.ScamperFn): Trigger {
  checkContract(arguments, contract('trigger', [C.func]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'trigger', fn }
}
Music.registerValue('trigger', trigger)

interface Par extends L.Struct { [L.structKind]: 'par', notes: Composition[] }
function par (...notes: Composition[]): Par {
  checkContract(arguments, contract('par', [], compositionC))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'par', notes }
}
Music.registerValue('par', par)

interface Seq extends L.Struct { [L.structKind]: 'seq', notes: Composition[] }
function seq (...notes: Composition[]): Seq {
  checkContract(arguments, contract('seq', [], compositionC)) 
  return { [L.scamperTag]: 'struct', [L.structKind]: 'seq', notes }
}
Music.registerValue('seq', seq)

interface Pickup extends L.Struct { [L.structKind]: 'pickup', pickup: Composition, notes: Composition }
function pickup (pickup: Composition, notes: Composition): Composition {
  checkContract(arguments, contract('pickup', [compositionC, compositionC]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'pickup', pickup, notes }
}
Music.registerValue('pickup', pickup)

/** TODO: we're missing on-note? */

type ModKind = Percussion | Tempo | Dynamics | Instrument | NoteHandlersMod

function modQ (v: any): boolean {
  return L.isStructKind(v, 'percussion') ||
    //R.isStructKind(v, 'pitchBend') ||
    L.isStructKind(v, 'tempo') ||
    L.isStructKind(v, 'dynamics') ||
    L.isStructKind(v, 'instrument') ||
    L.isStructKind(v, 'noteHandlers')
}
Music.registerValue('mod?', modQ)

const modC: C.Spec = {
  predicate: modQ,
  errorMsg: (actual: any) => `expected a mod, received ${L.typeOf(actual)}`,
}

interface Percussion extends L.Struct { kind: 'percussion' }
const percussion = ({ [L.scamperTag]: 'struct', [L.structKind]: 'percussion' })
Music.registerValue('percussion', percussion)

// TODO: need to implement bends again!

// interface PitchBend extends R.Struct { [R.structKind]: 'pitchBend', amount: number }
// function pitchBend (amount: number): PitchBend {
//   checkContract(arguments, contract('bend', [C.numRange(-1, 1)]))
//   return { [R.scamperTag]: 'struct', [R.structKind]: 'pitchBend', amount }
// }
// Music.registerValue('bend', pitchBend)

interface Tempo extends L.Struct { [L.structKind]: 'tempo', beat: Duration, bpm: number }
function tempo (beat: Duration, bpm: number): Tempo {
  checkContract(arguments, contract('tempo', [durC, C.nonneg]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'tempo', beat, bpm }
}
Music.registerValue('tempo', tempo)

interface Dynamics extends L.Struct { [L.structKind]: 'dynamics', amount: number }
function dynamics (amount: number): Dynamics {
  checkContract(arguments, contract('dynamics', [C.numRange(0, 127)])) 
  return { [L.scamperTag]: 'struct', [L.structKind]: 'dynamics', amount }
}
Music.registerValue('dynamics', dynamics)

interface Instrument extends L.Struct { [L.structKind]: 'instrument', program: number }
function instrument (program: number): Instrument {
  checkContract(arguments, contract('instrument', [C.numRange(0, 127)]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'instrument', program }
}
Music.registerValue('instrument', instrument)

interface NoteHandlersMod extends L.Struct { [L.structKind]: 'noteHandlers', handlers: NoteHandlers }
function noteHandlers (handlers: NoteHandlers): NoteHandlersMod {
  checkContract(arguments, contract('note-handlers', [C.vector]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'noteHandlers', handlers }
}
Music.registerValue('note-handlers', noteHandlers)

interface Mod extends L.Struct { [L.structKind]: 'mod', note: Composition, mod: ModKind }
function mod (mod: ModKind, note: Composition): Mod {
  checkContract(arguments, contract('mod', [modC, compositionC]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'mod', note, mod }
}
Music.registerValue('mod', mod)

interface NoteEvent extends L.Struct { [L.structKind]: 'note-event', id: string }
function noteEvent (id: string): NoteEvent {
  checkContract(arguments, contract('note-event', [C.string]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note-event', id }
}
Music.registerValue('note-event', noteEvent)

export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod | NoteEvent

function compositionQ (v: any): boolean {
  return L.isStructKind(v, 'empty') ||
    L.isStructKind(v, 'note') ||
    L.isStructKind(v, 'note-freq') ||
    L.isStructKind(v, 'rest') ||
    L.isStructKind(v, 'trigger') ||
    L.isStructKind(v, 'par') ||
    L.isStructKind(v, 'seq') ||
    L.isStructKind(v, 'pickup') ||
    L.isStructKind(v, 'mod') ||
    L.isStructKind(v, 'note-event')
}
Music.registerValue('composition?', compositionQ)

const compositionC: C.Spec = {
  predicate: compositionQ,
  errorMsg: (actual: any) => `expected a composition, received ${L.typeOf(actual)}`,
}

function loadInstrument(n: number): void {
  C.checkContract(arguments, contract('load-instrument', [C.nat]))
  waf().loadInstrument(n)
}
Music.registerValue('load-instrument', loadInstrument)

function loadPercussion(n: number): void {
  C.checkContract(arguments, contract('load-percussion', [C.nat]))
  waf().loadInstrument(n, true)
}
Music.registerValue('load-percussion', loadPercussion)

function useHighQualityInstruments(enable: boolean): void {
  C.checkContract(arguments, contract('use-high-quality-instruments', [C.boolean]))
  if (enable) {
    waf().fontName = 'FluidR3_GM'
  } else {
    waf().fontName = 'Chaos'
  }
}
Music.registerValue('use-high-quality-instruments', useHighQualityInstruments)

/***** Reactive Events ********************************************************/

export interface NoteMsg extends L.Struct {
  [L.structKind]: 'event-note',
  id: string
}

export type NoteHandlers = ((note: NoteMsg) => void)[]

function makeNoteHandlers (): NoteHandlers {
  checkContract(arguments, contract('make-note-handlers', []))
  return []
}
Music.registerValue('make-note-handlers', makeNoteHandlers)

/***** Rendering **************************************************************/

type MidiMsg = {
  tag: 'midi',
  time: number,
  duration: number,
  note: number,
  instrument: number,
  velocity: number
}

type TriggerMsg = {
  tag: 'trigger',
  time: number,
  callback: L.ScamperFn
}

type EventMsg = {
  tag: 'event',
  time: number,
  id: string,
  handlers: NoteHandlers
}

type Msg = MidiMsg | TriggerMsg | EventMsg

const midiMsg = (time: number, duration: number, note: number, instrument: number, velocity: number): Msg =>
  ({ tag: 'midi', time, duration, note, instrument, velocity })

const triggerMsg = (time: number, callback: L.ScamperFn): Msg =>
  ({ tag: 'trigger', time, callback })

const eventMsg = (time: number, id: string, handlers: NoteHandlers): Msg =>
  ({ tag: 'event', time, id, handlers })

function ratioToDouble (ratio: Duration) {
  return ratio.numerator / ratio.denominator
}

function durationToTimeMs (beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function freqToNote (freq: number): number {
  return Math.log2(freq / 440) * 12 + 69
}

function compositionToMsgs (
  beat: Duration, bpm: number, velocity: number, startTime: number,
  instrument: number, handlers: NoteHandlers, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition[L.structKind]) {
    case 'empty':
      return { endTime: startTime, msgs: [] }

    case 'note': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime,
        msgs: [
          midiMsg(
            startTime,
            durationToTimeMs(beat, bpm, composition.duration),
            composition.note,
            instrument,
            velocity / 127
          )
        ]
      }
    }

    case 'note-freq': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime,
        msgs: [
          midiMsg(
            startTime,
            durationToTimeMs(beat, bpm, composition.duration),
            freqToNote(composition.freq),
            instrument,
            velocity / 127
          )
        ]
      }
    }

    case 'rest':
      return {
        endTime: startTime + durationToTimeMs(beat, bpm, composition.duration),
        msgs: []
      }

    case 'trigger': {
      return {
        endTime: startTime,
        msgs: [triggerMsg(startTime, composition.fn)]
      }
    }

    case 'par': {
      const msgs: Msg[] = []
      let endTime = 0
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, startTime, instrument, handlers, note)
        msgs.push(...result.msgs)
        endTime = Math.max(result.endTime, endTime)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime, msgs }
    }

    case 'seq': {
      const msgs: Msg[] = []
      let time = startTime
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, time, instrument, handlers, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, instrument, handlers, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, instrument, handlers, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, instrument, handlers, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod[L.structKind] === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 128, handlers, composition.note)
      } else if (composition.mod[L.structKind] === 'pitchBend') {
        const msgs: Msg[] = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, instrument, handlers, composition.note)
        // TODO: handle pitch bends
        // msgs.push(midiMsg(startTime, pitchBendF(0, composition.mod.fields[0])))
        // msgs.push(...data.msgs)
        // msgs.push(midiMsg(data.endTime, pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod[L.structKind] === 'tempo') {
        return compositionToMsgs(composition.mod.beat, composition.mod.bpm, velocity, startTime, instrument, handlers, composition.note)
      } else if (composition.mod[L.structKind] === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.amount, startTime, instrument, handlers, composition.note)
      } else if (composition.mod[L.structKind] === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.program, handlers, composition.note)
      } else if (composition.mod[L.structKind] === 'noteHandlers') {
        return compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.mod.handlers, composition.note)
      } else {
        throw new L.ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }

    case 'note-event': {
      return {
        endTime: startTime,
        msgs: [eventMsg(startTime, composition.id, handlers)]
      }
    }
  }
}

export function playComposition (composition: Composition): number {
  checkContract(arguments, contract('play-composition', [compositionC]))
  const msgs = compositionToMsgs(dur(1, 4), 120, 64, 0, 0, [], composition).msgs
  const events = msgs.filter(msg => msg.tag === 'trigger' || msg.tag === 'event') as (TriggerMsg | EventMsg)[]
  const startTime = waf().audioContext.currentTime

  // Enqueue notes
  for (const msg of msgs) {
    // const elapsed = audioContext.currentTime - startTime
    if (msg.tag === 'midi') {
      const isPercussion = msg.instrument === 128
      const instr = isPercussion ? msg.note : msg.instrument
      waf().player.queueWaveTable(waf().audioContext,
        waf().audioContext.destination,
        waf().getInstrument(instr, isPercussion),
        startTime + msg.time / 1000,
        msg.note,
        msg.duration / 1000,
        msg.velocity)
    }
  }

  // Set up a timer to discharge triggers and events
  let idx = 0
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    // N.B., in milliseconds
    const now = waf().audioContext.currentTime
    while (idx < events.length) {
      const ev = events[idx]
      if (ev.time / 1000 + startTime <= now) {
        try {
          if (ev.tag === 'trigger') {
            L.callScamperFn(ev.callback, [])
          } else {
            ev.handlers.forEach(handler => {
              handler({ [L.scamperTag]: 'struct', [L.structKind]: 'event-note', id: ev.id })
            })
          }
        } catch (e) {
          alert(`Error in composition playback:\n\n${(e as Error).toString()}`)
          clearInterval(id)
          return
        }
        idx += 1
      } else {
        // N.B., the next trigger is not ready, so bail for now.
        break
      }
    }
    // Once we exhaust our triggers and events, stop the interval
    if (idx >= events.length) {
      clearInterval(id)
    }
  })
  return id
}
Music.registerValue('play-composition', playComposition)

export function render (v: any): HTMLElement {
  const composition: Composition = v as Composition
  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    if (waf().audioContext.state === 'suspended') {
      waf().audioContext.resume().catch(console.error)
    }
    timer = playComposition(composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
      waf().player.cancelQueue(waf().audioContext)
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}

HtmlRenderer.registerCustomRenderer(compositionQ, render)

Music.initializer = function initializer(): void {
  console.log('Initializing music library...')
  // Initialize webaudiofont
  waf()

  // Pre-load common, low bandwidth instruments
  waf().loadInstrument(0)         // 0: Acoustic Grand Piano
  waf().loadInstrument(35, true)  // 35: Acoustic Bass
  waf().loadInstrument(38, true)  // 38: Acoustic Snare
  waf().loadInstrument(42, true)  // 42: Closed Hi-Hat
  waf().loadInstrument(49, true)  // 49: Crash Cymbal 1
}

export default Music
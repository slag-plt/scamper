import * as L from '../../lpm'
import { waf } from './webaudiofont/webaudiofont.js'

export type PitchClass = string
export type Octave = number

export interface Duration extends L.Struct {
  [L.structKind] : 'dur',
  numerator : number,
  denominator : number
}

export function music_dur(numerator: number, denominator: number): Duration {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'dur', numerator, denominator }
}

export function music_numerator(dur: Duration): number {
  return dur.numerator
}

export function music_denominator(dur: Duration): number {
  return dur.denominator
}

export const music_wn = music_dur(1, 1)
export const music_hn = music_dur(1, 2)
export const music_qn = music_dur(1, 4)
export const music_en = music_dur(1, 8)
export const music_sn = music_dur(1, 16)
export const music_tn = music_dur(1, 32)

export function music_isPitchClass(s: string): boolean {
  return /^[A-Ga-g][#b]{0,2}$/.test(s)
}

export function music_isOctave(n: number): boolean {
  return n >= 0 && n <= 10
}

export function music_isValidMidiNote(n: number): boolean {
  return n >= 0 && n <= 127
}

export function music_durQ(v: any): boolean {
  return L.isStructKind(v, 'dur')
}

export interface Note extends L.Struct { [L.structKind]: 'note', note: number, duration: Duration }
export function music_note(note: number, duration: Duration): Note {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note', note, duration }
}

export interface NoteFreq extends L.Struct {
  [L.structKind]: 'note-freq',
  freq: number,
  duration: Duration
}
export function music_noteFreq(freq: number, duration: Duration): NoteFreq {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note-freq', freq, duration }
}

export function music_repeat(n: number, composition: Composition): Composition {
  if (n === 0) {
    return music_empty()
  } else {
    return music_seq(composition, music_repeat(n - 1, composition))
  }
}

interface Empty extends L.Struct { [L.structKind]: 'empty' }
export const music_empty = (): Empty => ({ [L.scamperTag]: 'struct', [L.structKind]: 'empty' })

interface Rest extends L.Struct { [L.structKind]: 'rest', duration: Duration }
export function music_rest(duration: Duration): Rest {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'rest', duration }
}

interface Trigger extends L.Struct { [L.structKind]: 'trigger', fn: L.ScamperFn }
export function music_trigger(fn: L.ScamperFn): Trigger {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'trigger', fn }
}

interface Par extends L.Struct { [L.structKind]: 'par', notes: Composition[] }
export function music_par(...notes: Composition[]): Par {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'par', notes }
}

interface Seq extends L.Struct { [L.structKind]: 'seq', notes: Composition[] }
export function music_seq(...notes: Composition[]): Seq {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'seq', notes }
}

interface Pickup extends L.Struct { [L.structKind]: 'pickup', pickup: Composition, notes: Composition }
export function music_pickup(pickup: Composition, notes: Composition): Composition {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'pickup', pickup, notes }
}

/** TODO: we're missing on-note? */

type ModKind = Percussion | Tempo | Dynamics | Instrument | NoteHandlersMod

export function music_modQ(v: any): boolean {
  return L.isStructKind(v, 'percussion') ||
    //R.isStructKind(v, 'pitchBend') ||
    L.isStructKind(v, 'tempo') ||
    L.isStructKind(v, 'dynamics') ||
    L.isStructKind(v, 'instrument') ||
    L.isStructKind(v, 'noteHandlers')
}

interface Percussion extends L.Struct { [L.structKind]: 'percussion' }
export const music_percussion = ({ [L.scamperTag]: 'struct', [L.structKind]: 'percussion' })

// TODO: need to implement bends again!

// interface PitchBend extends R.Struct { [R.structKind]: 'pitchBend', amount: number }
// function pitchBend (amount: number): PitchBend {
//   //   return { [R.scamperTag]: 'struct', [R.structKind]: 'pitchBend', amount }
// }
// Music.registerValue('bend', pitchBend)

interface Tempo extends L.Struct { [L.structKind]: 'tempo', beat: Duration, bpm: number }
export function music_tempo(beat: Duration, bpm: number): Tempo {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'tempo', beat, bpm }
}

interface Dynamics extends L.Struct { [L.structKind]: 'dynamics', amount: number }
export function music_dynamics(amount: number): Dynamics {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'dynamics', amount }
}

interface Instrument extends L.Struct { [L.structKind]: 'instrument', program: number }
export function music_instrument(program: number): Instrument {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'instrument', program }
}

interface NoteHandlersMod extends L.Struct { [L.structKind]: 'noteHandlers', handlers: NoteHandlers }
export function music_noteHandlers(handlers: NoteHandlers): NoteHandlersMod {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'noteHandlers', handlers }
}

interface Mod extends L.Struct { [L.structKind]: 'mod', note: Composition, mod: ModKind }
export function music_mod(mod: ModKind, note: Composition): Mod {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'mod', note, mod }
}

interface NoteEvent extends L.Struct { [L.structKind]: 'note-event', id: string }
export function music_noteEvent(id: string): NoteEvent {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'note-event', id }
}

export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod | NoteEvent

export function music_compositionQ (v: any): boolean {
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

export function music_loadInstrument(n: number): void {
  waf()!.loadInstrument(n)
}

export function music_loadPercussion(n: number): void {
  waf()!.loadInstrument(n, true)
}

export function music_useHighQualityInstruments(enable: boolean): void {
  if (enable) {
    waf()!.fontName = 'FluidR3_GM'
  } else {
    waf()!.fontName = 'Chaos'
  }
}

/***** Reactive Events ********************************************************/

export interface NoteMsg extends L.Struct {
  [L.structKind]: 'event-note',
  id: string
}

export type NoteHandlers = ((note: NoteMsg) => void)[]

export function music_makeNoteHandlers(): NoteHandlers {
  return []
}

/***** Rendering **************************************************************/

interface MidiMsg {
  tag: 'midi',
  time: number,
  duration: number,
  note: number,
  instrument: number,
  velocity: number
}

interface TriggerMsg {
  tag: 'trigger',
  time: number,
  callback: L.ScamperFn
}

interface EventMsg {
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
      const kind = composition.mod
      if (kind[L.structKind] === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 128, handlers, composition.note)
      // } else if (mod[L.structKind] === 'pitchBend') {
      //   const msgs: Msg[] = []
      //   const data = compositionToMsgs(beat, bpm, velocity, startTime, instrument, handlers, composition.note)
      //   // TODO: handle pitch bends
      //   // msgs.push(midiMsg(startTime, pitchBendF(0, composition.mod.fields[0])))
      //   // msgs.push(...data.msgs)
      //   // msgs.push(midiMsg(data.endTime, pitchBendF(0, 0)))
      //   return { msgs, endTime: data.endTime }
      } else if (kind[L.structKind] === 'tempo') {
        return compositionToMsgs(kind.beat, kind.bpm, velocity, startTime, instrument, handlers, composition.note)
      } else if (kind[L.structKind] === 'dynamics') {
        return compositionToMsgs(beat, bpm, kind.amount, startTime, instrument, handlers, composition.note)
      } else if (kind[L.structKind] === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, startTime, kind.program, handlers, composition.note)
      } else {
        // kind[L.structKind] === 'noteHandlers'
        return compositionToMsgs(beat, bpm, velocity, startTime, instrument, kind.handlers, composition.note)
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

export function music_playComposition (composition: Composition): number {
  const msgs = compositionToMsgs(music_dur(1, 4), 120, 64, 0, 0, [], composition).msgs
  const events = msgs.filter(msg => msg.tag === 'trigger' || msg.tag === 'event')
  const startTime = waf()!.audioContext.currentTime

  // Enqueue notes
  for (const msg of msgs) {
    // const elapsed = audioContext.currentTime - startTime
    if (msg.tag === 'midi') {
      const isPercussion = msg.instrument === 128
      const instr = isPercussion ? msg.note : msg.instrument
      waf()!.player.queueWaveTable(waf()!.audioContext,
        waf()!.audioContext.destination,
        waf()!.getInstrument(instr, isPercussion),
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
    const now = waf()!.audioContext.currentTime
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

///// Music module initialization code /////////////////////////////////////////

function initialize() {
  // Initialize webaudiofont
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-member-access
  if (typeof window !== 'undefined' && (window.AudioContext || (window as any).webkitAudioContext)) {
    const player = waf()
    if (player !== undefined) {
      player.loadInstrument(0)         // 0: Acoustic Grand Piano
      player.loadInstrument(35, true)  // 35: Acoustic Bass
      player.loadInstrument(38, true)  // 38: Acoustic Snare
      player.loadInstrument(42, true)  // 42: Closed Hi-Hat
      player.loadInstrument(49, true)  // 49: Crash Cymbal 1
    }
  }
}

initialize()

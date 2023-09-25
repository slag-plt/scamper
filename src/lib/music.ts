import { ICE, Value } from '../lang.js'
import { checkContract, contract } from '../contract.js'
import { callFunction } from '../sem.js'
import * as C from '../contract.js'
import * as Display from '../display.js'

import { waf, instrMap, percMap } from './webaudiofont/webaudiofont.js'

function registerFn (name: string, fn: Function, map: [string, Value.T][]) {
  Value.nameFn(name, fn)
  map.push([name, fn])
}

const Music: [string, Value.T][] = []

export type PitchClass = string
export type Octave = number

export interface Duration extends Value.Struct {
  [Value.structKind] : 'dur',
  numerator : number,
  denominator : number
}

function dur (numerator: number, denominator: number): Duration {
  checkContract(arguments, contract('dur', [C.number, C.number]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'dur', numerator, denominator }
}
registerFn('dur', dur, Music)

function numerator (dur: Duration): number {
  checkContract(arguments, contract('numerator', [durC]))
  return dur.numerator
}
registerFn('numerator', numerator, Music)

function denominator (dur: Duration): number {
  checkContract(arguments, contract('denominator', [durC]))
  return dur.denominator
}
registerFn('denominator', denominator, Music)

Music.push(
  ['wn', dur(1, 1)],
  ['hn', dur(1, 2)],
  ['qn', dur(1, 4)],
  ['en', dur(1, 8)],
  ['sn', dur(1, 16)],
  ['tn', dur(1, 32)],
)

function isPitchClass (s: string): boolean {
  checkContract(arguments, contract('pitch?', [C.string]))
  return /^[A-Ga-g][#b]{0,2}$/.test(s)
}
registerFn('pitch?', isPitchClass, Music)

function isOctave (n: number): boolean {
  checkContract(arguments, contract('octave?', [C.number]))
  return n >= 0 && n <= 10
}
registerFn('octave?', isOctave, Music)

function isValidMidiNote (n: number): boolean {
  checkContract(arguments, contract('note?', [C.number]))
  return n >= 0 && n <= 127
}
registerFn('note?', isValidMidiNote, Music)

const durC: C.Spec = {
  predicate: (v: any) => Value.isStructKind(v, 'dur'),
  errorMsg: (actual: any) => `expected a duration, received ${Value.typeOf(actual)}`,
}

const noteC: C.Spec = {
  predicate: (v: any) => isValidMidiNote(v),
  errorMsg: (actual: any) => `expected a midi note (0--127), received ${Value.typeOf(actual)}`,
}

export interface Note extends Value.Struct { [Value.structKind]: 'note', note: number, duration: Duration }
function note (note: number, duration: Duration): Note {
  checkContract(arguments, contract('note', [noteC, durC]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'note', note, duration }
}
registerFn('note', note, Music)

export interface NoteFreq extends Value.Struct {
  [Value.structKind]: 'note-freq',
  freq: number,
  duration: Duration
}
function noteFreq (freq: number, duration: Duration): NoteFreq {
  checkContract(arguments, contract('note-freq', [C.number, durC]))  
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'note-freq', freq, duration }
}
registerFn('note-freq', noteFreq, Music)

function repeat (n: number, composition: Composition): Composition {
  checkContract(arguments, contract('repeat', [C.nat, compositionC]))
  if (n === 0) {
    return empty()
  } else {
    return seq(composition, repeat(n - 1, composition))
  }
}
registerFn('repeat', repeat, Music)

interface Empty extends Value.Struct { [Value.structKind]: 'empty' }
const empty = (): Empty => ({ [Value.scamperTag]: 'struct', [Value.structKind]: 'empty' })
registerFn('empty', empty, Music)

interface Rest extends Value.Struct { [Value.structKind]: 'rest', duration: Duration }
function rest (duration: Duration): Rest {
  checkContract(arguments, contract('rest', [durC]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'rest', duration }
}
registerFn('rest', rest, Music)

interface Trigger extends Value.Struct { [Value.structKind]: 'trigger', fn: Value.ScamperFn }
function trigger (fn: Value.ScamperFn): Trigger {
  checkContract(arguments, contract('trigger', [C.func]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'trigger', fn }
}
registerFn('trigger', trigger, Music)

interface Par extends Value.Struct { [Value.structKind]: 'par', notes: Composition[] }
function par (...notes: Composition[]): Par {
  checkContract(arguments, contract('par', [], compositionC))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'par', notes }
}
registerFn('par', par, Music)

interface Seq extends Value.Struct { [Value.structKind]: 'seq', notes: Composition[] }
function seq (...notes: Composition[]): Seq {
  checkContract(arguments, contract('seq', [], compositionC)) 
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'seq', notes }
}
registerFn('seq', seq, Music)

interface Pickup extends Value.Struct { [Value.structKind]: 'pickup', pickup: Composition, notes: Composition }
function pickup (pickup: Composition, notes: Composition): Composition {
  checkContract(arguments, contract('pickup', [compositionC, compositionC]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'pickup', pickup, notes }
}
registerFn('pickup', pickup, Music)

type ModKind = Percussion | PitchBend | Tempo | Dynamics | Instrument

function modQ (v: any): boolean {
  if (!Value.isStruct(v)) {
    return false
  } else {
    const kind = v.kind
    return kind === 'percussion' || kind === 'pitchBend' || kind === 'tempo' || kind === 'dynamics' || kind === 'instrument'
  }
}
registerFn('mod?', modQ, Music)

const modC: C.Spec = {
  predicate: modQ,
  errorMsg: (actual: any) => `expected a mod, received ${Value.typeOf(actual)}`,
}

interface Percussion extends Value.Struct { [Value.structKind]: 'percussion' }
function percussion (): Percussion {
  checkContract(arguments, contract('percussion', [])) 
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'percussion' }
}
registerFn('percussion', percussion, Music)

interface PitchBend extends Value.Struct { [Value.structKind]: 'pitchBend', amount: number }
function pitchBend (amount: number): PitchBend {
  checkContract(arguments, contract('bend', [C.numRange(-1, 1)]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'pitchBend', amount }
}
registerFn('bend', pitchBend, Music)

interface Tempo extends Value.Struct { [Value.structKind]: 'tempo', beat: Duration, bpm: number }
function tempo (beat: Duration, bpm: number): Tempo {
  checkContract(arguments, contract('tempo', [durC, C.nonneg]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'tempo', beat, bpm }
}
registerFn('tempo', tempo, Music)

interface Dynamics extends Value.Struct { [Value.structKind]: 'dynamics', amount: number }
function dynamics (amount: number): Dynamics {
  checkContract(arguments, contract('dynamics', [C.numRange(0, 127)])) 
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'dynamics', amount }
}
registerFn('dynamics', dynamics, Music)

interface Instrument extends Value.Struct { [Value.structKind]: 'instrument', program: number }
function instrument (program: number): Instrument {
  checkContract(arguments, contract('instrument', [C.numRange(0, 127)]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'instrument', program }
}

interface Mod extends Value.Struct { [Value.structKind]: 'mod', note: Composition, mod: ModKind }
function mod (mod: ModKind, note: Composition): Mod {
  checkContract(arguments, contract('mod', [modC, compositionC]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'mod', note, mod }
}
registerFn('mod', mod, Music)

export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod

function compositionQ (v: any): boolean {
  if (!Value.isStruct(v)) {
    return false
  } else {
    const kind = v.kind
    return kind === 'empty' || kind === 'note' || kind === 'note-freq' || kind === 'rest' || kind === 'trigger' || kind === 'par' || kind === 'seq' || kind === 'pickup' || kind === 'mod'
  }
}
registerFn('composition?', compositionQ, Music)

const compositionC: C.Spec = {
  predicate: compositionQ,
  errorMsg: (actual: any) => `expected a composition, received ${Value.typeOf(actual)}`,
}

////////////////////////////////////////////////////////////////////////////////

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
  callback: Value.ScamperFn
}

type Msg = MidiMsg | TriggerMsg

const midiMsg = (time: number, duration: number, note: number, instrument: number, velocity: number): Msg =>
  ({ tag: 'midi', time, duration, note, instrument, velocity })

const triggerMsg = (time: number, callback: Value.ScamperFn): Msg =>
  ({ tag: 'trigger', time, callback })

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
  instrument: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition[Value.structKind]) {
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
        const result = compositionToMsgs(beat, bpm, velocity, startTime, instrument, note)
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
        const result = compositionToMsgs(beat, bpm, velocity, time, instrument, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, instrument, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod.kind === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 128, composition.note)
      } else if (composition.mod.kind === 'pitchBend') {
        const msgs: Msg[] = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.note)
        // TODO: handle pitch bends
        // msgs.push(midiMsg(startTime, pitchBendF(0, composition.mod.fields[0])))
        // msgs.push(...data.msgs)
        // msgs.push(midiMsg(data.endTime, pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.kind === 'tempo') {
        return compositionToMsgs(composition.mod.fields[0], composition.mod.fields[1], velocity, startTime, instrument, composition.note)
      } else if (composition.mod.kind === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.fields[0], startTime, instrument, composition.note)
      } else if (composition.mod.kind === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.fields[0], composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

export function playComposition (composition: Composition): number {
  checkContract(arguments, contract('play-composition', [compositionC]))
  const msgs = compositionToMsgs(dur(1, 4), 120, 64, 0, 0, composition).msgs
  const triggers = msgs.filter(msg => msg.tag === 'trigger') as TriggerMsg[]
  const startTime = waf().audioContext.currentTime

  // Enqueue notes
  for (const msg of msgs) {
    // const elapsed = audioContext.currentTime - startTime
    if (msg.tag === 'midi' && msg.instrument < 128) {
      waf().player.queueWaveTable(waf().audioContext, waf().audioContext.destination, instrMap.get(msg.instrument)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, msg.velocity)
    } else if (msg.tag === 'midi' && msg.instrument === 128) {
      waf().player.queueWaveTable(waf().audioContext, waf().audioContext.destination, percMap.get(msg.note)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, msg.velocity)
    }
  }

  // Set up a timer to discharge triggers
  let i = 0
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    // N.B., in milliseconds
    const now = waf().audioContext.currentTime
    while (i < triggers.length) {
      const trigger = triggers[i]
      if (trigger.time / 1000 + startTime <= now) {
        // TODO: need to catch and expose exceptions from here!
        // Likely need to install a global to display async errors.
        callFunction(trigger.callback, [])
        i += 1
        continue
      } else {
        return
      }
    }
    clearInterval(id)
  })
  return id
}
registerFn('play-composition', playComposition, Music)

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

Display.addCustomWebRenderer(compositionQ, render)

export default Music
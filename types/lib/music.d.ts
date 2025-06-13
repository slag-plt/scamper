import { Library, Value } from '../lang.js';
declare const Music: Library;
export type PitchClass = string;
export type Octave = number;
export interface Duration extends Value.Struct {
    [Value.structKind]: 'dur';
    numerator: number;
    denominator: number;
}
export interface Note extends Value.Struct {
    [Value.structKind]: 'note';
    note: number;
    duration: Duration;
}
export interface NoteFreq extends Value.Struct {
    [Value.structKind]: 'note-freq';
    freq: number;
    duration: Duration;
}
interface Empty extends Value.Struct {
    [Value.structKind]: 'empty';
}
interface Rest extends Value.Struct {
    [Value.structKind]: 'rest';
    duration: Duration;
}
interface Trigger extends Value.Struct {
    [Value.structKind]: 'trigger';
    fn: Value.ScamperFn;
}
interface Par extends Value.Struct {
    [Value.structKind]: 'par';
    notes: Composition[];
}
interface Seq extends Value.Struct {
    [Value.structKind]: 'seq';
    notes: Composition[];
}
interface Pickup extends Value.Struct {
    [Value.structKind]: 'pickup';
    pickup: Composition;
    notes: Composition;
}
/** TODO: we're missing on-note? */
type ModKind = Percussion | Tempo | Dynamics | Instrument | NoteHandlersMod;
interface Percussion extends Value.Struct {
    kind: 'percussion';
}
interface Tempo extends Value.Struct {
    [Value.structKind]: 'tempo';
    beat: Duration;
    bpm: number;
}
interface Dynamics extends Value.Struct {
    [Value.structKind]: 'dynamics';
    amount: number;
}
interface Instrument extends Value.Struct {
    [Value.structKind]: 'instrument';
    program: number;
}
interface NoteHandlersMod extends Value.Struct {
    [Value.structKind]: 'noteHandlers';
    handlers: NoteHandlers;
}
interface Mod extends Value.Struct {
    [Value.structKind]: 'mod';
    note: Composition;
    mod: ModKind;
}
interface NoteEvent extends Value.Struct {
    [Value.structKind]: 'note-event';
    id: string;
}
export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod | NoteEvent;
/***** Reactive Events ********************************************************/
export interface NoteMsg extends Value.Struct {
    [Value.structKind]: 'event-note';
    id: string;
}
export type NoteHandlers = ((note: NoteMsg) => void)[];
export declare function playComposition(composition: Composition): number;
export declare function render(v: any): HTMLElement;
export default Music;
//# sourceMappingURL=music.d.ts.map
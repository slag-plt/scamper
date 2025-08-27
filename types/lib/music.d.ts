import * as L from '../lpm';
declare const Music: L.Library;
export type PitchClass = string;
export type Octave = number;
export interface Duration extends L.Struct {
    [L.structKind]: 'dur';
    numerator: number;
    denominator: number;
}
export interface Note extends L.Struct {
    [L.structKind]: 'note';
    note: number;
    duration: Duration;
}
export interface NoteFreq extends L.Struct {
    [L.structKind]: 'note-freq';
    freq: number;
    duration: Duration;
}
interface Empty extends L.Struct {
    [L.structKind]: 'empty';
}
interface Rest extends L.Struct {
    [L.structKind]: 'rest';
    duration: Duration;
}
interface Trigger extends L.Struct {
    [L.structKind]: 'trigger';
    fn: L.ScamperFn;
}
interface Par extends L.Struct {
    [L.structKind]: 'par';
    notes: Composition[];
}
interface Seq extends L.Struct {
    [L.structKind]: 'seq';
    notes: Composition[];
}
interface Pickup extends L.Struct {
    [L.structKind]: 'pickup';
    pickup: Composition;
    notes: Composition;
}
/** TODO: we're missing on-note? */
type ModKind = Percussion | Tempo | Dynamics | Instrument | NoteHandlersMod;
interface Percussion extends L.Struct {
    kind: 'percussion';
}
interface Tempo extends L.Struct {
    [L.structKind]: 'tempo';
    beat: Duration;
    bpm: number;
}
interface Dynamics extends L.Struct {
    [L.structKind]: 'dynamics';
    amount: number;
}
interface Instrument extends L.Struct {
    [L.structKind]: 'instrument';
    program: number;
}
interface NoteHandlersMod extends L.Struct {
    [L.structKind]: 'noteHandlers';
    handlers: NoteHandlers;
}
interface Mod extends L.Struct {
    [L.structKind]: 'mod';
    note: Composition;
    mod: ModKind;
}
interface NoteEvent extends L.Struct {
    [L.structKind]: 'note-event';
    id: string;
}
export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod | NoteEvent;
/***** Reactive Events ********************************************************/
export interface NoteMsg extends L.Struct {
    [L.structKind]: 'event-note';
    id: string;
}
export type NoteHandlers = ((note: NoteMsg) => void)[];
export declare function playComposition(composition: Composition): number;
export declare function render(v: any): HTMLElement;
export default Music;
//# sourceMappingURL=music.d.ts.map
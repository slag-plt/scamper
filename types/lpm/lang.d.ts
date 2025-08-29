import { Range } from './range.js';
/** The field name of Scamper objects denoting that object's runtime tag. */
export declare const scamperTag = "##scamperTag##";
/** The field name of Scamper objects that are structs denoting that struct's kind. */
export declare const structKind = "##structKind##";
/** Identifiers name entities maintained at runtime. */
export type Id = string;
/** Indices provide "fast names" of objects, in particular locals, at runtime. */
export type Idx = number;
/** Environments are scoped collections of variable bindings. */
export declare class Env {
    bindings: Map<string, Value>;
    parent?: Env;
    constructor(parent?: Env);
    get(name: string): Value | undefined;
    set(name: string, value: Value): void;
    has(name: string): boolean;
    extend(...bindings: [string, Value][]): Env;
    pop(): Env;
}
/** A library is a collection of importable top-level definitions. */
export declare class Library {
    lib: [string, Value][];
    initializer: Function | undefined;
    constructor(initializer?: Function);
    registerValue(name: string, v: Value): void;
}
/** Tagged objects are Scamper values with a queryable runtime identity. */
export interface TaggedObject {
    [scamperTag]: string;
}
/** A closure is a tagged object that bundles a function with its captured environment. */
export interface Closure extends TaggedObject {
    [scamperTag]: 'closure';
    params: Id[];
    code: Blk;
    env: Env;
    call: (...args: Value[]) => Value;
    name?: Id;
}
/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
    [scamperTag]: 'char';
    value: string;
}
/** A symbol is a tagged object representing an identifier. */
export interface Sym extends TaggedObject {
    [scamperTag]: 'sym';
    value: string;
}
export interface Struct extends TaggedObject {
    [scamperTag]: 'struct';
    [structKind]: string;
    [key: string]: any;
    [key: number]: never;
}
/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[];
/** A Scamper function is either a closure or a raw Javascript function. */
export type ScamperFn = Closure | Function;
/** Calls a ScamperFn function with the provided arguments */
export declare function callScamperFn(fn: ScamperFn, ...args: Value[]): any;
/** Raw Javascript values are any Javascript object. */
export type Raw = object;
/** Values are the core datatype manipulated by LPM programs. */
export type Value = number | boolean | string | null | undefined | Vector | TaggedObject | ScamperFn | Raw;
/**
 * A pair is an algebraic datatype with a first and second component.
 */
export interface Pair extends Struct {
    [scamperTag]: 'struct';
    [structKind]: 'pair';
    fst: Value;
    snd: Value;
}
/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends Struct {
    [scamperTag]: 'struct';
    [structKind]: 'cons';
    head: Value;
    tail: List;
}
/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons;
export type Lit = {
    tag: 'lit';
    value: Value;
    range: Range;
    startsStmt: boolean;
};
export type Var = {
    tag: 'var';
    name: string;
    range: Range;
    startsStmt: boolean;
};
export type Ctor = {
    tag: 'ctor';
    name: string;
    fields: string[];
    range: Range;
    startsStmt: boolean;
};
export type Cls = {
    tag: 'cls';
    params: string[];
    body: Blk;
    name?: string;
    range: Range;
    startsStmt: boolean;
};
export type Ap = {
    tag: 'ap';
    numArgs: number;
    range: Range;
    startsStmt: boolean;
};
export type Match = {
    tag: 'match';
    branches: [Pat, Blk][];
    range: Range;
    startsStmt: boolean;
};
export type Disp = {
    tag: 'disp';
    range: Range;
    startsStmt: boolean;
};
export type Import = {
    tag: 'import';
    name: string;
    range: Range;
    startsStmt: boolean;
};
export type Define = {
    tag: 'define';
    name: string;
    range: Range;
    startsStmt: boolean;
};
export type Raise = {
    tag: 'raise';
    msg: string;
    range: Range;
    startsStmt: boolean;
};
export type PopS = {
    tag: 'pops';
    startsStmt: boolean;
};
export type PopV = {
    tag: 'popv';
    startsStmt: boolean;
};
export type Ops = Lit | Var | Ctor | Cls | Ap | Match | Disp | Import | Define | Raise | PopS | PopV;
export type Blk = Ops[];
export type PWild = {
    tag: 'pwild';
    range: Range;
};
export type PLit = {
    tag: 'plit';
    value: Value;
    range: Range;
};
export type PVar = {
    tag: 'pvar';
    name: string;
    range: Range;
};
export type PCtor = {
    tag: 'pctor';
    name: string;
    args: Pat[];
    range: Range;
};
export type Pat = PWild | PLit | PVar | PCtor;
/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export declare class Frame {
    name: string;
    env: Env;
    values: Value[];
    ops: Ops[];
    constructor(name: string, env: Env, blk: Blk);
    isFinished(): boolean;
    pushBlk(blk: Blk): void;
    popInstr(): Ops;
}
/** A single thread of execution in LPM. */
export declare class Thread {
    frames: Frame[];
    result: Value;
    constructor(name: string, env: Env, blk: Blk);
    isFinished(): boolean;
    getCurrentFrame(): Frame;
    push(name: string, env: Env, blk: Blk): void;
    pop(): void;
    unwindToNextStatement(): void;
}
//# sourceMappingURL=lang.d.ts.map
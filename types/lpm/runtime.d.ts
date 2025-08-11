/** Locations are used to track single positions within source code. */
export declare class Loc {
    line: number;
    col: number;
    idx: number;
    constructor(line: number, col: number, idx: number);
    toString(): string;
    static none: Loc;
}
/** Ranges bundle start and end locations within source code. */
export declare class Range {
    begin: Loc;
    end: Loc;
    constructor(begin: Loc, end: Loc);
    toString(): string;
    static none: Range;
    static of(startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number): Range;
}
/** Phases of scamper execution, used for the purposes of error reporting. */
type Phase = 'Parser' | 'Runtime';
/** Errors that arise during Scamper compilation and execution. */
export declare class ScamperError extends Error {
    phase: Phase;
    modName?: string;
    range?: Range;
    source?: string;
    constructor(phase: Phase, msg: string, modName?: string, range?: Range, source?: string);
    toString(): string;
}
/** Internal compiler errors arise due to bugs in Scamper. */
export declare class ICE extends Error {
    funcName: string;
    constructor(funcName: string, msg: string);
    toString(): string;
}
/** Identifiers name entities maintained at runtime. */
export type Id = string;
/**
 * Environments capture (scoped) mappings from identifiers to values. We
 * ensure that the length of the environment reflects the (statically known)
 * number of local variables in the current function scope with the following
 * layout:
 *
 * ~~~
 * [<captured scope>, <params>, <locals>]
 * ~~~
*/
export type Env = Value[];
/** Libraries are collections of (top-level) bindings from names to values. */
export declare class Library {
    lib: [string, Value][];
    initializer: Function | undefined;
    constructor();
    registerValue(name: string, v: Value): void;
}
/**
 * Local maps record local variable indices to variable names for the
 * purposes of debugging and error reporting.
 */
type LocalMap = string[];
/** The field name of Scamper objects denoting that object's runtime tag. */
export declare const scamperTag: unique symbol;
/** The field name of Scamper objects that are structs denoting that struct's kind. */
export declare const structKind: unique symbol;
/** Tagged objects are Scamper values with a queryable runtime identity. */
interface TaggedObject {
    [scamperTag]: string;
}
/** A closure is a tagged object that bundles a function with its captured environment. */
export interface Closure extends TaggedObject {
    [scamperTag]: 'closure';
    params: Id[];
    code: Id;
    env: Env;
    name?: string;
    localMap?: LocalMap;
}
/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
    [scamperTag]: 'char';
    value: string;
}
/** A symbol is tagged object that contains a string interned for fast equality. */
export interface Sym extends TaggedObject {
    [scamperTag]: 'sym';
    value: string;
}
/** A pair is a tagged object that contains a pair of (potentially heterogeneous) values. */
export interface Pair extends TaggedObject {
    [scamperTag]: 'pair';
    fst: Value;
    snd: Value;
    isList: boolean;
}
/**
 * A pattern variable appears in pattern objects and denotes a binding
 * location in the pattern. Note that a negative index indicates a
 * wildcard (non-binding) variable.
 */
export interface PVar extends TaggedObject {
    [scamperTag]: 'pvar';
    idx: number;
}
export interface Struct extends TaggedObject {
    [scamperTag]: 'struct';
    [structKind]: string;
    [key: string]: any;
    [key: number]: never;
}
/**
 * A Scamper list is either null or a pair whose second component is a list,
 * recursively.
 */
export type List = null | Pair;
/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[];
/** A Scamper function is either a closure or a raw Javascript function. */
export type ScamperFn = Closure | Function;
/** Raw Javascript values are any Javascript object. */
export type Raw = Object;
/** Values are the core datatype manipulated by Scamper programs. */
export type Value = boolean | number | string | List | Vector | Function | undefined | TaggedObject | Raw;
export declare const isNumber: (v: Value) => v is number;
export declare const isBoolean: (v: Value) => v is boolean;
export declare const isString: (v: Value) => v is string;
export declare const isNull: (v: Value) => v is null;
export declare const isVoid: (v: Value) => v is undefined;
export declare const isArray: (v: Value) => v is Array<Value>;
export declare const isJsFunction: (v: Value) => v is Function;
export declare const isTaggedObject: (v: Value) => v is TaggedObject;
export declare const isClosure: (v: Value) => v is Closure;
export declare const isFunction: (v: Value) => v is ScamperFn;
export declare const isChar: (v: Value) => v is Char;
export declare const isSym: (v: Value) => v is Sym;
export declare const isSymName: (v: Value, name: string) => boolean;
export declare const isPair: (v: Value) => v is Pair;
export declare const isList: (v: Value) => v is List;
export declare const isPVar: (v: Value) => v is PVar;
export declare const isStruct: (v: Value) => v is Struct;
export declare const isStructKind: <T extends Struct>(v: Value, k: string) => v is T;
export declare const mkClosure: (arity: number, params: Id[], code: Id, env: Env) => Value;
export declare const mkChar: (v: string) => Char;
export declare const mkSym: (v: string) => Sym;
export declare const mkPair: (fst: Value, snd: Value) => Pair;
export declare const mkList: (...values: Value[]) => List;
export declare const mkPVar: (idx: number) => PVar;
export declare const mkStruct: (kind: string, fields: string[], values: Value[]) => Struct;
/** @return a list of the fields of the given struct. */
export declare function getFieldsOfStruct(s: Struct): string[];
/** @return a branch object suitable for pattern matching */
export declare function mkBranch(pattern: Value, target: number): Pair;
/** Mutates a Javascript function to contain a `name` field with that function's name. */
export declare const nameFn: (name: string, fn: Function) => Function;
/** @return a vector (array) representation of the input list. */
export declare function listToVector(l: List): Value[];
/** @return a list representation of the input vector (array). */
export declare function vectorToList(arr: Value[]): Pair | null;
/**
 * @return the first elment of l assuming that l has the correct number of
 * elements.
 */
export declare function listFirst(l: Value): Value;
/**
 * @return the second elment of l assuming that l has the correct number of
 * elements.
 */
export declare function listSecond(l: Value): Value;
/**
 * @return the third elment of l assuming that l has the correct number of
 * elements.
 */
export declare function listThird(l: Value): Value;
/**
 * @return the fourth elment of l assuming that l has the correct number of
 * elements.
 */
export declare function listFourth(l: Value): Value;
/**
 * @return the tail of the l assuming it is a non-empty list.
 */
export declare function listTail(l: Value): Value;
/**
 * @returns the type of the given value as a string (for debugging purposes).
 */
export declare function typeOf(v: Value): string;
/**
 * @return true if the two values are structurally equal to each other.
 */
export declare function equals(v: Value, u: Value): boolean;
/** The code of a LPM program is an array of bytes. */
export type Code = {
    ops: Uint8Array;
    numLocals: number;
};
/**
 * A program collects together three structures:
 * + `code`: a mapping from function identifiers to their code.
 * + `identifiers`: a mapping from string identifiers to their strings.
 * + `objects`: a mapping from object identifiers to their values.
 */
export type Program = {
    code: Map<Id, Code>;
    identifiers: string[];
    objects: Value[];
};
export declare function mkProgram(): Program;
export {};
//# sourceMappingURL=runtime.d.ts.map
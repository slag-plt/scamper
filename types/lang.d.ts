import { AST } from "./ast";
export declare class Loc {
    line: number;
    col: number;
    idx: number;
    constructor(line: number, col: number, idx: number);
    toString(): string;
}
export declare class Range {
    begin: Loc;
    end: Loc;
    constructor(startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number);
    toString(): string;
}
export declare const mkRange: (beg: Loc, end: Loc) => Range;
export declare const noLoc: Loc;
export declare const noRange: Range;
type Phase = 'Parser' | 'Runtime';
export declare class ScamperError extends Error {
    phase: Phase;
    modName?: string;
    range?: Range;
    source?: string;
    constructor(phase: Phase, msg: string, modName?: string, range?: Range, source?: string);
    toString(): string;
}
export declare class ICE extends Error {
    funcName: string;
    constructor(funcName: string, msg: string);
    toString(): string;
}
export type Id = string;
export declare function charToName(c: string): string;
export declare const reservedWords: string[];
export declare namespace Value {
    const scamperTag: unique symbol;
    const structKind: unique symbol;
    type TaggedObject = Closure | Char | Sym | Pair | Syntax | Struct;
    type Closure = {
        [scamperTag]: 'closure';
        params: Id[];
        ops: Op.T[];
        env: Env;
        name?: string;
    };
    type Char = {
        [scamperTag]: 'char';
        value: string;
    };
    type Sym = {
        [scamperTag]: 'sym';
        value: string;
    };
    type Pair = {
        [scamperTag]: 'pair';
        fst: T;
        snd: T;
        isList: boolean;
    };
    type Syntax = {
        [scamperTag]: 'syntax';
        range: Range;
        value: T;
    };
    interface Struct {
        [scamperTag]: 'struct';
        [structKind]: string;
        [key: string]: any;
        [key: number]: never;
    }
    type List = null | Pair;
    type Vector = T[];
    type ScamperFn = Closure | Function;
    type Raw = Object;
    type T = boolean | number | string | List | Vector | Function | undefined | TaggedObject | Raw;
    const isNumber: (v: T) => boolean;
    const isBoolean: (v: T) => boolean;
    const isString: (v: T) => boolean;
    const isNull: (v: T) => boolean;
    const isVoid: (v: T) => boolean;
    const isArray: (v: T) => boolean;
    const isJsFunction: (v: T) => boolean;
    const isTaggedObject: (v: T) => boolean;
    const isClosure: (v: T) => boolean;
    const isFunction: (v: T) => boolean;
    const isChar: (v: T) => boolean;
    const isSym: (v: T) => boolean;
    const isSymName: (v: T, name: string) => boolean;
    const isPair: (v: T) => boolean;
    const isList: (v: T) => boolean;
    const isSyntax: (v: T) => boolean;
    const isStruct: (v: T) => boolean;
    const isStructKind: (v: T, k: string) => boolean;
    const mkClosure: (arity: number, params: Id[], ops: Op.T[], env: Env) => T;
    const mkChar: (v: string) => Char;
    const mkSym: (v: string) => Sym;
    const mkPair: (fst: T, snd: T) => Pair;
    const mkList: (...values: T[]) => List;
    const mkSyntax: (range: Range, value: T) => Syntax;
    const mkStruct: (kind: string, fields: string[], values: T[]) => T;
    const stripSyntax: (v: T) => T;
    function stripAllSyntax(v: T): T;
    const unpackSyntax: (v: T) => {
        range: Range;
        value: T;
    };
    const rangeOf: (v: T) => Range;
    const nameFn: (name: string, fn: Function) => Function;
    function listToVector(l: List): T[];
    function vectorToList(arr: T[]): Pair | null;
    function toString(v: T): string;
    function getFieldsOfStruct(s: Struct): string[];
    function equal(v1: T, v2: T): boolean;
    function typeOf(v: T): string;
}
export declare namespace Stmt {
    type T = Binding | Exp | Import | Display | Struct;
    type Binding = {
        _scamperTag: 'struct';
        kind: 'binding';
        name: Id;
        body: Op.T[];
        src: Value.T;
        range: Range;
    };
    type Exp = {
        _scamperTag: 'struct';
        kind: 'exp';
        body: Op.T[];
        src: Value.T;
        range: Range;
    };
    type Import = {
        _scamperTag: 'struct';
        kind: 'import';
        modName: string;
        range: Range;
    };
    type Display = {
        _scamperTag: 'struct';
        kind: 'display';
        body: Op.T[];
        src: Value.T;
        range: Range;
    };
    type Struct = {
        _scamperTag: 'struct';
        kind: 'struct';
        id: string;
        fields: string[];
        range: Range;
    };
    const mkStmtBinding: (name: Id, body: Op.T[], src: Value.T, range: Range) => T;
    const mkStmtExp: (body: Op.T[], src: Value.T, range: Range) => T;
    const mkImport: (modName: string, range: Range) => T;
    const mkDisplay: (body: Op.T[], src: Value.T, range: Range) => T;
    const mkStruct: (id: string, fields: string[], range: Range) => T;
}
export type Prog = Stmt.T[];
export type ParserOutput = {
    prog: Prog;
    ast: AST;
};
export declare namespace Op {
    type Label = string;
    const freshLabel: () => Label;
    type MatchBranch = {
        pattern: Value.T;
        body: T[];
    };
    type T = Var | Val | Cls | Ap | If | Let | Seq | Match | And | Or | Cond | Lbl | Exn;
    type Var = {
        tag: 'var';
        name: string;
        range: Range;
    };
    type Val = {
        tag: 'val';
        value: Value.T;
    };
    type Cls = {
        tag: 'cls';
        params: Id[];
        ops: T[];
    };
    type Ap = {
        tag: 'ap';
        arity: number;
        range: Range;
    };
    type If = {
        tag: 'if';
        ifb: T[];
        elseb: T[];
        range: Range;
    };
    type Let = {
        tag: 'let';
        names: Id[];
        body: T[];
    };
    type Seq = {
        tag: 'seq';
        numSubexps: number;
    };
    type Match = {
        tag: 'match';
        branches: MatchBranch[];
        range: Range;
    };
    type And = {
        tag: 'and';
        jmpTo: Label;
        range: Range;
    };
    type Or = {
        tag: 'or';
        jmpTo: Label;
        range: Range;
    };
    type Cond = {
        tag: 'cond';
        body: T[];
        end: Label;
        range: Range;
    };
    type Lbl = {
        tag: 'lbl';
        name: string;
    };
    type Exn = {
        tag: 'exn';
        msg: string;
        modName?: string;
        range?: Range;
        source?: string;
    };
    const mkVar: (name: string, range: Range) => T;
    const mkValue: (value: Value.T) => T;
    const mkCls: (params: Id[], ops: T[]) => T;
    const mkAp: (arity: number, range: Range) => T;
    const mkIf: (ifb: T[], elseb: T[], range: Range) => T;
    const mkLet: (names: Id[], body: T[]) => T;
    const mkSeq: (numSubexps: number) => T;
    const mkMatch: (branches: MatchBranch[], range: Range) => T;
    const mkAnd: (jmpTo: Label, range: Range) => T;
    const mkOr: (jmpTo: Label, range: Range) => T;
    const mkCond: (body: T[], end: Label, range: Range) => T;
    const mkLbl: (name: string) => T;
    const mkExn: (msg: string, modName?: string, range?: Range, source?: string) => T;
}
export declare class Env {
    private bindings;
    private parent?;
    constructor(bindings: [Id, Value.T][], parent?: Env);
    has(name: Id): boolean;
    get(name: Id): Value.T | undefined;
    extend(bindings: [Id, Value.T][]): Env;
    set(name: Id, v: Value.T): void;
    remove(...names: Id[]): void;
    clone(): Env;
    quotient(...names: Id[]): Env;
}
export type Library = {
    lib: [string, Value.T][];
    initializer: Function | undefined;
};
export declare function emptyLibrary(): Library;
export declare function registerValue(name: string, v: Value.T, library: Library): void;
export {};
//# sourceMappingURL=lang.d.ts.map
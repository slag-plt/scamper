import { Range } from './range.js';
import * as L from './lang.js';
export declare const isNumber: (v: L.Value) => v is number;
export declare const isBoolean: (v: L.Value) => v is boolean;
export declare const isString: (v: L.Value) => v is string;
export declare const isSymName: (v: L.Value, name: string) => boolean;
export declare const isNull: (v: L.Value) => v is null;
export declare const isVoid: (v: L.Value) => v is undefined;
export declare const isArray: (v: L.Value) => v is Array<L.Value>;
export declare const isTaggedObject: (v: L.Value) => v is L.TaggedObject;
export declare const isJsFunction: (v: L.Value) => v is Function;
export declare const isClosure: (v: L.Value) => v is L.Closure;
export declare const isFunction: (v: L.Value) => v is L.ScamperFn;
export declare const isChar: (v: L.Value) => v is L.Char;
export declare const isSym: (v: L.Value) => v is L.Sym;
export declare const isStruct: (v: L.Value) => v is L.Struct;
export declare const isStructKind: <T extends L.Struct>(v: L.Value, k: string) => v is T;
export declare const isPair: (v: L.Value) => v is L.Pair;
export declare const isList: (v: L.Value) => v is L.List;
export declare const mkClosure: (params: L.Id[], code: L.Blk, env: L.Env, call: (...args: any) => any, name?: L.Id) => L.Closure;
export declare const mkChar: (v: string) => L.Char;
export declare const mkSym: (v: string) => L.Sym;
export declare const mkStruct: (kind: string, fields: string[], values: L.Value[]) => L.Struct;
export declare const mkPair: (fst: L.Value, snd: L.Value) => L.Pair;
export declare const mkCons: (head: L.Value, tail: L.List) => L.Cons;
export declare const mkList: (...values: L.Value[]) => L.List;
export declare const mkLit: (value: L.Value, range?: Range, startsStmt?: boolean) => L.Lit;
export declare const mkVar: (name: string, range?: Range, startsStmt?: boolean) => L.Var;
export declare const mkCtor: (name: string, fields: string[], range?: Range, startsStmt?: boolean) => L.Ctor;
export declare const mkCls: (params: string[], body: L.Blk, name?: string, range?: Range, startsStmt?: boolean) => L.Cls;
export declare const mkAp: (numArgs: number, range?: Range, startsStmt?: boolean) => L.Ap;
export declare const mkMatch: (branches: [L.Pat, L.Blk][], range?: Range, startsStmt?: boolean) => L.Match;
export declare const mkDisp: (range?: Range, startsStmt?: boolean) => L.Disp;
export declare const mkDefine: (name: string, range?: Range, startsStmt?: boolean) => L.Define;
export declare const mkImport: (name: string, range?: Range, startsStmt?: boolean) => L.Import;
export declare const mkRaise: (msg: string, range?: Range, startsStmt?: boolean) => L.Raise;
export declare const mkPops: (startsStmt?: boolean) => L.PopS;
export declare const mkPopv: (startsStmt?: boolean) => L.PopV;
export declare const mkPWild: (range?: Range) => L.PWild;
export declare const mkPLit: (value: L.Value, range?: Range) => L.PLit;
export declare const mkPVar: (name: string, range?: Range) => L.PVar;
export declare const mkPCtor: (name: string, args: L.Pat[], range?: Range) => L.PCtor;
/** @return true iff the given field name is a hidden field of a struct. */
export declare function isHiddenField(fld: string): boolean;
/** @return a list of the fields of the given struct. */
export declare function getFieldsOfStruct(s: L.Struct): string[];
/** Mutates a Javascript function to contain a `name` field with that function's name. */
export declare const nameFn: (name: string, fn: Function) => Function;
export declare const namedCharValues: Map<string, string>;
export declare const charNamedValues: Map<string, string>;
export declare function charToName(c: string): string;
/** @return a vector (array) representation of the input list. */
export declare function listToVector(l: L.List): L.Value[];
/** @return a list representation of the input vector (array). */
export declare function vectorToList(arr: L.Value[]): L.List;
/** @returns the nth element of the list */
export declare function listNth(n: number, l: L.List): L.Value;
/** @return true if the two L.Values are structurally equal to each other. */
export declare function equals(v: L.Value, u: L.Value): boolean;
/** @returns the type of the given value as a string (for debugging purposes) */
export declare function typeOf(v: L.Value): string;
/** @return a generic string representation of value v. */
export declare function toString(v: L.Value): string;
//# sourceMappingURL=util.d.ts.map
import { Value } from '../lpm/runtime.js';
import * as R from '../lpm/runtime.js';
export type Metadata = Map<string, any>;
export type MetadataEntry = [string, any];
/**
 * A syntax value wraps a value that serves as a Scheme AST. It provides
 * metadata information, e.g., source ranges, for the underlying AST.
 */
export interface Syntax extends R.Struct {
    [R.scamperTag]: 'struct';
    [R.structKind]: 'syntax';
    metadata: Metadata;
    value: Value;
}
export declare const mkSyntax: (value: Value, ...metadata: MetadataEntry[]) => Syntax;
export declare const isSyntax: (v: Value) => v is Syntax;
/** @return v but with its top-level syntax wrapper removed, if it exists. */
export declare const stripSyntax: (v: Value) => Value;
/** @return v but with all syntax wrappers removed, recursively. */
export declare function stripAllSyntax(v: Value): Value;
/**
 * @return a pair of a value and its associated metadata if it is a syntax object
 *         or a fresh metadata map if it is a raw value.
 */
export declare const unpackSyntax: (v: Value) => {
    value: Value;
    metadata: Metadata;
};
export declare function isAtom(v: Value): boolean;
export declare function isApp(v: Value): boolean;
export declare function asIdentifier(v: Value): {
    name: string;
    metadata: Metadata;
};
export declare function nameFromIdentifier(v: Value): string;
export declare function isSpecialForm(v: Value, expected: string): boolean;
export declare const isLambda: (v: Value) => boolean;
export declare const isLet: (v: Value) => boolean;
export declare const isLetStar: (v: Value) => boolean;
export declare const isAnd: (v: Value) => boolean;
export declare const isOr: (v: Value) => boolean;
export declare const isBegin: (v: Value) => boolean;
export declare const isIf: (v: Value) => boolean;
export declare const isCond: (v: Value) => boolean;
export declare const isMatch: (v: Value) => boolean;
export declare const isQuote: (v: Value) => boolean;
export declare const isSection: (v: Value) => boolean;
export type Pair = {
    fst: Value;
    snd: Value;
    metadata: Metadata;
};
export declare function asApp(v: Value): {
    values: Value[];
    metadata: Metadata;
};
export declare function asLambda(v: Value): {
    params: Value[];
    body: Value;
    metadata: Metadata;
};
export declare function asLet(v: Value): {
    bindings: Pair[];
    body: Value;
    metadata: Metadata;
};
export declare function asLetStar(v: Value): {
    bindings: Pair[];
    body: Value;
    metadata: Metadata;
};
export declare function asAnd(v: Value): {
    values: Value[];
    metadata: Metadata;
};
export declare function asOr(v: Value): {
    values: Value[];
    metadata: Metadata;
};
export declare function asBegin(v: Value): {
    values: Value[];
    metadata: Metadata;
};
export declare function asIf(v: Value): {
    guard: Value;
    ifB: Value;
    elseB: Value;
    metadata: Metadata;
};
export declare function asCond(v: Value): {
    clauses: Pair[];
    metadata: Metadata;
};
export declare function asMatch(v: Value): {
    scrutinee: Value;
    clauses: Pair[];
    metadata: Metadata;
};
export declare function asQuote(v: Value): {
    value: Value;
    metadata: Metadata;
};
export declare function asSection(v: Value): {
    values: Value[];
    metadata: Metadata;
};
export declare const isImport: (v: Value) => boolean;
export declare const isDefine: (v: Value) => boolean;
export declare const isDisplay: (v: Value) => boolean;
export declare const isStruct: (v: Value) => boolean;
export declare function asImport(v: Value): {
    name: Value;
    metadata: Metadata;
};
export declare function asDefine(v: Value): {
    name: Value;
    value: Value;
    metadata: Metadata;
};
export declare function asDisplay(v: Value): {
    value: Value;
    metadata: Metadata;
};
export declare function asStruct(v: Value): {
    name: Value;
    fields: Value[];
    metadata: Metadata;
};
export declare function structPredName(name: string): string;
export declare function structFieldName(name: string, field: string): string;
//# sourceMappingURL=ast.d.ts.map
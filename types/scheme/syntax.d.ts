import * as L from '../lpm';
/**
 * A syntax value wraps a value that serves as a Scheme AST. It provides
 * metadata information, e.g., source ranges, for the underlying AST.
 */
export interface Syntax extends L.Struct {
    [L.scamperTag]: 'struct';
    [L.structKind]: 'syntax';
    value: L.Value;
    range: L.Range;
}
export declare const mkSyntax: (value: L.Value, range?: L.Range) => Syntax;
export declare const isSyntax: (v: L.Value) => v is Syntax;
/** @return v but with its top-level syntax wrapper removed, if it exists. */
export declare const stripSyntax: (v: L.Value) => L.Value;
/** @return v but with all syntax wrappers removed, recursively. */
export declare function stripAllSyntax(v: L.Value): L.Value;
/**
 * @return a pair of a value and its associated metadata if it is a syntax object
 *         or a fresh metadata map if it is a raw value.
 */
export declare const unpackSyntax: (v: L.Value) => {
    value: L.Value;
    range: L.Range;
};
//# sourceMappingURL=syntax.d.ts.map
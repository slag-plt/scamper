import * as L from '../lpm';
export declare const Runtime: L.Library;
/**
 * @returns a predicate function for struct types t.
 */
export declare function mkPredFn(t: string): (v: L.Value) => boolean;
/**
 * @returns a constructor function for struct type t with the given field names.
 */
export declare function mkCtorFn(t: string, fieldNames: string[]): (...args: L.Value[]) => L.Struct;
/**
 * @return field accessor function for struct type t and field name f.
 */
export declare function mkGetFn(t: string, f: string): (v: L.Value) => L.Value;
//# sourceMappingURL=runtime.d.ts.map
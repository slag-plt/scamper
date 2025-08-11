import { Library } from '../lang.js';
import * as R from '../lpm/runtime.js';
export declare const Runtime: Library;
/**
 * @returns a predicate function for struct types t.
 */
export declare function mkPredFn(t: string): (v: R.Value) => boolean;
/**
 * @returns a constructor function for struct type t with the given field names.
 */
export declare function mkCtorFn(t: string, fieldNames: string[]): (...args: R.Value[]) => R.Struct;
/**
 * @return field accessor function for struct type t and field name f.
 */
export declare function mkGetFn(t: string, f: string): (v: R.Value) => R.Value;
//# sourceMappingURL=runtime.d.ts.map
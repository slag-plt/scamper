import { Library, Value } from '../lang.js';
export declare const Prelude: Library;
export interface ReactiveFile extends Value.Struct {
    [Value.structKind]: 'reactive-file';
    callback: Value.ScamperFn;
}
export default Prelude;
//# sourceMappingURL=prelude.d.ts.map
import { Library, Value } from '../../lang.js';
/***** Image loading **********************************************************/
export interface ReactiveImageFile extends Value.Struct {
    [Value.structKind]: 'reactive-image-file';
    callback: Value.ScamperFn;
}
/***** Exports ****************************************************************/
export declare const lib: Library;
//# sourceMappingURL=image.d.ts.map
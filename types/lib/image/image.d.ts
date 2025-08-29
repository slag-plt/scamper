import * as L from '../../lpm';
/***** Image loading **********************************************************/
export interface ReactiveImageFile extends L.Struct {
    [L.structKind]: 'reactive-image-file';
    callback: L.ScamperFn;
}
/***** Exports ****************************************************************/
export declare const lib: L.Library;
//# sourceMappingURL=image.d.ts.map